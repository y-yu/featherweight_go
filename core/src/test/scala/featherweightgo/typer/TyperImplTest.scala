package featherweightgo.typer

import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec
import featherweightgo.parser.ParserImpl
import featherweightgo.model.ast._

class TyperImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp {
    // This test depends on `ParserFGImpl`(the other logic)
    // so this is not a *unit test*!
    // But it would take too time to mock parser or write the AST directory...
    // That's the why this test depends on the parser implementation.
    val parser = new ParserImpl
    
    val sut = new TyperImpl
  }

  "TyperFG" should "be well-typed the Main" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(AnyNamedType(AnyTypeName("V"), Nil)))
    }
  }

  it should "NOT be well-typed Main if the code refer a non-exist field" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field2
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach(ast => assert(sut.check(ast).isLeft))
  }

  it should "be well-typed Main if the code has interface" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  value V
        |}
        |type M interface {
        |  Method() V
        |}
        |func (this T) Method() V {
        |  return this.value
        |}
        |type S struct {
        |  field M
        |}
        |func main() {
        |  _ = S{T{V{}}}.field.Method()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual == Right(AnyNamedType(AnyTypeName("V"), Nil)))
    }
  }

  it should "be ill-typed if the arguments of structure literal is wrong" in new SetUp {
    val string =
      """package main;
        |
        |type T struct {
        |  field T
        |}
        |
        |func main() {
        |  _ = T{}.field
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isLeft)
    }
  }

  it should "be ill-typed even if the arguments of structure literal is subtype of expected" in new SetUp {
    val string =
      """package main;
        |type A struct { }
        |type T struct {
        |  field1 A
        |  field2 A
        |}
        |type U struct {
        |  field1 A
        |}
        |type V struct {
        |  field U
        |}
        |func main() {
        |  _ = V{T{A{}, A{}}}.field
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isLeft)
    }
  }

  it should "be ill-typed even if the arguments of method is subtype of expected" in new SetUp {
    val string =
      """package main;
        |type A struct { }
        |type T struct {
        |  field1 A
        |  field2 A
        |}
        |type U struct {
        |  field1 A
        |}
        |type V struct { }
        |func (this V) method(u U) A {
        |  return u.field1
        |}
        |func main() {
        |  _ = V{}.method(T{A{}, A{}})
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isLeft)
    }
  }

  it should "be ill-typed to call method even if the structure is subtype of the implementation of the interface" in new SetUp {
    val string =
      """package main;
        |type A struct { }
        |type T struct {
        |  field1 A
        |  field2 A
        |}
        |type U struct {
        |  field1 A
        |}
        |type I interface {
        |  Method() A
        |}
        |func (this U) Method() A {
        |  return this.field1
        |}
        |func main() {
        |  _ = T{ A{}, A{} }.Method()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isLeft)
    }
  }

  it should "be ill-typed if the structure has a recursive type field" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field T
        |}
        |func main() {
        |  _ = T{V{}}
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isLeft)
    }
  }

  it should "be well-typed if the expression in the StructureLiteral" in new SetUp {
    val string =
      """package main;
        |type Number interface { }
        |type Zero struct { }
        |type Succ struct {
        |  pred Number
        |}
        |func main() {
        |  _ = Succ{Succ{Zero{}}.pred}
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual.isRight)
    }
  }

  it should "be well-typed generic function" in new SetUp {
    """
      |func (this Nil[A any]) Length() Number {
      |  return Zero{}
      |}
      |func (this Cons[A any]) Length() Number {
      |  return Succ{this.tail.Length()}
      |}
      |
      |""".stripMargin

    val string =
      """package main;
        |type Number interface { }
        |type Zero struct { }
        |type Succ struct {
        |  pred Number
        |}
        |type any interface { }
        |type List[A any] interface {
        |  Length() Number
        |}
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |func (this Nil[A any]) Length() Number {
        |  return Zero{}
        |}
        |func (this Cons[A any]) Length() Number {
        |  return Succ{this.tail.Length()}
        |}
        |type V struct { }
        |type S struct { }
        |func main() {
        |  _ = Cons[V]{V{}, Cons[V]{V{}, Nil[V]{}}}.Length()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)
      assert(actual == Right(AnyNamedType(typeName = AnyTypeName(value = "Number"), types = List())))
    }
  }
}