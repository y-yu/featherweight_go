package featherweightgo.typer.fg

import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec
import featherweightgo.parser.fg.ParserFGImpl
import featherweightgo.model.fg.ast._

class TyperFGImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp {
    // This test depends on `ParserFGImpl`(the other logic)
    // so this is not a *unit test*!
    // But it would take too time to mock parser or write the AST directory...
    // That's the why this test depends on the parser implementation.
    val parser = new ParserFGImpl
    
    val sut = new TyperFGImpl
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

    parseResult.foreach(ast => assert(sut.check(ast).isRight))
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

      assert(actual == Right(AnyTypeName("V")))
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
}
