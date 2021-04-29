package featherweightgo.typer

import featherweightgo.model.ast.AbstractStructureType.IntegerType
import featherweightgo.model.ast.AbstractStructureType.StringType
import featherweightgo.model.ast.AbstractStructureType.StructureType
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

  it should "be well-typed valid type assertion" in new SetUp {
    val string =
      """package main;
        |type A interface {
        |  Method() V
        |}
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func (this T) Method() V {
        |  return this.field
        |}
        |func main() {
        |  _ = T{V{}}.(A)
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(InterfaceType(InterfaceTypeName("A"),List())))
    }
  }

  it should "NOT be well-typed type assertion which is not confirmed given interface" in new SetUp {
    val string =
      """package main;
        |type A interface {
        |  Method() V
        |}
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.(A)
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast).isLeft)
    }
  }

  it should "NOT be well-typed if the function body type is not equal of its defined return type" in new SetUp {
    val string =
      """package main;
        |type A struct { }
        |type V struct {
        |  a A
        |}
        |type T struct {
        |  b A
        |}
        |type S struct { }
        |func (this S) F() V {
        |  return T{A{}}
        |}
        |func main() {
        |  _ = S{}.F()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast).isLeft)
    }
  }

  it should "be well-typed valid type assertion which has type parameter" in new SetUp {
    val string =
      """package main;
        |type any interface { }
        |type List[A any] interface { }
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |type V struct { }
        |func main() {
        |  _ = Cons[V]{V{}, Nil[V]{}}.(Cons[V])
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(
        sut.check(ast) == Right(
          StructureType(
            StructureTypeName("Cons"),
            List(AnyNamedType(AnyTypeName("V"), List()))
          )
        )
      )
    }
  }

  it should "NOT be well-typed invalid generic type assertion" in new SetUp {
    val string =
      """package main;
        |type any interface { }
        |type List[A any] interface { }
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = Cons[V]{V{}, Nil[V]{}}.(Cons[T])
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast).isLeft)
    }
  }

  it should "be well-typed valid generic type assertion" in new SetUp {
    val string =
      """package main;
        |type any interface { }
        |type List[A any] interface { }
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |type V struct { }
        |type A interface {
        |  Method() V
        |}
        |type T struct {
        |  field V
        |}
        |func (this T) Method() V {
        |  return this.field
        |}
        |func main() {
        |  _ = Cons[T]{T{V{}}, Nil[T]{}}.(Cons[T])
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(
        sut.check(ast) ==
          Right(
            StructureType(
              StructureTypeName("Cons"),
              List(AnyNamedType(AnyTypeName("T"), List()))
            )
          )
      )
    }
  }

  it should "be well-typed type assertion for the type parameter" in new SetUp {
    val string =
      """package main;
        |type any interface { }
        |type List[A any] interface { }
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |func (this Cons[A any]) Head() A {
        |  return this.head.(A)
        |}
        |type V struct { }
        |func main() {
        |  _ = Cons[V]{V{}, Nil[V]{}}.Head()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(
        sut.check(ast).isRight
      )
    }
  }

  it should "be well-typed a function which returns type that has type parameter" in new SetUp {
    val string =
      """
        |package main;
        |type any interface { }
        |type List[A any] interface {
        |    Concat(a List[A]) List[A]
        |}
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |    head A
        |    tail List[A]
        |}
        |func (this Nil[A any]) Concat(a List[A]) List[A] {
        |    return a
        |}
        |func (this Cons[A any]) Concat(a List[A]) List[A] {
        |    return Cons[A]{this.head, this.tail.Concat(a)}
        |}
        |type V struct {}
        |func main() {
        |  _ = Cons[V]{V{}, Nil[V]{}}.Concat(Nil[V]{})
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(
        sut.check(ast).isRight
      )
    }
  }

  it should "be well-typed a function which has ad-hoc polymorphism" in new SetUp {
    val string =
      """
        |package main;
        |type any interface { }
        |
        |type N struct {
        |    f any
        |}
        |
        |type List[A any] interface {
        |    Concat(a List[A]) List[A]
        |}
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |    head A
        |    tail List[A]
        |}
        |func (this Nil[A any]) Concat(a List[A]) List[A] {
        |    return a
        |}
        |func (this Cons[A any]) Concat(a List[A]) List[A] {
        |    return Cons[A]{this.head, this.tail.Concat(a)}
        |}
        |
        |type Combiner[A any] struct {
        |    left A
        |    right A
        |}
        |func (this Combiner[A List[B]]) Combine[B any]() A {
        |    return this.left.Concat(this.right)
        |}
        |
        |type V struct {}
        |func main() {
        |  _ = Combiner[List[V]]{Cons[V]{V{}, Nil[V]{}}, Nil[V]{}}.Combine[V]()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(
        AnyNamedType(
        typeName = AnyTypeName(value = "List"),
        types = List(AnyNamedType(typeName = AnyTypeName(value = "V"), types = List()))
      )))
    }
  }

  it should "be well-typed a same name functions which has ad-hoc polymorphism" in new SetUp {
    val string =
      """
        |package main;
        |type any interface { }
        |
        |type List[A any] interface { }
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |    head A
        |    tail List[A]
        |}
        |
        |type Expr interface {
        |   EvalList[A any]() List[A]
        |}
        |
        |type V struct {}
        |func main() {
        |  _ = V{}
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast).isRight)
    }
  }

  it should "be well-typed for primitive string value" in new SetUp {
    val string =
      """
        |package main;
        |func main() {
        |  _ = "hoge"
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(StringType))
    }
  }

  it should "be well-typed for primitive integer value" in new SetUp {
    val string =
      """
        |package main;
        |func main() {
        |  _ = 123
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(IntegerType))
    }
  }

  it should "be well-typed for function returns a primitive value" in new SetUp {
    val string =
      """
        |package main;
        |type V struct { }
        |func (this V) F() int {
        |  return 1
        |}
        |func (this V) G(a int) string {
        |  return "hoge"
        |}
        |
        |func main() {
        |  _ = V{}.G(V{}.F())
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(StringType))
    }
  }

  it should "be well-typed to integer plus" in new SetUp {
    val string =
      """
        |package main;
        |type V struct { }
        |func (this V) F() int {
        |  return 1
        |}
        |func (this V) G() int {
        |  return 2
        |}
        |
        |func main() {
        |  _ = V{}.G() + V{}.F()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(IntegerType))
    }
  }

  it should "be well-typed to string concat" in new SetUp {
    val string =
      """
        |package main;
        |type V struct { }
        |func (this V) F() string {
        |  return "hoge"
        |}
        |func (this V) G() string {
        |  return "fuga"
        |}
        |
        |func main() {
        |  _ = V{}.F() ++ V{}.G()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(StringType))
    }
  }

  it should "be well-typed a function call which returns its receiver" in new SetUp {
    val string =
      """
        |package main;
        |type V struct {}
        |func (this V) F() V {
        |  return this
        |}
        |
        |func main() {
        |  _ = V{}.F()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(AnyNamedType(AnyTypeName("V"), List())))
    }
  }

  it should "be well-typed a function call which returns its primitive receiver" in new SetUp {
    val string =
      """
        |package main;
        |func (this string) F() string {
        |  return this
        |}
        |
        |func main() {
        |  _ = "hoge".F()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(StringType))
    }
  }

  it should "be well-typed a function call which returns its primitive receiver" in new SetUp {
    val string =
      """
        |package main;
        |type any interface { }
        |type P[A any] struct {
        |    value A
        |}
        |func (this P[A string]) Write() string {
        |    return this.value
        |}
        |func main() {
        |    _ = P[string]{"hoge"}.Write()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)
    parseResult.foreach { ast =>
      assert(sut.check(ast) == Right(StringType))
    }
  }
}