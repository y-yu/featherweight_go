package featherweightgo.evaluator

import featherweightgo.model.ast._
import featherweightgo.model.ast.AbstractStructureType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import featherweightgo.parser.ParserImpl


class EvaluatorImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp {
    // This test depends on `ParserFGImpl`(the other logic)
    // so this is not a *unit test*!
    // But it would take too time to mock parser or write the AST directory...
    // That's the why this test depends on the parser implementation.
    val parser = new ParserImpl

    val sut = new EvaluatorImpl()
  }

  "EvaluatorFG" should "eval field selection of structure literal" in new SetUp {
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
      val actual = sut.eval(ast)

      assert(actual.isRight)
      assert(actual == Right(ValuedStructureLiteral(StructureType(StructureTypeName("V"), Nil), Nil)))
    }
  }

  it should "NOT eval field selection which doesn't exist" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.nonExistField
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)

      assert(actual.isLeft)
    }
  }

  it should "eval to call method" in new SetUp {
    val string =
      """package main;
        |type T struct { }
        |type V struct { }
        |func (this V) f(a T) T {
        |  return a
        |}
        |func main() {
        |  _ = V{}.f(T{})
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)

      assert(actual == Right(ValuedStructureLiteral(StructureType(StructureTypeName("T"), Nil), Nil)))
    }
  }


  it should "eval to call method and get the receiver field" in new SetUp {
    val string =
      """package main;
        |type T struct { }
        |type V struct {
        |  field T
        |}
        |func (this V) f() T {
        |  return this.field
        |}
        |func main() {
        |  _ = V{T{}}.f()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)

      assert(actual == Right(ValuedStructureLiteral(StructureType(StructureTypeName("T"), Nil), Nil)))
    }
  }

  it should "eval interface and struct" in new SetUp {
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
      val actual = sut.eval(ast)

      assert(actual == Right(ValuedStructureLiteral(StructureType(StructureTypeName("V"), Nil), Nil)))
    }
  }

  it should "eval expression in the StructureLiteral" in new SetUp {
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
      val actual = sut.eval(ast)

      assert(actual == Right(ValuedStructureLiteral(
        StructureType(StructureTypeName("Succ"), Nil),
        List(ValuedStructureLiteral(
          StructureType(StructureTypeName("Zero"), Nil),Nil))))
      )
    }
  }

  it should "eval expression with generics" in new SetUp {
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
        |type T struct { }
        |func main() {
        |  _ = Cons[V]{V{}, Cons[V]{V{}, Nil[V]{}}}.Length()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)
      assert(actual == Right(
        ValuedStructureLiteral(
          structureTypeName = StructureType(
            structureTypeName = StructureTypeName(value = "Succ"),
            types = List()
          ),
          values = List(
            ValuedStructureLiteral(
              structureTypeName = StructureType(
                structureTypeName = StructureTypeName(value = "Succ"),
                types = List()
              ),
              values = List(
                ValuedStructureLiteral(
                  structureTypeName = StructureType(
                    structureTypeName = StructureTypeName(value = "Zero"),
                    types = List()
                  ),
                  values = List()
                )
              )
            )
          )
        )
      ))
    }
  }

  it should "eval a function which takes generic types" in new SetUp {
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
        |  _ = Combiner[List[V]]{Cons[V]{V{}, Nil[V]{}}, Cons[V]{V{}, Nil[V]{}}}.Combine[V]()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)
      assert(actual == Right(
        value = ValuedStructureLiteral(
          structureTypeName = StructureType(
            structureTypeName = StructureTypeName(value = "Cons"),
            types = List(AnyNamedType(typeName = AnyTypeName(value = "V"), types = List()))
          ),
          values = List(
            ValuedStructureLiteral(
              structureTypeName = StructureType(
                structureTypeName = StructureTypeName(value = "V"),
                types = List()
              ),
              values = List()
            ),
            ValuedStructureLiteral(
              structureTypeName = StructureType(
                structureTypeName = StructureTypeName(value = "Cons"),
                types = List(AnyNamedType(typeName = AnyTypeName(value = "V"), types = List()))
              ),
              values = List(
                ValuedStructureLiteral(
                  structureTypeName = StructureType(
                    structureTypeName = StructureTypeName(value = "V"),
                    types = List()
                  ),
                  values = List()
                ),
                ValuedStructureLiteral(
                  structureTypeName = StructureType(
                    structureTypeName = StructureTypeName(value = "Nil"),
                    types = List(AnyNamedType(typeName = AnyTypeName(value = "V"), types = List()))
                  ),
                  values = List()
                )
              )
            )
          )
        )
      ))
    }
  }

  it should "eval a function which returns a primitive string" in new SetUp {
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
      val actual = sut.eval(ast)
      assert(actual == Right(StringValue("hoge")))
    }
  }

  it should "eval a primitive integer plus" in new SetUp {
    val string =
      """
        |package main;
        |type V struct { }
        |func (this V) F() int {
        |  return 123
        |}
        |func (this V) G() int {
        |  return 456
        |}
        |
        |func main() {
        |  _ = V{}.F() + V{}.G()
        |}
        |""".stripMargin

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.eval(ast)
      assert(actual == Right(IntegerValue(579)))
    }
  }

  it should "eval a primitive string concat" in new SetUp {
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
      val actual = sut.eval(ast)
      assert(actual == Right(StringValue("hogefuga")))
    }
  }
}