package featherweightgo.evaluator

import featherweightgo.model.ast._
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
}