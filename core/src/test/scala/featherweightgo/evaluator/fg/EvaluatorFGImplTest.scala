package featherweightgo.evaluator.fg

import featherweightgo.model.fg.ast._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import featherweightgo.parser.fg.ParserFGImpl

class EvaluatorFGImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp {
    // This test depends on `ParserFGImpl`(the other logic)
    // so this is not a *unit test*!
    // But it would take too time to mock parser or write the AST directory...
    // That's the why this test depends on the parser implementation.
    val parser = new ParserFGImpl

    val sut = new EvaluatorFGImpl()
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
      assert(actual == Right(ValuedStructureLiteral(StructureTypeName("V"), Nil)))
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

      assert(actual == Right(ValuedStructureLiteral(StructureTypeName("T"), Nil)))
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

      assert(actual == Right(ValuedStructureLiteral(StructureTypeName("T"), Nil)))
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

      assert(actual == Right(ValuedStructureLiteral(StructureTypeName("V"), Nil)))
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

      assert(actual == Right(ValuedStructureLiteral(StructureTypeName("Succ"),List(ValuedStructureLiteral(StructureTypeName("Zero"),Nil)))))
    }
  }
}
