package featherweightgo

import featherweightgo.evaluator.EvaluatorImpl
import featherweightgo.model.ast.IntegerValue
import featherweightgo.model.ast.Primitive
import featherweightgo.model.ast.StringValue
import featherweightgo.model.ast.ValuedStructureLiteral
import featherweightgo.parser.ParserImpl
import featherweightgo.typer.TyperImpl
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("FeatherweightGoMain")
object Main {
  val evaluatorFG = new EvaluatorImpl()
  val typerFG = new TyperImpl()
  val parserFG = new ParserImpl()

  @JSExport
  val exampleSourceCode: String =
    """package main;
      |type any interface { }
      |type List[A any] interface {
      |    Length() int
      |}
      |type Nil[A any] struct { }
      |type Cons[A any] struct {
      |    head A
      |    tail List[A]
      |}
      |func (this Nil[A any]) Length() int {
      |    return 0
      |}
      |func (this Cons[A any]) Length() int {
      |    return this.tail.Length() + 1
      |}
      |func main() {
      |    _ = Cons[int]{1, Cons[int]{2, Nil[int]{}}}.Length()
      |}
      |""".stripMargin

  @JSExport
  def parse(
    source: String
  ): String =
    parserFG.parse(source) match {
      case Right(result) =>
        s"$result"
      case Left(failure) =>
        failure.getMessage
    }

  @JSExport
  def typeCheck(
    source: String
  ): js.Tuple2[Boolean, String] =
    try {
      (for {
        ast <- parserFG.parse(source)
        t <- typerFG.check(ast)
      } yield t) match {
        case Right(tn) =>
          (true, s"${tn.name.value}")
        case Left(t) =>
          (false, t.getMessage)
      }
    } catch {
      case e: Throwable =>
        (false, e.getMessage)
    }

  @JSExport
  def eval(
    source: String
  ): js.Tuple2[Boolean, String] =
    try {
      (for {
        ast <- parserFG.parse(source)
        r <- evaluatorFG.eval(ast)
      } yield r) match {
        case Right(v) => (true, valuePrinter(v))
        case Left(t) => (false, t.getMessage)
      }
    } catch {
      case e: Throwable =>
        (false, e.getMessage)
    }

  private def valuePrinter(
    primitive: Primitive
  ): String = primitive match {
    case value: ValuedStructureLiteral =>
      val arguments = value.values.map(valuePrinter).mkString("{", ", ", "}")

      s"${value.structureTypeName.name.value}$arguments"

    case StringValue(value) =>
      value

    case IntegerValue(value) =>
      value.toString
  }
}
