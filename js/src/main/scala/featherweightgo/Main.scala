package featherweightgo

import featherweightgo.evaluator.EvaluatorImpl
import featherweightgo.model.ast.ValuedStructureLiteral
import featherweightgo.parser.ParserImpl
import featherweightgo.typer.TyperImpl
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("FeatherweightGoMain")
object Main {
  val evaluatorFG = new EvaluatorImpl()
  val typerFG = new TyperImpl()
  val parserFG = new ParserImpl()

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
  ): String =
    (for {
      ast <- parserFG.parse(source)
      t <- typerFG.check(ast)
    } yield t) match {
      case Right(tn) => s"${tn.name.value}"
      case Left(t) => t.getMessage
    }

  @JSExport
  def eval(
    source: String
  ): String =
    (for {
      ast <- parserFG.parse(source)
      r <- evaluatorFG.eval(ast)
    } yield r) match {
      case Right(v) => valuePrinter(v)
      case Left(t) => t.getMessage
    }

  private def valuePrinter(
    value: ValuedStructureLiteral
  ): String = {
    val arguments = value.values.map(valuePrinter).mkString("{", ", ", "}")

    s"${value.structureTypeName.name.value}$arguments"
  }
}
