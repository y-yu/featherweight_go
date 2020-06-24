package featherweightgo

import featherweightgo.evaluator.fg.EvaluatorFGImpl
import featherweightgo.parser.fg.ParserFGImpl
import featherweightgo.typer.fg.TyperFGImpl
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("FeatherweightGoMain")
object Main {
  val evaluatorFG = new EvaluatorFGImpl()
  val typerFG = new TyperFGImpl()
  val parserFG = new ParserFGImpl()

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
      case Right(tn) => s"$tn"
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
      case Right(v) => s"$v"
      case Left(t) => t.getMessage
    }
}
