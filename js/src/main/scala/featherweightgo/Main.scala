package featherweightgo

import featherweightgo.evaluator.fg.EvaluatorFG
import featherweightgo.parser.fg.ParserFG
import featherweightgo.typer.fg.TyperFG
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("FeatherweightGoMain")
object Main {
  val evaluatorFG = new EvaluatorFG()
  val typerFG = new TyperFG()
  val parserFG = new ParserFG()
  import parserFG._

  @JSExport
  def parse(
    source: String
  ): String =
    parserFG.parse(mainMethod, source) match {
      case parserFG.Success(result, _) =>
        s"$result"
      case failure =>
        s"Parse error!: $failure"
    }

  @JSExport
  def typeCheck(
    source: String
  ): String =
    (for {
      ast <- parserFG.parse(parserFG.mainMethod, source) match {
        case Success(result, _) =>
          Right(result)
        case failure =>
          Left(new IllegalArgumentException(s"$failure"))
      }
      t <- typerFG.check(ast)
    } yield t) match {
      case Right(tn) => s"$tn"
      case Left(t) => s"Type error!: $t"
    }


  @JSExport
  def eval(
    source: String
  ): String =
    (for {
      ast <- parserFG.parse(parserFG.mainMethod, source) match {
        case Success(result, _) =>
          Right(result)
        case failure =>
          Left(new IllegalArgumentException(s"$failure"))
      }
      r <- evaluatorFG.eval(ast)
    } yield r) match {
      case Right(v) => s"$v"
      case Left(t) => s"Evaluator error!: $t"
    }
}
