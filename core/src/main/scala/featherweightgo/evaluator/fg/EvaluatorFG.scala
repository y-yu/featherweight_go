package featherweightgo.evaluator.fg

import featherweightgo.model.fg.ast._
import featherweightgo.model.fg.ast.ValuedStructureLiteral
import featherweightgo.model.fg.error.FGError.FGEvalError

trait EvaluatorFG {
  /**
    * Eval `Main` program of Featherweight Go.
    *
    * @param main `Main` program represented as `AST`
    * @return Right(ValuedStructureLiteral) success evaluation
    *         Left(FGEvalError) fail!
    */
  def eval(
    main: Main
  ): Either[FGEvalError, ValuedStructureLiteral]
}
