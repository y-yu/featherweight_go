package featherweightgo.evaluator

import featherweightgo.model.ast.Main
import featherweightgo.model.ast.ValuedStructureLiteral
import featherweightgo.model.error.FGError.FGEvalError

trait Evaluator {
  /**
   * Eval `Main` program of Featherweight Go.
   *
   * @param main `Main` program represented as `AST`
   * @return Right(ValuedStructureLiteral) success evaluation
   * Left(FGEvalError) fail!
   */
  def eval(
    main: Main
  ): Either[FGEvalError, ValuedStructureLiteral]
}