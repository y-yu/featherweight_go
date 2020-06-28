package featherweightgo.typer.fg

import featherweightgo.model.fg.ast._
import featherweightgo.model.fg.error.FGError.FGTypeError

trait TyperFG {
  /**
    * Type checker for Featherweight Go.
    *
    * @param main `Main` program
    * @return Right(TypeName) result type of the main program
    *         Left(FGTypeError) fail!
    */
  def check(
    main: Main
  ): Either[FGTypeError, TypeName]
}
