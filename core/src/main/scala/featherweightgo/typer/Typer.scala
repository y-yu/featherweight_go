package featherweightgo.typer

import featherweightgo.model.ast._
import featherweightgo.model.error.FGError.FGTypeError

trait Typer {
  /**
    * Type checker for Featherweight Go.
    *
    * @param main `Main` program
    * @return Right(Type) result type of the main program
    *         Left(FGTypeError) fail!
    */
  def check(
    main: Main
  ): Either[FGTypeError, Type]
}
