package featherweightgo.parser

import featherweightgo.model.ast.Main
import featherweightgo.model.error.FGError.FGParseError

trait Parser {
  /**
    * Parse from `String` source code of Featherweight Go.
    *
    * @param string source code
    * @return Right(Main) result of parsing
    *         Left(FGParseError) fail!
    */
  def parse(
    string: String
  ): Either[FGParseError, Main]

}
