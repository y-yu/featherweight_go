package featherweightgo.parser.fg

import featherweightgo.model.fg.ast.Main
import featherweightgo.model.fg.error.FGError.FGParseError

trait ParserFG {
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
