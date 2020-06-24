package featherweightgo.parser.fg

import featherweightgo.model.fg.ast._
import featherweightgo.model.fg.error.FGError
import featherweightgo.model.fg.error.FGError.FGParseError
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader


class ParserFGImpl extends ParserFG {
  private val parserImpl = new ParserFGImpl.ParserImpl

  def parse(
    string: String
  ): Either[FGError.FGParseError, Main] = {
    import parserImpl._

    parserImpl.parse(mainMethod, string) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(FGParseError(msg))
    }
  }
}

object ParserFGImpl {
  private[parser] class ParserImpl extends RegexParsers { self =>
    override def skipWhitespace = false

    val name: Parser[String] = """\w+""".r

    val variableName: Parser[VariableName] =
      name.map(VariableName.apply)

    val methodName: Parser[MethodName] =
      name.map(MethodName.apply)

    val structureTypeName: Parser[StructureTypeName] =
      name.map(StructureTypeName.apply)

    val interfaceTypeName: Parser[InterfaceTypeName] =
      name.map(InterfaceTypeName.apply)

    val typeName: Parser[TypeName] =
      name.map(AnyTypeName.apply)

    val fieldName: Parser[FieldName] =
      name.map(FieldName.apply)

    def receiver: Parser[(VariableName, StructureTypeName)] =
      (("(" ~> variableName <~ whiteSpace) ~ (structureTypeName <~ ")")).map {
        case vn ~ stn =>
          (vn, stn)
      }

    private def commaSeparatedSequence[A](parser: Parser[A]): Parser[Seq[A]] =
      ((parser <~ whiteSpace.? <~ "," <~ whiteSpace.?).* ~ parser.?).map {
        case pSeq ~ pOpt =>
          pSeq ++ pOpt
      }

    def methodSignature: Parser[MethodSignature] =
      (("(" ~> commaSeparatedSequence((variableName <~ whiteSpace) ~ typeName)
        <~ whiteSpace.? <~ ")" <~ whiteSpace.?) ~ typeName).map {
        case args ~ rt =>
          MethodSignature(
            arguments = args
              .map { case vn ~ tn => (vn, tn)}
              .toMap,
            returnType = rt
          )
      }

    def methodSpecification: Parser[MethodSpecification] =
      (methodName ~ methodSignature).map {
        case mn ~ ms =>
          MethodSpecification(
            methodName = mn,
            methodSignature = ms
          )
      }

    def structure: Parser[Structure] =
      ("struct" ~> whiteSpace.? ~> "{" ~> whiteSpace.? ~>
        ((fieldName <~ whiteSpace) ~ typeName <~ whiteSpace.?).*
        <~ whiteSpace.? <~ "}").map { fs =>
        Structure(
          fields = fs.map {
            case fn ~ tn => StructureField(fn, tn)
          }
        )
      }

    def interface: Parser[Interface] =
      ("interface" ~> whiteSpace.? ~> "{" ~> whiteSpace.? ~>
        (methodSpecification <~ whiteSpace.?).* <~ whiteSpace.? <~ "}").map { mss =>
        Interface(
          methods = mss
        )
      }

    def typeLiteral: Parser[~[TypeName, TypeLiteral]] =
      ((structureTypeName <~ whiteSpace) ~ structure) |
        ((interfaceTypeName <~ whiteSpace) ~ interface)

    def methodDefinition: Parser[Method] =
      ((("func" ~> whiteSpace.? ~> receiver <~ whiteSpace.?) ~ methodSpecification <~ whiteSpace.? <~
        "{" <~ whiteSpace.? <~ "return" <~ whiteSpace) ~ expression <~ whiteSpace.? <~ "}").map {
        case r ~ ms ~ e =>
          featherweightgo.model.fg.ast.Method(
            receiver = r,
            methodSpecification = ms,
            body = e
          )
      }

    def typeDefinition: Parser[Type] =
      ("type" ~> whiteSpace ~> typeLiteral).map {
        case tn ~ tl =>
          Type(
            name = tn,
            typeLiteral = tl
          )
      }

    def declaration: Parser[Declaration] =
      methodDefinition | typeDefinition

    def mainMethod: Parser[Main] =
      ((whiteSpace.? ~> """package *main;""".r ~> whiteSpace.? ~> (declaration <~ whiteSpace).* <~
        """func +main\(\) *\{""".r <~ whiteSpace.? <~ """_ *= *""".r) ~
        (expression <~ whiteSpace.? <~ "}")).map {
        case ds ~ e =>
          featherweightgo.model.fg.ast.Main(
            declarations = ds,
            main = e
          )
      }

    def variable: Parser[Variable] =
      name.map { vn =>
        Variable(VariableName(vn))
      }

    private def empty[A](end: String): Parser[Seq[A]] =
      ("".r <~ end).map(_ => Nil)

    private def expressions(end: String): Parser[Seq[Expression]] =
      empty(end) | (commaSeparatedSequence(expression) <~ end)

    def structureLiteral: Parser[StructureLiteral] =
      ((structureTypeName <~ "{" <~ whiteSpace.?) ~
        expressions("}") <~ whiteSpace.?).map {
        case stn ~ es =>
          StructureLiteral(
            structureTypeName = stn,
            arguments = es
          )
      }

    def methodCall(expression: Expression): Parser[MethodCall] =
      ((whiteSpace.? ~> methodName <~ "(") ~ expressions(")")).map {
        case mn ~ es =>
          MethodCall(
            expression = expression,
            methodName = mn,
            arguments = es
          )
      }

    def fieldSelect(expression: Expression): Parser[FieldSelect] =
      (whiteSpace.? ~> fieldName).map { fn =>
        FieldSelect(
          expression = expression,
          fieldName = fn
        )
      }

    def typeAssertion(expression: Expression): Parser[TypeAssertion] =
      (whiteSpace.? ~> "(" ~> whiteSpace.? ~>
        typeName <~ whiteSpace.? <~ ")").map { tn =>
        TypeAssertion(
          expression = expression,
          typeName = tn
        )
      }

    def expression: Parser[Expression] = Parser { in =>
      ((""".+(?=\.)""".r <~ ".") ~ """.+""".r)(in) match {
        case Success(l ~ r, next) =>
          expression(new CharSequenceReader(l)).flatMapWithNext { e => i =>
            (typeAssertion(e) | methodCall(e) | fieldSelect(e))(
              new CharSequenceReader(r)
            ).flatMapWithNext { result => j =>
              if (i.atEnd && j.atEnd)
                Success(result, next)
              else
                Failure("parse error!", in)
            }
          }
        case _: NoSuccess =>
          (structureLiteral | variable)(in)
      }
    }
  }
}
