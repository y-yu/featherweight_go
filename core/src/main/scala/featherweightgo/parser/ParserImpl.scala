package featherweightgo.parser

import featherweightgo.model.ast._
import featherweightgo.model.error.FGError
import featherweightgo.model.error.FGError.FGParseError
import scala.util.parsing.combinator.RegexParsers


class ParserImpl extends Parser {
  private val parserImpl = new ParserImpl.ParserImpl

  def parse(
    string: String
  ): Either[FGError.FGParseError, Main] = {
    import parserImpl._

    parserImpl.parse(mainMethod, string) match {
      case Success(result, _) => Right(result)
      case e: NoSuccess => Left(FGParseError(e.msg))
    }
  }
}

object ParserImpl {
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

    private def flatten[A](asOpt: Option[List[A]]): List[A] =
      asOpt.fold(List.empty[A])(identity)

    def typ: Parser[Type] =
      namedType

    def typeParameter: Parser[TypeParameter] =
      name.map(TypeParameter)

    def typeParameters: Parser[List[Type]] =
      "[" ~> commaSeparatedSequence(typ) <~ "]"

    def namedType: Parser[AnyNamedType] = {
      (typeName ~ typeParameters.?).map {
        case tn ~ types =>
          AnyNamedType(tn, flatten(types))
      }
    }

    def structureType: Parser[StructureType] =
      (structureTypeName ~ typeParameters.?).map {
        case structureTypeName ~ types =>
          StructureType(structureTypeName, flatten(types))
      }

    def interfaceType: Parser[InterfaceType] =
      (interfaceTypeName ~ typeParameters.?).map {
        case interfaceTypeName ~ types =>
          InterfaceType(interfaceTypeName, flatten(types))
      }

    def receiver: Parser[MethodReceiver] =
      (("(" ~> variableName <~ whiteSpace) ~ structureTypeName ~ typeFormals.? <~ ")").map {
        case vn ~ stn ~ typeFormals =>
          MethodReceiver(vn, stn, flatten(typeFormals))
      }

    private def commaSeparatedSequence[A](parser: Parser[A]): Parser[List[A]] =
      ((parser <~ whiteSpace.? <~ "," <~ whiteSpace.?).* ~ parser.?).map {
        case pSeq ~ pOpt =>
          pSeq ++ pOpt
      }

    private def typeFormals: Parser[List[TypeFormal]] =
      ("[" ~> whiteSpace.* ~> whiteSpace.* ~>
        commaSeparatedSequence((typeParameter <~ whiteSpace.+) ~ interfaceType) <~ whiteSpace.* <~ "]"  ).map {
          _.foldLeft(List.empty[TypeFormal]) { (acc, x) =>
            acc :+ TypeFormal(x._1, x._2)
          }
      }

    def methodSignature: Parser[MethodSignature] =
      (typeFormals.? ~
        ("(" ~> commaSeparatedSequence((variableName <~ whiteSpace) ~ typ)
        <~ whiteSpace.? <~ ")" <~ whiteSpace.?) ~ typ).map {
        case typeFormals ~ args ~ rt =>
          MethodSignature(
            typeFormals = flatten(typeFormals),
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
        ((fieldName <~ whiteSpace) ~ typ <~ whiteSpace.?).*
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

    def typeLiteral: Parser[(TypeName, List[TypeFormal], TypeLiteral)] =
      (((structureTypeName ~ typeFormals.? <~ whiteSpace) ~ structure) |
        ((interfaceTypeName ~ typeFormals.? <~ whiteSpace) ~ interface)).map {
        case tn ~ typeFormals ~ typeLiteral =>
          (tn, flatten(typeFormals), typeLiteral)
      }

    def methodDefinition: Parser[MethodDeclaration] =
      ((("func" ~> whiteSpace.? ~> receiver <~ whiteSpace.?) ~ methodSpecification <~ whiteSpace.? <~
        "{" <~ whiteSpace.? <~ "return" <~ whiteSpace) ~ expression <~ whiteSpace.? <~ "}").map {
        case r ~ ms ~ e =>
          MethodDeclaration(
            receiver = r,
            methodSpecification = ms,
            body = e
          )
      }

    def typeDefinition: Parser[TypeDeclaration] =
      ("type" ~> whiteSpace ~> typeLiteral).map {
        case (tn, tfs, tl) =>
          TypeDeclaration(
            name = tn,
            typeFormals = tfs,
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
          Main(
            declarations = ds,
            main = e
          )
      }

    def variable: Parser[Variable] =
      name.map { vn =>
        Variable(VariableName(vn))
      }

    private def empty[A](end: String): Parser[List[A]] =
      ("".r <~ end).map(_ => Nil)

    private def expressions(end: String): Parser[List[Expression]] =
      empty(end) | (commaSeparatedSequence(expression) <~ end)

    def structureLiteral: Parser[StructureLiteral] =
      ((structureType <~ "{" <~ whiteSpace.?) ~
        expressions("}") <~ whiteSpace.?).map {
        case stn ~ es =>
          StructureLiteral(
            structureType = stn,
            arguments = es
          )
      }

    def methodCall(expression: Expression): Parser[MethodCall] =
      ((((whiteSpace.? ~> methodName <~ whiteSpace.*) ~ typeParameters.?) <~ "(") ~ expressions(")")).map {
        case mn ~ types ~ es =>
          MethodCall(
            expression = expression,
            methodName = mn,
            types = flatten(types),
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
        typ <~ whiteSpace.? <~ ")").map { tn =>
        TypeAssertion(
          expression = expression,
          typ = tn
        )
      }

    def dotSequence(
      e: Expression
    ): Parser[Expression] = Parser { in =>
      ("." ~> (typeAssertion(e) | methodCall(e) | fieldSelect(e)))(in) match {
        case Success(exp, next) =>
          dotSequence(exp)(next)

        case _: NoSuccess =>
          Success(e, in)
      }
    }

    def expression: Parser[Expression] = Parser { in =>
      structureLiteral(in) match {
        case Success(e, next) =>
          dotSequence(e)(next)

        case _: NoSuccess =>
          variable(in).flatMapWithNext( e => next =>
            dotSequence(e)(next)
          )
      }
    }
  }
}
