package parser.fg

import ast.fg._
import scala.util.parsing.combinator._

class ParserFG extends RegexParsers {
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
    structureTypeName | interfaceTypeName

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
            case fn ~ tn => (fn, tn)
          }.toMap
        )
    }

  def interface: Parser[Interface] =
    ("interface" ~> whiteSpace.? ~> "{" ~> whiteSpace.? ~>
      (methodSpecification <~ whiteSpace.?).* <~ whiteSpace.? <~ "}").map { mss =>
        Interface(
          methods = mss
        )
    }

  def typeLiteral: Parser[TypeLiteral] =
    structure | interface

  def methodDefinition: Parser[Method] =
    ((("func" ~> whiteSpace.? ~> receiver <~ whiteSpace.?) ~ methodSpecification <~ whiteSpace.? <~
      "{" <~ whiteSpace.? <~ "return" <~ whiteSpace) ~ expression <~ whiteSpace.? <~ "}").map {
      case r ~ ms ~ e =>
        ast.fg.Method(
          receiver = r,
          methodSpecification = ms,
          body = e
        )
    }

  def typeDefinition: Parser[Type] =
    (("type" ~> whiteSpace ~> typeName <~ whiteSpace) ~ typeLiteral).map {
      case tn ~ tl =>
        Type(
          name = tn,
          typeLiteral = tl
        )
    }

  def declaration: Parser[Declaration] =
    methodDefinition | typeDefinition

  def mainMethod: Parser[Main] =
    (("""package *main;""".r ~> whiteSpace.? ~> (declaration <~ whiteSpace).* <~ """func *main\(\) *\{""".r <~ whiteSpace.? <~ """_ *= *""".r) ~
      (expression <~ whiteSpace.? <~ "}")).map {
      case ds ~ e =>
        ast.fg.Main(
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

  def methodCall: Parser[MethodCall] =
    ((inversePriorityExpression <~ whiteSpace.? <~ "." <~ whiteSpace.? ) ~
      (methodName <~ "(") ~ expressions(")")).map {
      case e ~ mn ~ es =>
        MethodCall(
          expression = e,
          methodName = mn,
          arguments = es
        )
    }

  def structureLiteral: Parser[StructureLiteral] =
    ((structureTypeName <~ "{" <~ whiteSpace.?) ~
      expressions("}") <~ whiteSpace.?).map {
      case stn ~ es =>
        StructureLiteral(
          structureTypeName = stn,
          arguments = es
        )
    }

  def fieldSelect: Parser[FieldSelect] =
    ((inversePriorityExpression <~ whiteSpace.? <~ "." <~ whiteSpace.?) ~ fieldName).map {
      case e ~ fn =>
        FieldSelect(
          expression = e,
          fieldName = fn
        )
    }

  def typeAssertion: Parser[TypeAssertion] =
    ((inversePriorityExpression <~ whiteSpace.? <~ ".(" <~ whiteSpace.?) ~
      (typeName <~ whiteSpace.? <~ ")")).map {
      case e ~ tn =>
        TypeAssertion(
          expression = e,
          typeName = tn
        )
    }
  def nonDotExpression: Parser[Expression] =
    structureLiteral | variable

  def dotExpression: Parser[Expression] =
    typeAssertion | methodCall | fieldSelect

  private def inversePriorityExpression: Parser[Expression] =
    nonDotExpression | dotExpression

  def expression: Parser[Expression] =
    dotExpression | nonDotExpression
}
