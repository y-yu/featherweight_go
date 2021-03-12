package featherweightgo.model.fg.error

import featherweightgo.model.fg.ast._
import featherweightgo.model.fg.typer.Environment

sealed abstract class FGError(
  message: String = null,
  cause: Throwable = null
) extends Throwable(message, cause)

object FGError {
  case class FGParseError(
    message: String,
    cause: Throwable = null
  ) extends FGError(
    s"Parse error! [$message]",
    cause
  )

  case class FGEvalError(
    ast: AST,
    cause: Throwable = null
  ) extends FGError(
    s"Given AST[$ast] cannot evaluate!",
    cause
  )

  case class FGTypeError(
    message: String,
    cause: Throwable = null
  ) extends FGError(message, cause)

  object FGTypeError {
    def apply(
      foundType: TypeName,
      expectedType: TypeName
    ): FGTypeError = FGTypeError(
      s"Found type is [${foundType.value}] but expected type is [${expectedType.value}]!"
    )

    def apply(
      variableName: VariableName,
      environment: Environment
    ): FGTypeError = FGTypeError(
      s"The variable[${variableName.value}]'s type is not found in the type environment[$environment]!"
    )

    def apply(
      interfaceName: TypeName,
      methodName: MethodName
    ): FGTypeError = FGTypeError(
      s"The interface[${interfaceName.value}] doesn't have the method[${methodName.value}]!"
    )

    def apply(
      structureName: TypeName,
      fieldName: FieldName
    ): FGTypeError = FGTypeError(
      s"The structure[${structureName.value}] doesn't have the field[${fieldName.value}]!"
    )

    def apply(
      foundTypes: List[TypeName],
      expectedTypes: List[TypeName]
    ): FGTypeError = FGTypeError(
      s"Found types are [${foundTypes.map(_.value).mkString(", ")}] but expected type is [${expectedTypes.map(_.value).mkString(", ")}]!"
    )
  }
}