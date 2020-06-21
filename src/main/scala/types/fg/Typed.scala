package types.fg

import ast.fg._

sealed trait Typed extends Product with Serializable

case class TypedVariable(
  variable: Variable,
  typeName: TypeName
) extends Typed