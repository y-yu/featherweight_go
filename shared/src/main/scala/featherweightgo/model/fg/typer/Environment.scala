package featherweightgo.model.fg.typer

import featherweightgo.model.fg.ast._

/**
  * Type environment: a map of variable and type.
  *
  * @note It's sometimes represented by Γ(gamma) in some paper.
  */
case class Environment(
  value: Map[VariableName, TypeName]
) {
  def get(variableName: VariableName): Option[TypeName] =
    value.get(variableName)
}