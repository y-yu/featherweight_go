package featherweightgo.model.typer

import featherweightgo.model.ast._

/**
  * Type environment: a map of variable and type.
  *
  * @note It's sometimes represented by Γ(gamma) in some paper.
  */
case class Environment(
  value: Map[VariableName, Type]
) {
  def get(variableName: VariableName): Option[Type] =
    value.get(variableName)

  def :+(e: (VariableName, Type)): Environment =
    Environment(value + e)
}