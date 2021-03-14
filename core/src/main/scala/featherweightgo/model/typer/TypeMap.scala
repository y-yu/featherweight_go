package featherweightgo.model.typer

import featherweightgo.model.ast.Type
import featherweightgo.model.ast.TypeParameter

/**
  * Type parameter to Type mapping.
  *
  * @note It's sometimes represented by η(eta) in some paper.
  */
case class TypeMap(
  value: Map[TypeParameter, Type]
) {
  def get(typeParameter: TypeParameter): Option[Type] =
    value.get(typeParameter)

  def ++(rhs: TypeMap): TypeMap =
    TypeMap(this.value ++ rhs.value)
}
