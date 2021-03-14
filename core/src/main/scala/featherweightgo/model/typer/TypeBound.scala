package featherweightgo.model.typer

import featherweightgo.model.ast.InterfaceType
import featherweightgo.model.ast.TypeFormal
import featherweightgo.model.ast.TypeParameter

/**
  * Type parameter bounds the interface type.
  *
  * @note It's sometimes represented by Δ(delta) in some paper.
  */
case class TypeBound(
  value: Map[TypeParameter, InterfaceType]
) {
  def get(typeParameter: TypeParameter): Option[InterfaceType] =
    value.get(typeParameter)

  def |-[A](value: A)(implicit
    checking: Checking[A]
  ): Boolean = checking.isOk(value, this)

  def |-[A](values: List[A])(implicit
    checking: Checking[A]
  ): Boolean = values.forall(
    value => checking.isOk(value, this)
  )
}

object TypeBound {
  val empty: TypeBound = TypeBound(Map.empty)

  def fromTypeFormals(
    typeFormals: List[TypeFormal]
  ): TypeBound =
    TypeBound(
      typeFormals.map { typeFormal =>
        typeFormal.typeParameter -> typeFormal.interfaceType
      }.toMap
    )
}