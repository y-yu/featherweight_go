package featherweightgo.model.typer

trait Checking[A] {
  def isOk(a: A, typeBound: TypeBound): Boolean
}

object Checking {
  implicit val booleanInstance: Checking[Boolean] =
    (a: Boolean, _: TypeBound) => a
}