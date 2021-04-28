package featherweightgo.model.typer

import featherweightgo.model.ast.AnyNamedType
import featherweightgo.model.ast.Declaration
import featherweightgo.model.ast.InterfaceType
import featherweightgo.model.ast.StructureType
import featherweightgo.model.ast.Type
import featherweightgo.model.ast.TypeParameter
import featherweightgo.util.Utils.lookupAnyType
import featherweightgo.util.Utils.methodsInSet
import scala.util.Failure
import scala.util.Success
import scala.util.Try

case class Implement(
  lhs: Type,
  rhs: Type
)

object Implement {
  implicit class ImplementSyntax(val lhs: Type) {
    def <:<(rhs: Type): Implement =
      Implement(lhs, rhs)
  }

  private case class CheckingError(
    lhs: Type,
    rhs: Type,
    message: String = null,
    cause: Throwable = null
  ) extends Throwable(
    s"""
       |lhs: $lhs
       |rhs: $rhs
       |""".stripMargin,
    cause
  )

  private val success: Try[Unit] = Success()
  private def failure(lhs: Type, rhs: Type): Try[Unit] = Failure(CheckingError(lhs, rhs))

  implicit def checkingInstance(implicit
    declarations: List[Declaration]
  ): Checking[Implement] = {
    (value: Implement, typeBound: TypeBound) =>
      def loop(lhs: Type, rhs: Type): Try[Unit] = (lhs, rhs) match {
        case (anyNameType: AnyNamedType, rhs) =>
          lookupAnyType(anyNameType) match {
            case Some(typ) =>
              loop(typ, rhs)

            case None if anyNameType.types.isEmpty =>
              loop(TypeParameter(anyNameType.name.value), rhs)

            case _ =>
              failure(lhs, rhs)
          }

        case (lhs, anyNameType: AnyNamedType) =>
          lookupAnyType(anyNameType) match {
            case Some(typ) =>
              loop(lhs, typ)

            case None if anyNameType.types.isEmpty =>
              loop(lhs, TypeParameter(anyNameType.name.value))

            case _ =>
              failure(lhs, rhs)
          }

        case (TypeParameter(n1), TypeParameter(n2)) =>
          if (n1 == n2)
            success
          else
            failure(lhs, rhs)

        case (lhs: StructureType, rhs: StructureType) =>
          if(lhs.structureTypeName == rhs.structureTypeName && checkTypesRecursive(lhs.types, rhs.types))
            success
          else
            failure(lhs, rhs)

        case (lhs: StructureType, rhs: InterfaceType) =>
          if (methodsInSet(lhs, rhs, typeBound))
            success
          else
            failure(lhs, rhs)

        case (lhs: InterfaceType, rhs: InterfaceType) =>
          if (methodsInSet(lhs, rhs, typeBound))
            success
          else
            failure(lhs, rhs)

        case (lhs: TypeParameter, rhs: InterfaceType) =>
          if (methodsInSet(lhs, rhs, typeBound))
            success
          else
            failure(lhs, rhs)

        case (lhs: InterfaceType, rhs: TypeParameter) =>
          if (methodsInSet(lhs, rhs, typeBound))
            success
          else
            failure(lhs, rhs)

        case (lhs, rhs) =>
          println("matching failed!")
          pprint.pprintln(lhs)
          pprint.pprintln(rhs)
          failure(lhs, rhs)
      }

      def checkTypesRecursive(
        ltypse: List[Type], rtypes: List[Type]
      ): Boolean =
        ltypse.length == rtypes.length &&
          (ltypse zip rtypes).forall {
            case (l, r) =>
              getAndPrintError(loop(l, r))
          }

      getAndPrintError(loop(value.lhs, value.rhs))
  }

  private def getAndPrintError(t: Try[Unit]): Boolean =
    t.recoverWith {
      case e: Throwable =>
        e.printStackTrace()
        Failure(e)
    }.isSuccess
}