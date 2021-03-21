package featherweightgo.model.typer

import featherweightgo.model.ast.AnyNamedType
import featherweightgo.model.ast.Declaration
import featherweightgo.model.ast.InterfaceType
import featherweightgo.model.ast.StructureType
import featherweightgo.model.ast.Type
import featherweightgo.model.ast.TypeParameter
import featherweightgo.util.Utils.lookupAnyType
import featherweightgo.util.Utils.methods

case class Implement(
  lhs: Type,
  rhs: Type
)

object Implement {
  implicit class ImplementSyntax(val lhs: Type) {
    def <:<(rhs: Type): Implement =
      Implement(lhs, rhs)
  }


  implicit def checkingInstance(implicit
    declarations: List[Declaration]
  ): Checking[Implement] = {
    (value: Implement, typeBound: TypeBound) =>
      def loop(lhs: Type, rhs: Type): Boolean = (lhs, rhs) match {
        case (anyNameType: AnyNamedType, rhs) =>
          lookupAnyType(anyNameType) match {
            case Some(typ) =>
              loop(typ, rhs)

            case None if anyNameType.types.isEmpty =>
              loop(TypeParameter(anyNameType.name.value), rhs)

            case _ =>
              false
          }

        case (lhs, anyNameType: AnyNamedType) =>
          lookupAnyType(anyNameType) match {
            case Some(typ) =>
              loop(lhs, typ)

            case None if anyNameType.types.isEmpty =>
              loop(lhs, TypeParameter(anyNameType.name.value))

            case _ =>
              false
          }

        case (TypeParameter(n1), TypeParameter(n2)) =>
          n1 == n2

        case (lhs: StructureType, rhs: StructureType) =>
          lhs.structureTypeName == rhs.structureTypeName &&
            checkTypesRecursive(lhs.types, rhs.types)

        case (lhs: StructureType, rhs: InterfaceType) =>
          methods(rhs, typeBound).toSet.subsetOf(
            methods(lhs, typeBound).toSet
          ) &&
            checkTypesRecursive(lhs.types, rhs.types)

        case (lhs: InterfaceType, rhs: InterfaceType) =>
          methods(rhs, typeBound).toSet.subsetOf(
            methods(lhs, typeBound).toSet
          ) &&
            checkTypesRecursive(lhs.types, rhs.types)

        case (lhs: TypeParameter, rhs: InterfaceType) =>
          methods(rhs, typeBound).toSet.subsetOf(
            methods(lhs, typeBound).toSet
          )

        case _ =>
          false
      }

      def checkTypesRecursive(
        ltypse: List[Type], rtypes: List[Type]
      ): Boolean =
        ltypse.length == rtypes.length &&
          (ltypse zip rtypes).forall {
            case (l, r) =>
              loop(l, r)
          }

      loop(value.lhs, value.rhs)
  }
}