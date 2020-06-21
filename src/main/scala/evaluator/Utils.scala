package evaluator

import ast.fg._
import types.fg.TypedVariable
import scala.annotation.tailrec

object Utils {
  def fields(
    declarations: Seq[Declaration],
    structureTypeName: StructureTypeName
  ): Map[FieldName, TypeName] = {
    @tailrec
    def loop(
      ds: Seq[Declaration]
    ): Map[FieldName, TypeName] = ds match {
      case Type(name, Structure(fields)) :: ts =>
        if (name.value == structureTypeName.value) {
          fields
        } else {
          loop(ts)
        }
      case _ :: ds => loop(ds)
      case Nil => Map.empty
    }

    loop(declarations)
  }

  def getType(
    valuedStructureLiteral: ValuedStructureLiteral
  ): StructureTypeName = valuedStructureLiteral.structureTypeName

  def body(
    declarations: Seq[Declaration],
    structureTypeName: StructureTypeName,
    methodName: MethodName
  ): Option[(Seq[TypedVariable], Expression)] = {
    @tailrec
    def loop(
      ds: Seq[Declaration]
    ): Option[(Seq[TypedVariable], Expression)] = ds match {
      case Method(receiver, methodSpecification, body) :: ts =>
        if (receiver._2 == structureTypeName && methodSpecification.methodName == methodName) {
          Some(
            (
              TypedVariable(
                Variable(receiver._1),
                receiver._2
              ) +:
                methodSpecification.methodSignature.arguments.map {
                  case (vn -> tn) =>
                    TypedVariable(
                      Variable(vn),
                      tn
                    )
                }.toList,
              body
            )
          )
        } else {
          loop(ts)
        }
      case _ :: ds => loop(ds)
      case Nil => None
    }
    loop(declarations)
  }

  def unique(
    methodSpecifications: Seq[MethodSpecification]
  ): Boolean = methodSpecifications match {
    case h :: ts =>
      ts.forall { m =>
        if (m.methodName == h.methodName)
          m.methodSignature == h.methodSignature
        else
          true
      } && unique(ts)

    case Nil => true
  }
}
