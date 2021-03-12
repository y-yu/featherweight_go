package featherweightgo.util

import featherweightgo.model.fg.ast._
import scala.annotation.tailrec

object Utils {
  case class TypedVariable(
    variable: Variable,
    typeName: TypeName
  )

  def fields(
    declarations: List[Declaration],
    structureTypeName: StructureTypeName
  ): List[StructureField] = {
    @tailrec
    def loop(
      ds: List[Declaration]
    ): List[StructureField] = ds match {
      case Type(name, Structure(fields)) :: ts =>
        if (name.value == structureTypeName.value) {
          fields
        } else {
          loop(ts)
        }
      case _ :: ds => loop(ds)
      case Nil => Nil
    }

    loop(declarations)
  }

  def `type`(
    valuedStructureLiteral: ValuedStructureLiteral
  ): StructureTypeName = valuedStructureLiteral.structureTypeName

  def body(
    declarations: List[Declaration],
    structureTypeName: StructureTypeName,
    methodName: MethodName
  ): Option[(List[TypedVariable], Expression)] = {
    @tailrec
    def loop(
      ds: List[Declaration]
    ): Option[(List[TypedVariable], Expression)] = ds match {
      case Method(receiver, methodSpecification, body) :: ts =>
        if (receiver._2 == structureTypeName && methodSpecification.methodName == methodName) {
          Some(
            (
              TypedVariable(
                Variable(receiver._1),
                receiver._2
              ) +:
                methodSpecification.methodSignature.arguments.map {
                  case (vn, tn) =>
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

  @tailrec
  def unique(
    methodSpecifications: List[MethodSpecification]
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

  def tdecls(
    declarations: List[Declaration]
  ): List[TypeName] =
    declarations.collect {
      case Type(name, _) =>
        name
    }

  def mdecls(
    declarations: List[Declaration]
  ): List[(StructureTypeName, MethodName)] =
    declarations.collect {
      case Method(receiver, methodSpecification, _) =>
        (receiver._2, methodSpecification.methodName)
    }

  def methods(
    declarations: List[Declaration],
    typeName: TypeName
  ): List[MethodSpecification] = {
    def ifStructureTypeName(
      structureTypeName: StructureTypeName
    ): List[MethodSpecification] =
      declarations.collect {
        case Method(receiver, methodSpecification, _)
          if receiver._2 == structureTypeName =>
          methodSpecification
      }

    def ifInterfaceTypeName(
      interfaceTypeName: InterfaceTypeName
    ): List[MethodSpecification] =
      declarations.collect {
        case Type(typeName, Interface(methods))
          if typeName == interfaceTypeName =>
          methods
      }
      .flatten

    typeName match {
      case stn @ StructureTypeName(_) =>
        ifStructureTypeName(stn)
      case itn @ InterfaceTypeName(_) =>
        ifInterfaceTypeName(itn)
      case AnyTypeName(value) =>
        ifStructureTypeName(StructureTypeName(value)) ++
          ifInterfaceTypeName(InterfaceTypeName(value))
    }
  }

  def distinct[A](
    seq: List[A]
  ): Boolean =
    seq.distinct.length == seq.length

  def sequence[A, E](seq : List[Either[E, A]]): Either[E, List[A]] =
    seq.foldLeft(Right(List.empty[A]): Either[E, List[A]]) { (x, y) =>
      for {
        a <- x
        b <- y
      } yield a :+ b
    }

  implicit class WellFormedImplements(val leftHand: TypeName) {
    def :<(
      rightHand: TypeName
    )(
      implicit declarations: List[Declaration]
    ): Boolean = {
      def strictType(t: TypeName): Option[StructureTypeName Either InterfaceTypeName] =
        declarations.collectFirst {
          case Type(stn @ StructureTypeName(name), Structure(_)) if name == t.value =>
            Left(stn)
          case Type(itn @ InterfaceTypeName(name), Interface(_)) if name == t.value =>
            Right(itn)
        }

      (strictType(leftHand), strictType(rightHand)) match {
        case (Some(Left(lv)), Some(Left(rv))) =>
          lv == rv
        case (Some(l), Some(Right(rv))) =>
          methods(declarations, l.fold(identity, identity)).toSet.subsetOf(
            methods(declarations, rv).toSet
          )
        case _ =>
          false
      }
    }

  }
}