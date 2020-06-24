package featherweightgo.util

import featherweightgo.model.fg.ast._
import scala.annotation.tailrec

object Utils {
  case class TypedVariable(
    variable: Variable,
    typeName: TypeName
  )

  def fields(
    declarations: Seq[Declaration],
    structureTypeName: StructureTypeName
  ): Seq[StructureField] = {
    @tailrec
    def loop(
      ds: Seq[Declaration]
    ): Seq[StructureField] = ds match {
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

  @tailrec
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

  def tdecls(
    declarations: Seq[Declaration]
  ): Seq[TypeName] =
    declarations.collect {
      case Type(name, _) =>
        name
    }

  def mdecls(
    declarations: Seq[Declaration]
  ): Seq[(StructureTypeName, MethodName)] =
    declarations.collect {
      case Method(receiver, methodSpecification, _) =>
        (receiver._2, methodSpecification.methodName)
    }

  def methods(
    declarations: Seq[Declaration],
    typeName: TypeName
  ): Seq[MethodSpecification] = {
    def ifStructureTypeName(
      structureTypeName: StructureTypeName
    ): Seq[MethodSpecification] =
      declarations.collect {
        case Method(receiver, methodSpecification, _)
          if receiver._2 == structureTypeName =>
          methodSpecification
      }

    def ifInterfaceTypeName(
      interfaceTypeName: InterfaceTypeName
    ): Seq[MethodSpecification] =
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
    seq: Seq[A]
  ): Boolean =
    seq.distinct.length == seq.length

  def sequence[A, E](seq : Seq[Either[E, A]]): Either[E, Seq[A]] =
    seq.foldLeft(Right(Seq.empty[A]): Either[E, Seq[A]]) { (x, y) =>
      for {
        a <- x
        b <- y
      } yield a :+ b
    }

  implicit class WellFormedImplements(val leftHand: TypeName) {
    def :<(
      rightHand: TypeName
    )(
      implicit declarations: Seq[Declaration]
    ): Boolean =
      leftHand == rightHand ||
        methods(declarations, rightHand).toSet.subsetOf(
          methods(declarations, leftHand).toSet
        )
  }
}