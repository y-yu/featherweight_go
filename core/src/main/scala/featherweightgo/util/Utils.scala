package featherweightgo.util

import featherweightgo.model.ast._
import featherweightgo.model.error.FGError.FGTypeError
import featherweightgo.model.typer.Implement._
import featherweightgo.model.typer.TypeBound
import featherweightgo.model.typer.TypeMap
import scala.annotation.tailrec

object Utils {
  case class TypedVariable(
    variable: Variable,
    typ: Type
  )

  case class TypeSubstitution(
    typeFormal: TypeFormal,
    typeMap: TypeMap
  )

  def typeSubstitute(
    typeFormals: List[TypeFormal],
    types: List[Type]
  ): TypeMap = {
    if (typeFormals.length != types.length) {
      throw new RuntimeException(
        s"""
           |typeFormals: $typeFormals
           |types: $types
           |""".stripMargin
      )
    }

    TypeMap(
      (typeFormals zip types).map {
        case (typeFormal, typ) =>
          typeFormal.typeParameter -> typ
      }.toMap
    )
  }

  def typeSubstitute(
    typeFormals: List[TypeFormal],
    types: List[Type],
    typeBound: TypeBound
  )(implicit
    declarations: List[Declaration]
  ): TypeMap = {
    val typeMap = typeSubstitute(typeFormals, types)

    if (
      typeBound |- typeFormals.map { typeFormal =>
        typeReplace(typeMap, typeFormal.typeParameter) <:< typeFormal.interfaceType
      }
    ) typeMap
    else throw FGTypeError("Type bound error!")
  }

  def typeReplace(
    typeMap: TypeMap,
    typ: Type
  ): Type = typ match {
    case typeParameter: TypeParameter =>
      typeMap.get(typeParameter) match {
        case Some(replacedType) =>
          replacedType
        case None =>
          typ
      }
    case anyNamedType: AnyNamedType =>
      if (anyNamedType.types.isEmpty)
        typeMap.get(TypeParameter(anyNamedType.name.value)) match {
          case Some(replacedType) =>
            replacedType
          case None =>
            typ
        }
      else
        anyNamedType.copy(
          types = anyNamedType.types.map { typ =>
            typeReplace(typeMap, typ)
          }
        )

    case st @ StructureType(_, types) =>
      st.copy(
        types = types.map(t => typeReplace(typeMap, t))
      )

    case it @ InterfaceType(_, types) =>
      it.copy(
        types = types.map(t => typeReplace(typeMap, t))
      )
  }

  def fields(
    declarations: List[Declaration],
    structureType: StructureType
  ): List[StructureField] = {
    @tailrec
    def loop(
      ds: List[Declaration]
    ): List[StructureField] = ds match {
      case TypeDeclaration(name, typeFormals, Structure(fields)) :: ts =>
        if (name.value == structureType.structureTypeName.value) {
          val typeMap = typeSubstitute(typeFormals, structureType.types)

          fields.map { field =>
            field.copy(
              typ = typeReplace(typeMap, field.typ)
            )
          }
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
  ): StructureType = valuedStructureLiteral.structureTypeName

  def expressionTypeReplace(
    typeMap: TypeMap,
    expression: Expression
  ): Expression = expression match {
    case MethodCall(exp, methodName, types, arguments) =>
      MethodCall(
        expressionTypeReplace(typeMap, exp),
        methodName,
        types.map(t => typeReplace(typeMap, t)),
        arguments.map(a => expressionTypeReplace(typeMap, a))
      )

    case StructureLiteral(st, arguments) =>
      StructureLiteral(
        st.copy(
          types = st.types.map(t => typeReplace(typeMap, t))
        ),
        arguments.map(a => expressionTypeReplace(typeMap, a))
      )

    case FieldSelect(exp, fieldName) =>
      FieldSelect(
        expressionTypeReplace(typeMap, exp),
        fieldName
      )

    case TypeAssertion(exp, typ) =>
      TypeAssertion(
        expressionTypeReplace(typeMap, exp),
        typeReplace(typeMap, typ)
      )

    case _ =>
      expression
  }

  def body(
    declarations: List[Declaration],
    structureType: StructureType,
    methodName: MethodName,
    methodTypes: List[Type]
  ): Option[(List[TypedVariable], Expression)] = {
    @tailrec
    def loop(
      ds: List[Declaration]
    ): Option[(List[TypedVariable], Expression)] = ds match {
      case MethodDeclaration(receiver, methodSpecification, body) :: ts =>
        if (
          receiver.structureTypeName == structureType.structureTypeName &&
            methodSpecification.methodName == methodName
        ) {
          val typeMap =
            typeSubstitute(receiver.typeFormals, structureType.types) ++
              typeSubstitute(methodSpecification.methodSignature.typeFormals, methodTypes)

          Some(
            (
              TypedVariable(
                Variable(receiver.variableName),
                StructureType(
                  receiver.structureTypeName,
                  structureType.types
                )
              ) +:
                methodSpecification.methodSignature.arguments.map {
                  case (vn, tn) =>
                    TypedVariable(
                      Variable(vn),
                      typeReplace(typeMap, tn)
                    )
                }.toList,
              expressionTypeReplace(typeMap, body)
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
      case TypeDeclaration(name, _, _) =>
        name
    }

  def mdecls(
    declarations: List[Declaration]
  ): List[(StructureTypeName, MethodName)] =
    declarations.collect {
      case MethodDeclaration(receiver, methodSpecification, _) =>
        (receiver.structureTypeName, methodSpecification.methodName)
    }

  def bounds(
    typeBound: TypeBound,
    typ: Type
  ): Either[InterfaceType, StructureType] = typ match {
    case typeParameter: TypeParameter =>
      Left(typeBound.get(typeParameter).get)

    case interfaceType: InterfaceType =>
      Left(interfaceType)

    case structureType: StructureType =>
      Right(structureType)

    case anyNamedType: AnyNamedType =>
      throw FGTypeError(s"`bounds` cannot accept AnyNamedType(${anyNamedType.name})!")
  }

  def lookupAnyType(
    anyNamedType: AnyNamedType
  )(implicit
    declarations: List[Declaration]
  ): Option[Type] = {
    declarations.collectFirst {
      case TypeDeclaration(name, _, _) if name.value == anyNamedType.typeName.value =>
        name
    }.flatMap {
      case stn: StructureTypeName =>
        Some(StructureType(stn, anyNamedType.types))

      case itn: InterfaceTypeName =>
        Some(InterfaceType(itn, anyNamedType.types))

      case _ =>
        None
    }
  }

  def methodsInSet(
    lhs: Type,
    rhs: Type,
    typeBound: TypeBound
  )(implicit
    declarations: List[Declaration]
  ): Boolean =
    methods(rhs, typeBound, ifUpdate = false).toSet.subsetOf(methods(lhs, typeBound, ifUpdate = false).toSet)

  def methods(
    typ: Type,
    typeBound: TypeBound,
    ifUpdate: Boolean = true
  )(implicit
    declarations: List[Declaration]
  ): List[MethodSpecification] = {
    def updateMethodsType(
      typeFormals: List[TypeFormal],
      types: List[Type],
      methodSpecification: MethodSpecification
    ): MethodSpecification =
      if (ifUpdate) {
        val typeMap = typeSubstitute(typeFormals, types, typeBound)

        methodSpecification.copy(
          methodSignature = methodSpecification.methodSignature.copy(
            arguments = methodSpecification.methodSignature.arguments.map {
              case variableName -> typ =>
                variableName -> typeReplace(typeMap, typ)
            },
            returnType = typeReplace(typeMap, methodSpecification.methodSignature.returnType)
          )
        )
      } else {
        methodSpecification
      }

    def ifStructureType(
      structureType: StructureType
    ): List[MethodSpecification] =
      declarations.collect {
        case MethodDeclaration(receiver, methodSpecification, _)
          if receiver.structureTypeName == structureType.structureTypeName =>
          updateMethodsType(
            receiver.typeFormals,
            structureType.types,
            methodSpecification
          )
      }

    def ifInterfaceType(
      interfaceType: InterfaceType
    ): List[MethodSpecification] =
      declarations.collect {
        case TypeDeclaration(typeName, typeFormals, Interface(methods))
          if typeName == interfaceType.interfaceTypeName =>
          methods.map { method =>
            updateMethodsType(
              typeFormals,
              interfaceType.types,
              method
            )
          }
      }
      .flatten

    typ match {
      case anyNamedType: AnyNamedType =>
        lookupAnyType(anyNamedType).map { typ =>
          methods(
            typ,
            typeBound
          )
        }.getOrElse(
          if (anyNamedType.types.isEmpty)
            methods(
              TypeParameter(anyNamedType.name.value),
              typeBound
            )
          else
            throw FGTypeError("type lookup in methods error!")
        )
      case stn: StructureType =>
        ifStructureType(stn)
      case itn: InterfaceType =>
        ifInterfaceType(itn)
      case typeParameter: TypeParameter =>
        typeBound
          .get(typeParameter)
          .map(ifInterfaceType)
          .getOrElse(throw FGTypeError(
            s""" method error!
               | typeBound: $typeBound
               | typeParameter: $typeParameter
               |""".stripMargin))
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
}