package featherweightgo.typer

import featherweightgo.model.ast.AbstractStructureType.IntegerType
import featherweightgo.model.ast.AbstractStructureType.StringType
import featherweightgo.model.ast.AbstractStructureType.StructureType
import featherweightgo.model.ast._
import featherweightgo.model.error.FGError.FGTypeError
import featherweightgo.model.typer.Environment
import featherweightgo.model.typer.Implement._
import featherweightgo.model.typer.TypeBound
import featherweightgo.util.Utils._

class TyperImpl extends Typer {
  def check(
    main: Main
  ): Either[FGTypeError, Type] = {
    implicit val declarations: List[Declaration] = main.declarations

    def formalsImplementCheck(
      fs1: List[TypeFormal],
      fs2: List[TypeFormal]
    ): Boolean =
      fs1.length == fs2.length &&
        (TypeBound.empty |- (fs1 zip fs2).map {
          case (f1, f2) =>
            f1.interfaceType <:< f2.interfaceType
        })

    def typeParameterCheck(
      typeBound: TypeBound,
      typeParameter: TypeParameter
    ): Boolean =
      typeBound.value.contains(typeParameter)

    def namedTypeCheck(
      typeBound: TypeBound,
      namedType: AnyNamedType
    ): Boolean = {
      typeActualCheck(typeBound, namedType.types) &&
        (declarations.forall {
          case TypeDeclaration(name, typeFormals, _) if name.value == namedType.typeName.value =>
            try {
              typeSubstitute(typeFormals, namedType.types, typeBound)
              true
            } catch {
              case e: FGTypeError =>
                e.printStackTrace()
                false
            }
          case _ =>
            true
        } || (
          namedType.types.isEmpty &&
            typeParameterCheck(typeBound, TypeParameter(namedType.name.value))
        ))
    }

    def typeActualCheck(
      typeBound: TypeBound,
      types: List[Type]
    ): Boolean = {
      types.forall {
        case typeParameter: TypeParameter =>
          typeParameterCheck(typeBound, typeParameter)

        case anyNamedType : AnyNamedType =>
          namedTypeCheck(typeBound, anyNamedType)

        case IntegerType | StringType =>
          true

        case StructureType(structureTypeName, ts) =>
          declarations.exists {
            case TypeDeclaration(name, _, _) =>
              name == structureTypeName
            case _ =>
              false
          } &&
          typeActualCheck(typeBound, ts)

        case InterfaceType(interfaceTypeName, ts) =>
          declarations.exists {
            case TypeDeclaration(name, _, _) =>
              name == interfaceTypeName
            case _ =>
              false
          } &&
          typeActualCheck(typeBound, ts)
      }
    }

    def formalsCheck(
      fs1: List[TypeFormal],
      fs2: List[TypeFormal]
    ): Boolean = {
      val typeBound = TypeBound.fromTypeFormals(fs1 ++ fs2)

      distinct(
        fs1.map(_.typeParameter) ++ fs2.map(_.typeParameter)
      ) &&
        (typeBound |- typeActualCheck(typeBound, fs2.map(_.interfaceType)))
    }

    def methodSpecificationCheck(
      typeFormals: List[TypeFormal],
      methodSpecification: MethodSpecification,
    ): Boolean = {
      val methodSignature = methodSpecification.methodSignature
      val typeBound = TypeBound.fromTypeFormals(typeFormals ++ methodSignature.typeFormals)

      formalsCheck(
        typeFormals,
        methodSignature.typeFormals
      ) &&
        distinct(methodSignature.arguments.keys.toList) &&
        typeActualCheck(
          typeBound,
          methodSignature.returnType :: methodSignature.arguments.values.toList
        )
    }

    def typeLiteralCheck(
      typeFormals: List[TypeFormal],
      typeLiteral: TypeLiteral
    ): Boolean = {
      val typeBound = TypeBound.fromTypeFormals(typeFormals)

      typeLiteral match {
        case Structure(fields) =>
          distinct(fields.map(_.name)) &&
            (typeBound |- typeActualCheck(typeBound, fields.map(_.typ)))
        case Interface(methods) =>
          unique(methods) &&
            (typeBound |- methods.forall(method => methodSpecificationCheck(typeFormals, method)))
      }
    }

    def typeDeclarationCheck(
      typeDeclaration: TypeDeclaration
    ): Boolean =
      formalsCheck(Nil, typeDeclaration.typeFormals) &&
      (TypeBound.fromTypeFormals(typeDeclaration.typeFormals) |-
        typeLiteralCheck(typeDeclaration.typeFormals, typeDeclaration.typeLiteral))

    def declarationCheck: Boolean = {
      declarations.forall {
        case typeDeclaration: TypeDeclaration =>
          typeDeclarationCheck(typeDeclaration)

        case MethodDeclaration(receiver, methodSpecification, body) =>
          val methodSignature = methodSpecification.methodSignature
          val arguments = methodSignature.arguments
          val typeBound = TypeBound.fromTypeFormals(
            receiver.typeFormals ++ methodSignature.typeFormals
          )
          val receiverDeclaration = declarations.collectFirst {
            case td@TypeDeclaration(name, typeFormals, _) if
            receiver.structureTypeName == name &&
              formalsImplementCheck(
                receiver.typeFormals,
                typeFormals
              ) => td
          }

          distinct(receiver.variableName :: arguments.keys.toList) &&
            receiverDeclaration.isDefined &&
            formalsCheck(
              receiver.typeFormals,
              methodSignature.typeFormals
            ) &&
            typeActualCheck(typeBound, methodSignature.returnType :: arguments.values.toList) &&
            (expressionCheck(
              environment = Environment(
                  Map(
                    receiver.variableName -> StructureType(
                      receiver.structureTypeName,
                      receiverDeclaration.get.typeFormals.map(_.typeParameter)
                    )
                  ) ++ arguments
              ),
              typeBound = typeBound,
              expression = body
            ) match {
              case Right(typ) =>
                typeBound |- typ <:< methodSignature.returnType

              case Left(e) =>
                throw e
            })
      }
    }

    def expressionCheck(
      environment: Environment,
      typeBound: TypeBound,
      expression: Expression
    ): Either[FGTypeError, Type] = expression match {
      case ValuedStructureLiteral(structureTypeName, _) =>
        Right(structureTypeName)

      case IntegerValue(_) =>
        Right(IntegerType)

      case StringValue(_) =>
        Right(StringType)

      case Plus(lhs, rhs) =>
        for {
          lhsType <- expressionCheck(
            environment,
            typeBound,
            lhs
          )
          rhsType <- expressionCheck(
            environment,
            typeBound,
            rhs
          )
          _ <- (lhsType, rhsType) match {
            case (IntegerType, IntegerType) =>
              Right(())
            case _ =>
              Left(
                FGTypeError(
                  s"""
                     |Primitive integer + takes integer type arguments!
                     | lhs type: $lhsType
                     | rhs type: $rhsType
                     |""".stripMargin)
              )
          }
        } yield IntegerType


      case Concat(lhs, rhs) =>
        for {
          lhsType <- expressionCheck(
            environment,
            typeBound,
            lhs
          )
          rhsType <- expressionCheck(
            environment,
            typeBound,
            rhs
          )
          _ <- (lhsType, rhsType) match {
            case (StringType, StringType) =>
              Right(())
            case _ =>
              Left(
                FGTypeError(
                  s"""
                     |Primitive string ++ takes string type arguments!
                     | lhs type: $lhsType
                     | rhs type: $rhsType
                     |""".stripMargin)
              )
          }
        } yield StringType

      case Variable(name) =>
        toEither(
          environment.get(name),
          FGTypeError(name, environment)
        )

      case MethodCall(e, methodName, types, arguments) =>
        for {
          eType <- expressionCheck(
            environment,
            typeBound,
            e
          )
          argumentTypes <- sequence(
            arguments.map(arg =>
              expressionCheck(
                environment,
                typeBound,
                arg
              )
            )
          )
          methodSignature <- toEither(
            methods(eType, typeBound)
              .find(_.methodName == methodName)
              .map(_.methodSignature),
            FGTypeError(eType.name, methodName)
          )
          updatedTypeMap = typeSubstitute(
            methodSignature.typeFormals,
            types,
            typeBound
          )
          methodSignatureArgumentTypes = methodSignature.arguments.values.toList
          _ <-
            if (
              argumentTypes.length == methodSignatureArgumentTypes.length &&
                (typeBound |-
                  (argumentTypes zip methodSignatureArgumentTypes).map {
                    case (argumentType, methodSignatureArgumentType) =>
                      argumentType <:< methodSignatureArgumentType
                  }
                )
            )
              Right(())
            else {
              Left(
                FGTypeError(
                  argumentTypes.map(_.name),
                  methodSignature.arguments.values.toList.map(_.name)
                )
              )
            }
        } yield
          typeReplace(
            updatedTypeMap,
            methodSignature.returnType
          )

      case StructureLiteral(structureType, arguments) =>
        for {
          _ <-
            if(
              typeActualCheck(
                typeBound,
                List(structureType)
              )
            ) Right(())
            else Left(FGTypeError(s"${structureType.name} error!"))
          argumentTypes <- sequence(
            arguments.map(arg =>
              expressionCheck(
                environment,
                typeBound,
                arg
              )
            )
          )
          fs = fields(declarations, structureType)
          _ <-
            if (
              argumentTypes.length == fs.length &&
              (typeBound |-
                (argumentTypes zip fs.map(_.typ)).map {
                case (argumentType, fieldType) =>
                  argumentType <:< fieldType
                }
              )
            ) {
              Right(())
            }
            else {
              Left(
                FGTypeError(argumentTypes.map(_.name), fs.map(_.typ.name))
              )
            }
        } yield structureType

      case FieldSelect(e, fieldName) =>
        for {
          eType <- expressionCheck(environment, typeBound, e)
          eStructureType <- eType match {
            case st: AbstractStructureType =>
              Right(st)
            case anyNamedType: AnyNamedType =>
              lookupAnyType(anyNamedType) match {
                case Some(typ: AbstractStructureType) =>
                  Right(typ)
                case _ =>
                  Left(FGTypeError(s"The give type[${eType.name.value}] is not structure!"))
              }
            case _ =>
              Left(FGTypeError(s"The give type[${eType.name.value}] is not structure!"))
          }
          field <- toEither(
            fields(declarations, eStructureType)
              .find(_.name == fieldName),
            FGTypeError(eStructureType.name, fieldName)
          )
        } yield field.typ

      case TypeAssertion(e, anyNamedType: AnyNamedType) =>
        lookupAnyType(anyNamedType) match {
          case Some(typ) =>
            expressionCheck(
              environment,
              typeBound,
              TypeAssertion(e, typ)
            )
          case None =>
            if (anyNamedType.types.isEmpty)
              expressionCheck(
                environment,
                typeBound,
                TypeAssertion(e, TypeParameter(anyNamedType.name.value))
              )
            else
              Left(FGTypeError(s"assert target ${anyNamedType.name} is invalid!"))
        }

      case TypeAssertion(e, typ) =>
        for {
          _ <-
            if(
              typeActualCheck(
                typeBound,
                List(typ)
              )
            ) Right(())
            else Left(FGTypeError(s"assert ${typ.name} error!"))
          eType <- expressionCheck(environment, typeBound, e)
          _ <- typ match {
            case st: AbstractStructureType =>
              if (
                typeBound |- (
                  st <:< bounds(typeBound, eType).fold(identity, identity)
                )
              ) Right(())
              else Left(FGTypeError(s"assert ${st.name}(StructureType) error!"))

            case it: InterfaceType =>
              if (
                typeBound |- (
                  eType <:< it
                )
              ) Right(())
              else Left(FGTypeError(s"assert ${it.name}(InterfaceType) error!"))

            case tp: TypeParameter =>
              if (
                typeBound |- (
                  eType <:< tp
                  )
              ) Right(())
              else Left(FGTypeError(s"assert ${tp.name}(TypeParameter) error!"))

            case _: AnyNamedType =>
              Left(FGTypeError(s"assert is ${typ.name} any named type"))
          }
        } yield typ
    }

    val i = distinct(tdecls(declarations)) &&
      distinct(mdecls(declarations)) &&
      declarationCheck

    expressionCheck(
      environment = Environment(Map.empty),
      TypeBound.empty,
      expression = main.main
    ).flatMap { result =>
      if (i)
        Right(result)
      else {
        Left(FGTypeError("The declarations of the main program seems to be wrong!"))
      }
    }
  }

  private def toEither[A, E](
    opt: Option[A],
    error: E
  ): Either[E, A] = opt match {
    case Some(a) => Right(a)
    case None => Left(error)
  }
}