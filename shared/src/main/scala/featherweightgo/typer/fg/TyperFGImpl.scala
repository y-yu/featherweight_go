package featherweightgo.typer.fg

import featherweightgo.model.fg.ast._
import featherweightgo.model.fg.error.FGError.FGTypeError
import featherweightgo.model.fg.typer.Environment
import featherweightgo.util.Utils._

class TyperFGImpl extends TyperFG {

  def check(
    main: Main
  ): Either[FGTypeError, TypeName] = {
    implicit val declarations: Seq[Declaration] = main.declarations

    def typeCheck(
      typeName: TypeName
    ): Boolean =
      declarations.exists {
        case Type(name, _) =>
          name.value == typeName.value
        case _ =>
          false
      }

    def declarationCheck: Boolean = {
      def methodSpecificationCheck(
        methodSpecification: MethodSpecification
      ): Boolean = {
        val arguments = methodSpecification.methodSignature.arguments
        val returnType = methodSpecification.methodSignature.returnType

        distinct(arguments.keys.toList) &&
          arguments.values.forall(typeCheck) &&
          typeCheck(returnType)
      }

      def typeLiteralCheck(
        typeLiteral: TypeLiteral
      ): Boolean = typeLiteral match {
        case Structure(fields) =>
          distinct(fields.map(_.name)) && fields.map(_.typeName).forall(typeCheck)
        case Interface(methods) =>
          unique(methods) && methods.forall(methodSpecificationCheck)
      }

      declarations.forall {
        case Type(_, typeLiteral) =>
          typeLiteralCheck(typeLiteral)

        case Method(receiver, methodSpecification, body) =>
          val methodSignature = methodSpecification.methodSignature
          val arguments = methodSignature.arguments

          typeCheck(receiver._2) &&
            arguments.values.forall(typeCheck) &&
            typeCheck(methodSignature.returnType) &&
            expressionCheck(
              environment = Environment(
                Map(receiver) ++ arguments
              ),
              expression = body
            )
            .exists(_ :< methodSignature.returnType)
      }
    }

    def expressionCheck(
      environment: Environment,
      expression: Expression
    ): Either[FGTypeError, TypeName] = expression match {
      case ValuedStructureLiteral(structureTypeName, _) =>
        Right(structureTypeName)

      case Variable(name) =>
        toEither(environment.get(name), FGTypeError(name, environment))

      case MethodCall(e, methodName, arguments) =>
        for {
          eType <- expressionCheck(environment, e)
          argumentTypes <- sequence(
            arguments.map(arg =>
              expressionCheck(environment, arg)
            )
          )
          methodSignature <- toEither(
            methods(declarations, eType)
              .find(_.methodName == methodName)
              .map(_.methodSignature),
            FGTypeError(eType, methodName)
          )
          _ <-
            if ((argumentTypes zip methodSignature.arguments.values).forall {
              case (l, r) =>
                l :< r
              }
            )
              Right(())
            else
              Left(
                FGTypeError(argumentTypes, methodSignature.arguments.values.toSeq)
              )
        } yield methodSignature.returnType

      case StructureLiteral(structureTypeName, arguments) =>
        for {
          argumentTypes <- sequence(
            arguments.map(arg =>
              expressionCheck(environment, arg)
            )
          )
          fs = fields(declarations, structureTypeName)
          _ <-
            if (
              typeCheck(structureTypeName) &&
              (argumentTypes zip fs.map(_.typeName)).forall {
                case (l, r) =>
                  l :< r
              }
            )
              Right(())
            else
              Left(
                FGTypeError(argumentTypes, fs.map(_.typeName))
              )
        } yield structureTypeName

      case FieldSelect(e, fieldName) =>
        for {
          eType <- expressionCheck(environment, e)
          eStructureType <- eType match {
            case stn @ StructureTypeName(_) =>
              Right(stn)
            case _ =>
              Left(FGTypeError(s"The give type[${eType.value}] is not structure!"))
          }
          field <- toEither(
            fields(declarations, eStructureType)
              .find(_.name == fieldName),
            FGTypeError(eStructureType, fieldName)
          )
        } yield field.typeName

      case TypeAssertion(e, typeName) =>
        for {
          _ <- expressionCheck(environment, e)
          _ <-
            if (typeCheck(typeName)) Right(())
            else Left(
              FGTypeError(s"The type[${typeName.value}] fail to check!")
            )
        } yield typeName
    }

    val i = distinct(tdecls(declarations)) &&
      distinct(mdecls(declarations)) &&
      declarationCheck

    expressionCheck(
      environment = Environment(Map.empty),
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
