package featherweightgo.typer.fg

import featherweightgo.ast.fg._
import featherweightgo.typer.fg.TyperFG._
import featherweightgo.util.Utils._

class TyperFG {

  def check(
    main: Main
  ): Either[Throwable, TypeName] = {
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
    ): Either[Throwable, TypeName] = expression match {
      case ValuedStructureLiteral(structureTypeName, _) =>
        Right(structureTypeName)

      case Variable(name) =>
        toEither(environment.get(name), new IllegalArgumentException)

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
            new IllegalArgumentException
          )
          _ <- if ((argumentTypes zip methodSignature.arguments.values).forall {
            case (l, r) =>
              l :< r
            }
          ) Right(()) else Left(new IllegalArgumentException)
        } yield methodSignature.returnType

      case StructureLiteral(structureTypeName, arguments) =>
        for {
          argumentTypes <- sequence(
            arguments.map(arg =>
              expressionCheck(environment, arg)
            )
          )
          fs = fields(declarations, structureTypeName)
          _ <- if (
            typeCheck(structureTypeName) &&
            (argumentTypes zip fs.map(_.typeName)).forall {
              case (l, r) =>
                l :< r
            }
          ) Right(()) else Left(new IllegalArgumentException)
        } yield structureTypeName

      case FieldSelect(e, fieldName) =>
        for {
          eType <- expressionCheck(environment, e)
          eStructureType <- eType match {
            case stn @ StructureTypeName(_) =>
              Right(stn)
            case _ =>
              Left(new IllegalArgumentException)
          }
          field <- toEither(
            fields(declarations, eStructureType)
              .find(_.name == fieldName),
            new IllegalArgumentException
          )
        } yield field.typeName

      case TypeAssertion(e, typeName) =>
        for {
          _ <- expressionCheck(environment, e)
          _ <- if (typeCheck(typeName)) Right(()) else Left(new IllegalArgumentException)
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
        Left(new IllegalArgumentException)
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

object TyperFG {
  case class Environment(
    value: Map[VariableName, TypeName]
  ) {
    def get(variableName: VariableName): Option[TypeName] =
      value.get(variableName)
  }
}
