package featherweightgo.evaluator

import featherweightgo.model.ast._
import featherweightgo.model.error.FGError.FGEvalError
import featherweightgo.model.typer.Implement.ImplementSyntax
import featherweightgo.model.typer.TypeBound
import featherweightgo.util.Utils._


class EvaluatorImpl extends Evaluator {
  def eval(
    main: Main
  ): Either[FGEvalError, Primitive] = {
    // The operator `<:` requires this list. It's used like an operator
    // taking 2 parameters so `declarations` is defined as `implicit`.
    implicit val declarations: List[Declaration] = main.declarations

    def innerEval(
      expression: Expression
    ): Either[FGEvalError, Primitive] = {
      def evalError[A]: Either[FGEvalError, A] = Left(FGEvalError(expression))

      expression match {
        case v: Primitive => Right(v)

        case StructureLiteral(structureTypeName, arguments) =>
          evalAll(arguments)
            .map { vs =>
              ValuedStructureLiteral(
                structureTypeName,
                vs
              )
            }

        case fs @ FieldSelect(e, fieldName) =>
          for {
            leftHandResult <- innerEval(e)
            valuedStructureLiteral <- leftHandResult match {
              case vsl: ValuedStructureLiteral =>
                Right(vsl)
              case p =>
                Left(FGEvalError(fs,
                  message = Some(s"Primitive type $p doesn't have any fields!")
                ))
            }
            fs = fields(declarations, valuedStructureLiteral.structureTypeName)
            result <- fs
              .zip(valuedStructureLiteral.values)
              .find {
                case (sf, _) =>
                  sf.name == fieldName
              }
              .map {
                v => Right.apply(v._2)
              }
              .getOrElse(evalError)
          } yield result

        case MethodCall(e, methodName, types, arguments) =>
          for {
            leftHandResult <- innerEval(e)
            argumentResult <- evalAll(arguments)
            vs <- body(declarations, `type`(leftHandResult), methodName, types) match {
              case Some(result) => Right(result)
              case None => evalError
            }
            variablesMap <- vs._1 match {
              case h :: ts =>
                for {
                  _ <- if (ts.length != argumentResult.length)
                    evalError
                  else
                    Right(())
                } yield {
                  (
                    (h -> leftHandResult) +:
                      ts.zip(argumentResult)
                    ).toMap
                }
              case Nil =>
                evalError
            }
            result <- innerEval(substitute(vs._2, variablesMap))
          } yield result

        case TypeAssertion(e, typeName) =>
          for {
            leftHand <- innerEval(e)
            result <- if ( TypeBound.empty |- `type`(leftHand) <:< typeName)
              Right(leftHand)
            else
              evalError
          } yield result

        case Variable(_) =>
          evalError
      }
    }

    def evalAll(
      expressions: List[Expression]
    ): Either[FGEvalError, List[Primitive]] =
      expressions
        .map(innerEval)
        .foldLeft(Right(Nil): Either[FGEvalError, List[Primitive]]) { (x, y) =>
          for {
            xv <- x
            yv <- y
          } yield xv :+ yv
        }

    def substitute(
      expression: Expression,
      variables: Map[TypedVariable, Primitive]
    ): Expression = {
      def loop(
        e: Expression
      ): Expression = e match {
        case Variable(variableName) =>
          variables.find(_._1.variable.variableName == variableName) match {
            case Some((_, v)) =>
              v
            case None =>
              expression
          }

        case ValuedStructureLiteral(_, _) =>
          e

        case v: Primitive =>
          v

        case FieldSelect(expression, fn) =>
          FieldSelect(loop(expression), fn)

        case MethodCall(expression, mn, types, arguments) =>
          MethodCall(loop(expression), mn, types, arguments.map(loop))

        case TypeAssertion(expression, tn) =>
          TypeAssertion(loop(expression), tn)

        case StructureLiteral(stn, arguments) =>
          StructureLiteral(stn, arguments.map(loop))
      }

      loop(expression)
    }

    innerEval(main.main)
  }
}
