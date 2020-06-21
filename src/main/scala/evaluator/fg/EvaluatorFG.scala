package evaluator.fg

import ast.fg._
import evaluator.Utils._
import types.fg.TypedVariable

class EvaluatorFG {
  def eval(
    main: Main
  ): Either[Throwable, ValuedStructureLiteral] = {
    val declarations = main.declarations

    def innerEval(
      expression: Expression
    ): Either[Throwable, ValuedStructureLiteral] = expression match {
      case v: ValuedStructureLiteral => Right(v)

      case StructureLiteral(structureTypeName, arguments) =>
        evalAll(arguments)
          .map { vs =>
            ValuedStructureLiteral(
              structureTypeName,
              vs
            )
          }

      case FieldSelect(expression, fieldName) =>
        for {
          leftHandResult <- innerEval(expression)
          fs = fields(declarations, leftHandResult.structureTypeName)
          result <- fs.zip(leftHandResult.values).find {
            case((fn, _), _) =>
              fn == fieldName
          }.map {
            v => Right.apply(v._2)
          }
          .getOrElse(Left(new IllegalArgumentException))
        } yield result

      case MethodCall(expression, methodName, arguments) =>
        for {
          leftHandResult <- innerEval(expression)
          argumentResult <- evalAll(arguments)
          vs <- body(declarations, getType(leftHandResult), methodName) match {
            case Some(result) => Right(result)
            case None => Left(new IllegalArgumentException)
          }
          variablesMap <- vs._1 match {
            case h :: ts =>
              for {
                _ <- if (ts.length != argumentResult.length)
                  Left(new IllegalArgumentException)
                else
                  Right(())
              } yield {
                (
                  (h -> leftHandResult) +:
                  ts.zip(argumentResult)
                ).toMap
              }
            case Nil =>
              Left(new IllegalArgumentException)
          }
          result <- innerEval(substitute(vs._2, variablesMap))
        } yield result

      case TypeAssertion(_, _) =>
        // This will be implemented after type checker will be implemented.
        ???

      case Variable(_) =>
        Left(new IllegalArgumentException)
    }

    def evalAll(
      expressions: Seq[Expression]
    ): Either[Throwable, Seq[ValuedStructureLiteral]] =
      expressions
        .map(innerEval)
        .foldLeft(Right(Nil): Either[Throwable, Seq[ValuedStructureLiteral]]) { (x, y) =>
          for {
            xv <- x
            yv <- y
          } yield xv :+ yv
        }

    def substitute(
      expression: Expression,
      variables: Map[TypedVariable, ValuedStructureLiteral]
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

        case FieldSelect(expression, fn) =>
          FieldSelect(loop(expression), fn)

        case MethodCall(expression, mn, arguments) =>
          MethodCall(loop(expression), mn, arguments.map(loop))

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
