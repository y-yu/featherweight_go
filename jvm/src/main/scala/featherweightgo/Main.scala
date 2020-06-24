package featherweightgo

import featherweightgo.evaluator.fg.EvaluatorFGImpl
import featherweightgo.parser.fg.ParserFGImpl
import featherweightgo.typer.fg.TyperFGImpl

object Main {
  val parserFG = new ParserFGImpl()
  val evaluatorFG = new EvaluatorFGImpl()
  val typerFG = new TyperFGImpl()

  private def pp(
    string: String
  ): Unit = {
    println("=========== Input ===========")
    println(string)

    parserFG.parse(string).foreach { ast =>
      println("=========== AST ===========")
      pprint.pprintln(ast)

      println("=========== Result ===========")
      pprint.pprintln(evaluatorFG.eval(ast))

      println("=========== Type ===========")
      pprint.pprintln(typerFG.check(ast))
    }
  }

  def runGetField(): Unit = {
    pp(
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field
        |}""".stripMargin
    )
  }

  def runMethodCall(): Unit = {
    pp(
      """package main;
        |type T struct { }
        |type V struct {
        |  t T
        |}
        |func (this V) f(a T) T {
        |  return this.t
        |}
        |func main() {
        |  _ = V{T{}}.f(T{})
        |}
        |""".stripMargin
    )
  }

  def runTypeCheck(): Unit = {
    pp(
      """package main;
        |type V struct { }
        |type T struct {
        |  value V
        |}
        |type M interface {
        |  Method() V
        |}
        |func (this T) Method() V {
        |  return this.value
        |}
        |type S struct {
        |  field M
        |}
        |func main() {
        |  _ = S{T{V{}}}.field.Method()
        |}
        |""".stripMargin
    )
  }

  def main(args: Array[String]): Unit = {
    runGetField()

    runMethodCall()

    runTypeCheck()
  }
}
