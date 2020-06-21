import evaluator.fg.EvaluatorFG
import parser.fg.ParserFG

object Main extends ParserFG {
  val evalFG = new EvaluatorFG()

  def runGetField(): Unit = {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field
        |}""".stripMargin

    println("=========== Input ===========")
    println(string)
    val parseResult = parse(mainMethod, string)
    assert(parseResult.successful)
    val ast = parseResult.get

    println("=========== AST ===========")
    pprint.pprintln(ast)
    println("=========== Result ===========")
    pprint.pprintln(evalFG.eval(ast))
  }

  def runMethodCall(): Unit = {
    val string =
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

    println("=========== Input ===========")
    println(string)
    val parseResult = parse(mainMethod, string)
    assert(parseResult.successful)
    val ast = parseResult.get

    println("=========== AST ===========")
    pprint.pprintln(ast)
    println("=========== Result ===========")
    pprint.pprintln(evalFG.eval(ast))
  }

  def main(args: Array[String]): Unit = {
    runGetField()

    runMethodCall()
  }
}
