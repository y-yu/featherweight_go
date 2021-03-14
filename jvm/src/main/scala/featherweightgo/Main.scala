package featherweightgo

import featherweightgo.evaluator.EvaluatorImpl
import featherweightgo.parser.ParserImpl
import featherweightgo.typer.TyperImpl

object Main {
  val parserFG = new ParserImpl()
  val evaluatorFG = new EvaluatorImpl()
  val typerFG = new TyperImpl()

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

  def runGeneric(): Unit = {
    pp(
      """package main;
        |type Number interface { }
        |type Zero struct { }
        |type Succ struct {
        |  pred Number
        |}
        |type any interface { }
        |type List[A any] interface {
        |  Length() Number
        |}
        |type Nil[A any] struct { }
        |type Cons[A any] struct {
        |  head A
        |  tail List[A]
        |}
        |func (this Nil[A any]) Length() Number {
        |  return Zero{}
        |}
        |func (this Cons[A any]) Length() Number {
        |  return Succ{this.tail.Length()}
        |}
        |type V struct { }
        |type S struct { }
        |func main() {
        |  _ = Cons[V]{V{}, Cons[V]{V{}, Nil[V]{}}}.Length()
        |}
        |""".stripMargin
    )
  }

  def main(args: Array[String]): Unit = {
    runGetField()

    runMethodCall()

    runTypeCheck()

    runGeneric()
  }
}
