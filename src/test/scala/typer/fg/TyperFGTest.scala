package typer.fg

import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec
import parser.fg.ParserFG
import ast.fg._

class TyperFGTest extends AnyFlatSpec with Diagrams {
  trait SetUp extends ParserFG {
    val sut = new TyperFG
  }

  "TyperFG" should "be well-typed the Main" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field
        |}
        |""".stripMargin

    val parseResult = parse(mainMethod, string)
    assert(parseResult.successful)

    val ast = parseResult.get

    assert(sut.check(ast).isRight)
  }

  it should "NOT be well-typed Main if the code refer a non-exist field" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{V{}}.field2
        |}
        |""".stripMargin

    val parseResult = parse(mainMethod, string)
    assert(parseResult.successful)

    val ast = parseResult.get

    assert(sut.check(ast).isLeft)
  }

  it should "be well-typed Main if the code has interface" in new SetUp {
    val string =
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

    val parseResult = parse(mainMethod, string)
    assert(parseResult.successful)

    val ast = parseResult.get
    val actual = sut.check(ast)
    assert(actual == Right(AnyTypeName("V")))
  }
}
