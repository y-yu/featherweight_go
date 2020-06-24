package featherweightgo.typer.fg

import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec
import featherweightgo.parser.fg.ParserFGImpl
import featherweightgo.model.fg.ast._

class TyperFGImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp {
    // This test depends on `ParserFGImpl`(the other logic)
    // so this is not a *unit test*!
    // But it would take too time to mock parser or write the AST directory...
    // That's the why this test depends on the parser implementation.
    val parser = new ParserFGImpl
    
    val sut = new TyperFGImpl
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

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach(ast => assert(sut.check(ast).isRight))
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

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach(ast => assert(sut.check(ast).isLeft))
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

    val parseResult = parser.parse(string)
    assert(parseResult.isRight)

    parseResult.foreach { ast =>
      val actual = sut.check(ast)

      assert(actual == Right(AnyTypeName("V")))
    }
  }
}
