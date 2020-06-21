package parser.fg

import ast.fg._
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class ParserFGTest extends AnyFlatSpec with Diagrams {
  trait SetUp extends ParserFG
  "ParserFG" should "parse a field name" in new SetUp {
    val string = "value"

    val actual = parse(fieldName, string)

    assert(actual.successful)
    assert(actual.get == FieldName("value"))
  }

  it should "parse a type name" in new SetUp {
    val string = "int"

    val actual = parse(typeName, string)

    assert(actual.successful)
    assert(actual.get.value == "int")
  }

  it should "parse a empty struct definition" in new SetUp {
    val string = "struct { }"

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(
      actual.get.fields.keys.toList == Nil
    )
    assert(
      actual.get.fields.values.toList.map(_.value) == Nil
    )
  }

  it should "parse a struct definition with a single field" in new SetUp {
    val string = "struct { value int }"

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(
      actual.get.fields.keys.toList == List(FieldName("value"))
    )
    assert(
      actual.get.fields.values.toList.map(_.value) == List("int")
    )
  }

  it should "parse a struct definition with multiple fields" in new SetUp {
    val string =
      """struct {
        |  f1 int
        |  f2 string
        |}""".stripMargin

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(
      actual.get.fields.keys.toList == List(FieldName("f1"), FieldName("f2"))
    )
    assert(
      actual.get.fields.values.toList.map(_.value) == List("int", "string")
    )
  }

  it should "parse a method signature with empty argument" in new SetUp {
    val string = "() int"

    val actual = parse(methodSignature, string)
    assert(actual.successful)
    assert(actual.get.arguments == Map.empty)
    assert(actual.get.returnType.value == "int")


    val string2 = "()string"

    val actual2 = parse(methodSignature, string2)
    assert(actual2.successful)
    assert(actual2.get.arguments == Map.empty)
    assert(actual2.get.returnType.value == "string")
  }

  it should "parse a method signature with some arguments" in new SetUp {
    val string = "(v1 int, v2 string) int"

    val actual = parse(methodSignature, string)
    assert(actual.successful)
    assert(actual.get.arguments.keys.toList == List(VariableName("v1"), VariableName("v2")))
    assert(actual.get.arguments.values.toList.map(_.value) == List("int", "string"))
    assert(actual.get.returnType.value == "int")
  }

  it should "NOT parse a method signature with comma at the end" in new SetUp {
    pending

    val string = "(v1 int, v2 string,) int"

    val actual = parse(methodSignature, string)
    assert(! actual.successful)
  }

  it should "parse a method signature with the method name" in new SetUp {
    val string = "method1(v1 int, v2 string) int"

    val actual = parse(methodSpecification, string)
    assert(actual.successful)
    assert(actual.get.methodName == MethodName("method1"))
    assert(actual.get.methodSignature.arguments.keys.toList == List(VariableName("v1"), VariableName("v2")))
    assert(actual.get.methodSignature.arguments.values.toList.map(_.value) == List("int", "string"))
    assert(actual.get.methodSignature.returnType.value == "int")
  }

  it should "parse a empty interface definition" in new SetUp {
    val string = "interface {}"

    val actual = parse(interface, string)
    assert(actual.successful)
    assert(actual.get.methods == Nil)
  }

  it should "NOT parse a interface definition without `}`" in new SetUp {
    val string = "interface {"

    val actual = parse(interface, string)
    assert(! actual.successful)
  }

  it should "parse a interface definition with a single method" in new SetUp {
    val string =
      """interface {
        |  method1(v int) string
        |}""".stripMargin

    val actual = parse(interface, string)
    assert(actual.successful)
    assert(actual.get.methods.map(_.methodName) == List(MethodName("method1")))
    assert(actual.get.methods.flatMap(_.methodSignature.arguments.keys.toList) == List(VariableName("v")))
    assert(actual.get.methods.flatMap(_.methodSignature.arguments.values.toList.map(_.value)) == List("int"))
    assert(actual.get.methods.map(_.methodSignature.returnType.value) == List("string"))
  }

  it should "parse a interface definition with multiple single methods" in new SetUp {
    val string =
      """interface {
        |  m1(v1 int) string
        |  m2(v2 string) int
        |}""".stripMargin

    val actual = parse(interface, string)
    assert(actual.successful)
    assert(actual.get.methods.map(_.methodName) == List(MethodName("m1"), MethodName("m2")))
    assert(actual.get.methods.map(_.methodSignature.arguments.keys.toList) == List(List(VariableName("v1")), List(VariableName("v2"))))
    assert(actual.get.methods.map(_.methodSignature.arguments.values.toList.map(_.value)) == List(List("int"), List("string")))
    assert(actual.get.methods.map(_.methodSignature.returnType.value) == List("string", "int"))
  }

  it should "parse type declaration by structure" in new SetUp {
    val string =
      """type t1 struct {
        |  f1 int
        |}""".stripMargin

    val actual = parse(typeDefinition, string)
    assert(actual.successful)
    assert(actual.get.typeLiteral.isInstanceOf[Structure])
  }

  it should "parse type declaration by interface" in new SetUp {
    val string =
      """type t1 interface {
        |  f1(v1 int) string
        |}""".stripMargin

    val actual = parse(typeDefinition, string)
    assert(actual.successful)
    assert(actual.get.typeLiteral.isInstanceOf[Interface])
  }

  it should "parse a method declaration" in new SetUp {
    val string =
      """func (this Tree) method(v int) string {
        |  return v
        |}""".stripMargin

    val actual = parse(methodDefinition, string)
    assert(actual.successful)
    assert(actual.get.receiver ==((VariableName("this"), StructureTypeName("Tree"))))
    assert(actual.get.methodSpecification.methodName == MethodName("method"))
    assert(actual.get.methodSpecification.methodSignature.arguments.keys.toList == List(VariableName("v")))
    assert(actual.get.methodSpecification.methodSignature.arguments.values.toList.map(_.value) == List("int"))
    assert(actual.get.methodSpecification.methodSignature.returnType.value == "string")
  }

  it should "parse a method call without any argument" in new SetUp {
    val string = "v.method()"

    val actual = parse(methodCall, string)
    assert(actual.successful)
  }

  it should "parse a method call with a argument" in new SetUp {
    val string = "v.method(a)"

    val actual = parse(methodCall, string)
    assert(actual.successful)
    assert(actual.get.expression == Variable(VariableName("v")))
    assert(actual.get.methodName == MethodName("method"))
    assert(actual.get.arguments == List(Variable(VariableName("a"))))
  }

  it should "parse a structure literal" in new SetUp {
    val string = "Point{a, b}"

    val actual = parse(structureLiteral, string)
    assert(actual.successful)
    assert(actual.get.structureTypeName == StructureTypeName("Point"))
    assert(actual.get.arguments == List(Variable(VariableName("a")), Variable(VariableName("b"))))
  }

  it should "parse a field select" in new SetUp {
    val string = "this.value"

    val actual = parse(fieldSelect, string)
    assert(actual.successful)
    assert(actual.get.expression == Variable(VariableName("this")))
    assert(actual.get.fieldName == FieldName("value"))
  }

  it should "parse a type assertion" in new SetUp {
    val string = "t.(int)"

    val actual = parse(typeAssertion, string)
    assert(actual.successful)
    assert(actual.get.typeName.value == "int")
    assert(actual.get.expression == Variable(VariableName("t")))
  }

  it should "parse the main function" in new SetUp {
    val string =
      """package main;
        |func (this Tree) method(a int) int {
        |  return a
        |}
        |func main() {
        |  _ = method
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)
    assert(actual.get.main == Variable(VariableName("method")))
    actual.get.declarations.foreach(d => assert(d.isInstanceOf[Method]))
  }

  it should "parse structure literal" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct {
        |  field V
        |}
        |func main() {
        |  _ = T{}
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)
    assert(actual.get.main == StructureLiteral(StructureTypeName("T"), Nil))
  }

  it should "parse field selection of structure literal" in new SetUp {
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

    val actual = parse(mainMethod, string)
    assert(actual.successful)
    assert(actual.get.main == FieldSelect(
      StructureLiteral(StructureTypeName("T"), List(StructureLiteral(StructureTypeName("V"), Nil))),
      FieldName("field")
    ))
  }

  it should "eval to call method" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |func (this V) f() V {
        |  return this
        |}
        |func main() {
        |  _ = V{}.f()
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)
    assert(actual.get.main == MethodCall(StructureLiteral(StructureTypeName("V"), Nil), MethodName("f"), Nil))
  }
}
