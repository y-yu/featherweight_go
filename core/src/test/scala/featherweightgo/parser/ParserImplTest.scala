package featherweightgo.parser

import featherweightgo.model.ast._
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec

class ParserImplTest extends AnyFlatSpec with Diagrams {
  trait SetUp extends ParserImpl.ParserImpl

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
      actual.get.fields.map(_.name) == Nil
    )
    assert(
      actual.get.fields.map(_.typ) == Nil
    )
  }

  it should "parse a struct definition with a single field" in new SetUp {
    val string = "struct { value int }"

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(actual.get.fields.nonEmpty)
    assert(
      actual.get.fields.map(_.name) == List(FieldName("value"))
    )
    assert(actual.get.fields.map(_.typ) == List(AnyNamedType(AnyTypeName("int"), List())))
  }

  it should "parse a struct definition with multiple fields" in new SetUp {
    val string =
      """struct {
        |  f1 int
        |  f2 string
        |}""".stripMargin

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(actual.get.fields.nonEmpty)
    assert(
      actual.get.fields.map(_.name) == List(FieldName("f1"), FieldName("f2"))
    )

    assert(actual.get.fields.map(_.typ) == List(AnyNamedType(AnyTypeName("int"), List()), AnyNamedType(AnyTypeName("string"), List())))
  }

  it should "parse a struct definition with a single named type field" in new SetUp {
    val string = "struct { value List[int] }"

    val actual = parse(structure, string)

    assert(actual.successful)
    assert(actual.get.fields.nonEmpty)
    assert(
      actual.get.fields.map(_.name) == List(FieldName("value"))
    )
    assert(actual.get.fields.map(_.typ) == List(AnyNamedType(AnyTypeName("List"), List(AnyNamedType(AnyTypeName("int"), List())))))
  }

  it should "parse a method signature with empty argument" in new SetUp {
    val string = "() int"

    val actual = parse(methodSignature, string)
    assert(actual.successful)
    assert(actual.get.arguments == Map.empty)
    assert(actual.get.returnType == AnyNamedType(AnyTypeName("int"), List()))


    val string2 = "()string"

    val actual2 = parse(methodSignature, string2)
    assert(actual2.successful)
    assert(actual2.get.arguments == Map.empty)
    assert(actual2.get.returnType == AnyNamedType(AnyTypeName("string"), List()))
  }

  it should "parse a method signature with some arguments" in new SetUp {
    val string = "(v1 int, v2 string) int"

    val actual = parse(methodSignature, string)
    assert(actual.successful)
    assert(actual.get.arguments.keys.toList == List(VariableName("v1"), VariableName("v2")))
    assert(actual.get.arguments.values.toList == List(AnyNamedType(AnyTypeName("int"), List()), AnyNamedType(AnyTypeName("string"), List())))
    assert(actual.get.returnType == AnyNamedType(AnyTypeName("int"), List()))
  }

  it should "NOT parse a method signature with comma at the end" in new SetUp {
    pending

    val string = "(v1 int, v2 string,) int"

    val actual = parse(methodSignature, string)
    assert(! actual.successful)
  }

  it should "parse a method signature with a named type argument" in new SetUp {
    val string = "(v1 List[string]) int"

    val actual = parse(methodSignature, string)
    assert(actual.successful)
    assert(actual.get.arguments.keys.toList == List(VariableName("v1")))
    assert(actual.get.arguments.values.toList == List(AnyNamedType(AnyTypeName("List"), List(AnyNamedType(AnyTypeName("string"), List())))))
    assert(actual.get.returnType == AnyNamedType(AnyTypeName("int"), List()))
  }

  it should "parse a method signature with the method name" in new SetUp {
    val string = "method1(v1 int, v2 string) int"

    val actual = parse(methodSpecification, string)
    assert(actual.successful)
    assert(actual.get.methodName == MethodName("method1"))
    assert(actual.get.methodSignature.arguments.keys.toList == List(VariableName("v1"), VariableName("v2")))
    assert(actual.get.methodSignature.arguments.values.toList == List(AnyNamedType(AnyTypeName("int"), List()), AnyNamedType(AnyTypeName("string"), List())))
    assert(actual.get.methodSignature.returnType == AnyNamedType(AnyTypeName("int"), List()))
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
    assert(actual.get.methods.flatMap(_.methodSignature.arguments.values.toList) ==  List(AnyNamedType(AnyTypeName("int"), List())))
    assert(actual.get.methods.map(_.methodSignature.returnType) == List(AnyNamedType(AnyTypeName("string"), List())))
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
    assert(actual.get.methods.map(_.methodSignature.arguments.values.toList) == List(List(AnyNamedType(AnyTypeName("int"), List())), List(AnyNamedType(AnyTypeName("string"), List()))))
    assert(actual.get.methods.map(_.methodSignature.returnType) ==  List(AnyNamedType(AnyTypeName("string"), List()), AnyNamedType(AnyTypeName("int"), List())))
  }

  it should "parse a interface definition with a single named type method" in new SetUp {
    val string =
      """interface {
        |  method1[T any](v int) List[T]
        |}""".stripMargin

    val actual = parse(interface, string)
    assert(actual.successful)
    assert(actual.get.methods.map(_.methodName) == List(MethodName("method1")))
    assert(actual.get.methods.flatMap(_.methodSignature.arguments.keys.toList) == List(VariableName("v")))
    assert(actual.get.methods.flatMap(_.methodSignature.arguments.values.toList) == List(AnyNamedType(AnyTypeName("int"), List())))
    assert(actual.get.methods.flatMap(_.methodSignature.typeFormals) == List(TypeFormal(TypeParameter("T"), InterfaceType(InterfaceTypeName("any"), Nil))))
    assert(actual.get.methods.map(_.methodSignature.returnType) == List(AnyNamedType(AnyTypeName("List"), List(AnyNamedType(AnyTypeName("T"), List())))))
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

  it should "parse type declaration by interface with generics" in new SetUp {
    val string =
      """type t1[A any] interface {
        |  f1(v1 int) string
        |}""".stripMargin

    val actual = parse(typeDefinition, string)
    assert(actual.successful)
    assert(actual.get.typeFormals == List(TypeFormal(TypeParameter("A"), InterfaceType(InterfaceTypeName("any"), Nil))))
    assert(actual.get.typeLiteral.isInstanceOf[Interface])
  }

  it should "parse type declaration by structure with generics" in new SetUp {
    val string =
      """type t1[A any] struct {
        |  f1 List[A]
        |}""".stripMargin

    val actual = parse(typeDefinition, string)
    assert(actual.successful)
    assert(actual.get.typeFormals == List(TypeFormal(TypeParameter("A"), InterfaceType(InterfaceTypeName("any"), Nil))))
    assert(actual.get.typeLiteral.isInstanceOf[Structure])
    assert(actual.get.typeLiteral.asInstanceOf[Structure].fields.map(_.typ)
      == List(AnyNamedType(AnyTypeName("List"), List(AnyNamedType(AnyTypeName("A"), List())))))
  }

  it should "parse a method declaration" in new SetUp {
    val string =
      """func (this Tree) method(v int) string {
        |  return v
        |}""".stripMargin

    val actual = parse(methodDefinition, string)
    assert(actual.successful)
    assert(
      actual.get.receiver ==
        MethodReceiver(VariableName("this"), StructureTypeName("Tree"), Nil)
    )
    assert(actual.get.methodSpecification.methodName == MethodName("method"))
    assert(actual.get.methodSpecification.methodSignature.arguments.keys.toList == List(VariableName("v")))
    assert(actual.get.methodSpecification.methodSignature.arguments.values.toList == List(AnyNamedType(AnyTypeName("int"), List())))
    assert(actual.get.methodSpecification.methodSignature.returnType == AnyNamedType(AnyTypeName("string"), List()))
  }

  it should "parse a method declaration with generics" in new SetUp {
    val string =
      """func (this Tree[V any]) method[A any](v int) string {
        |  return v
        |}""".stripMargin

    val actual = parse(methodDefinition, string)
    assert(actual.successful)
    assert(
      actual.get.receiver ==
        MethodReceiver(
          VariableName("this"),
          StructureTypeName("Tree"),
          List(TypeFormal(TypeParameter("V"), InterfaceType(InterfaceTypeName("any"), Nil)))
        )
    )
    assert(actual.get.methodSpecification.methodName == MethodName("method"))
    assert(actual.get.methodSpecification.methodSignature.typeFormals == List(TypeFormal(TypeParameter("A"), InterfaceType(InterfaceTypeName("any"), Nil))))
    assert(actual.get.methodSpecification.methodSignature.arguments.keys.toList == List(VariableName("v")))
    assert(actual.get.methodSpecification.methodSignature.arguments.values.toList ==  List(AnyNamedType(AnyTypeName("int"), List())))
    assert(actual.get.methodSpecification.methodSignature.returnType == AnyNamedType(AnyTypeName("string"), List()))
  }

  it should "parse a method call without any argument" in new SetUp {
    val string = "v.method()"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[MethodCall])

    val mc = actual.get.asInstanceOf[MethodCall]
    assert(mc.expression == Variable(VariableName("v")))
    assert(mc.methodName == MethodName("method"))
    assert(mc.arguments == Nil)
  }

  it should "parse a method call with a argument" in new SetUp {
    val string = "v.method(a)"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[MethodCall])

    val mc = actual.get.asInstanceOf[MethodCall]
    assert(mc.expression == Variable(VariableName("v")))
    assert(mc.methodName == MethodName("method"))
    assert(mc.arguments == List(Variable(VariableName("a"))))
  }

  it should "parse a generic method call with a argument" in new SetUp {
    val string = "v.method[int](a)"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[MethodCall])

    val mc = actual.get.asInstanceOf[MethodCall]
    assert(mc.expression == Variable(VariableName("v")))
    assert(mc.methodName == MethodName("method"))
    assert(mc.arguments == List(Variable(VariableName("a"))))
    assert(mc.types == List(AnyNamedType(AnyTypeName("int"), List())))
  }

  it should "parse a structure literal" in new SetUp {
    val string = "Point{a, b}"

    val actual = parse(structureLiteral, string)
    assert(actual.successful)
    assert(actual.get.structureType == StructureType(StructureTypeName("Point"), Nil))
    assert(actual.get.arguments == List(Variable(VariableName("a")), Variable(VariableName("b"))))
  }

  it should "parse a structure literal with a type parameter" in new SetUp {
    val string = "Point[int]{a, b}"

    val actual = parse(structureLiteral, string)
    assert(actual.successful)
    assert(actual.get.structureType == StructureType(StructureTypeName("Point"), List(AnyNamedType(AnyTypeName("int"), List()))))
    assert(actual.get.arguments == List(Variable(VariableName("a")), Variable(VariableName("b"))))
  }

  it should "parse a field select" in new SetUp {
    val string = "this.value"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[FieldSelect])

    val fc = actual.get.asInstanceOf[FieldSelect]

    assert(fc.expression == Variable(VariableName("this")))
    assert(fc.fieldName == FieldName("value"))
  }

  it should "parse a filed select in structure literal " in new SetUp {
    val string = "Point{a.b, b.c}"

    val actual = parse(expression, string)
    assert(actual.successful)
  }

  it should "parse a type assertion" in new SetUp {
    val string = "t.(int)"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[TypeAssertion])

    val ta = actual.get.asInstanceOf[TypeAssertion]

    assert(ta.typ == AnyNamedType(AnyTypeName("int"), List()))
    assert(ta.expression == Variable(VariableName("t")))
  }

  it should "parse a named type assertion" in new SetUp {
    val string = "t.(List[int])"

    val actual = parse(expression, string)
    assert(actual.successful)
    assert(actual.get.isInstanceOf[TypeAssertion])

    val ta = actual.get.asInstanceOf[TypeAssertion]

    assert(ta.typ == AnyNamedType(AnyTypeName("List"), List(AnyNamedType(AnyTypeName("int"), List()))))
    assert(ta.expression == Variable(VariableName("t")))
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
    actual.get.declarations.foreach(d => assert(d.isInstanceOf[MethodDeclaration]))
  }

  it should "parse the main function even if there are whitespaces behind `package main;`" in new SetUp {
    val string =
      """
        |
        |package main;
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
    actual.get.declarations.foreach(d => assert(d.isInstanceOf[MethodDeclaration]))
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
    assert(actual.get.main == StructureLiteral(StructureType(StructureTypeName("T"), Nil), Nil))
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
      StructureLiteral(StructureType(StructureTypeName("T"), Nil), List(StructureLiteral(StructureType(StructureTypeName("V"), Nil), Nil))),
      FieldName("field")
    ))
  }

  it should "parse to call method" in new SetUp {
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
    assert(actual.get.main == MethodCall(StructureLiteral(StructureType(StructureTypeName("V"), Nil), Nil), MethodName("f"), Nil, Nil))
  }

  it should "parse interface and its function" in new SetUp {
    val string =
      """package main;
        |type V struct { }
        |type T struct { }
        |type T interface {
        |  method() V
        |}
        |func (this T) method() V {
        |  return V{}
        |}
        |func main() {
        |  _ = T{}.method()
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)
    assert(actual.get.main == MethodCall(StructureLiteral(StructureType(StructureTypeName("T"), Nil), Nil), MethodName("method"), Nil, Nil) )
  }

  it should "parse interface and struct" in new SetUp {
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

    val actual = parse(mainMethod, string)
    assert(actual.successful)
  }

  it should "parse expression in the StructureLiteral" in new SetUp {
    val string =
      """package main;
        |type Number interface { }
        |type Zero struct { }
        |type Succ struct {
        |  pred Number
        |}
        |func main() {
        |  _ = Succ{Succ{Succ{Zero{}}}.pred}
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)
  }

  it should "parse type formals left to right order" in new SetUp {
    val string =
      """package main;
        |type any interface { }
        |type Function[A any, B any] interface {
        |  Apply(x A) B
        |}
        |type A struct { }
        |
        |func main() {
        |  _ = A{}
        |}
        |""".stripMargin

    val actual = parse(mainMethod, string)
    assert(actual.successful)

    actual.map { ast =>
      ast.declarations.foreach {
        case TypeDeclaration(name, typeFormals, _) if name.value == "Function" =>
          assert(typeFormals == List(
            TypeFormal(
              typeParameter = TypeParameter(typeName = "A"),
              interfaceType = InterfaceType(
                interfaceTypeName = InterfaceTypeName(value = "any"),
                types = List()
              )
            ),
            TypeFormal(
              typeParameter = TypeParameter(typeName = "B"),
              interfaceType = InterfaceType(
                interfaceTypeName = InterfaceTypeName(value = "any"),
                types = List()
              )
            )
          ))
        case _ =>
          ()
      }
    }
  }

  it should "parse a function which returns a generic value" in new SetUp {
    val string =
      """func (this Cons[A any]) Concat(a List[A]) List[A] {
        |    return Cons[A]{this.head, this.tail.Concat(a)}
        |}
        |""".stripMargin

    val actual = parse(methodDefinition, string)
    assert(actual.successful)
    assert(
      actual.get.receiver ==
        MethodReceiver(
          VariableName("this"),
          StructureTypeName("Cons"),
          List(TypeFormal(TypeParameter("A"),
            InterfaceType(InterfaceTypeName("any"),List()))))
    )
    assert(
      actual.get.body == StructureLiteral(
        structureType = StructureType(
          structureTypeName = StructureTypeName(value = "Cons"),
          types = List(AnyNamedType(typeName = AnyTypeName(value = "A"), types = List()))
        ),
        arguments = List(
          FieldSelect(
            expression = Variable(variableName = VariableName(value = "this")),
            fieldName = FieldName(value = "head")
          ),
          MethodCall(
            expression = FieldSelect(
              expression = Variable(variableName = VariableName(value = "this")),
              fieldName = FieldName(value = "tail")
            ),
            methodName = MethodName(value = "Concat"),
            types = List(),
            arguments = List(Variable(variableName = VariableName(value = "a")))
          )
        )
      )
    )
  }
}
