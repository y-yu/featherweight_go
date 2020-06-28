package featherweightgo.model.fg.ast

sealed trait AST extends Product with Serializable

case class VariableName(value: String) extends AST

case class FieldName(value: String) extends AST

case class MethodName(value: String) extends AST

abstract class TypeName(val value: String) extends AST

case class MethodSignature(
  arguments: Map[VariableName, TypeName],
  returnType: TypeName
) extends AST

case class MethodSpecification(
  methodName: MethodName,
  methodSignature: MethodSignature
) extends AST

case class StructureTypeName(override val value: String) extends TypeName(value)

case class InterfaceTypeName(override val value: String) extends TypeName(value)

case class AnyTypeName(override val value: String) extends TypeName(value)

sealed trait TypeLiteral extends AST

case class StructureField(
  name: FieldName,
  typeName: TypeName
)

case class Structure(
  fields: Seq[StructureField]
) extends TypeLiteral

case class Interface(
  methods: Seq[MethodSpecification]
) extends TypeLiteral

sealed trait Declaration extends AST

case class Type(
  name: TypeName,
  typeLiteral: TypeLiteral
) extends Declaration

case class Method(
  receiver: (VariableName, StructureTypeName),
  methodSpecification: MethodSpecification,
  body: Expression
) extends Declaration

case class Main(
  declarations: Seq[Declaration],
  main: Expression
) extends AST

sealed trait Expression extends AST

case class Variable(
  variableName: VariableName
) extends Expression

case class MethodCall(
  expression: Expression,
  methodName: MethodName,
  arguments: Seq[Expression]
) extends Expression

case class StructureLiteral(
  structureTypeName: StructureTypeName,
  arguments: Seq[Expression]
) extends Expression

case class FieldSelect(
  expression: Expression,
  fieldName: FieldName
) extends Expression

case class TypeAssertion(
  expression: Expression,
  typeName: TypeName
) extends Expression

case class ValuedStructureLiteral(
  structureTypeName: StructureTypeName,
  values: Seq[ValuedStructureLiteral]
) extends Expression // Does it make sense?
