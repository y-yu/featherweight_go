package featherweightgo.model.ast

import featherweightgo.model.ast.AbstractStructureType.StructureType

sealed trait AST extends Product with Serializable

case class VariableName(value: String) extends AST

case class FieldName(value: String) extends AST

case class MethodName(value: String) extends AST

sealed abstract class TypeName(val value: String) extends AST

case class MethodSignature(
  typeFormals: List[TypeFormal],
  arguments: Map[VariableName, Type],
  returnType: Type
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
  typ: Type
)

case class Structure(
  fields: List[StructureField]
) extends TypeLiteral

case class Interface(
  methods: List[MethodSpecification]
) extends TypeLiteral

sealed trait Declaration extends AST

case class TypeDeclaration(
  name: TypeName,
  typeFormals: List[TypeFormal],
  typeLiteral: TypeLiteral
) extends Declaration

case class MethodReceiver(
  variableName: VariableName,
  structureTypeName: StructureTypeName,
  typeFormals: List[TypeFormal]
)

case class MethodDeclaration(
  receiver: MethodReceiver,
  methodSpecification: MethodSpecification,
  body: Expression
) extends Declaration

case class Main(
  declarations: List[Declaration],
  main: Expression
) extends AST

sealed trait Expression extends AST

case class Variable(
  variableName: VariableName
) extends Expression

case class MethodCall(
  expression: Expression,
  methodName: MethodName,
  types: List[Type],
  arguments: List[Expression]
) extends Expression

case class StructureLiteral(
  structureType: StructureType,
  arguments: List[Expression]
) extends Expression

case class FieldSelect(
  expression: Expression,
  fieldName: FieldName
) extends Expression

case class TypeAssertion(
  expression: Expression,
  typ: Type
) extends Expression

sealed trait IntegerOps extends Expression

case class Plus(
  lhs: Expression,
  rhs: Expression
) extends IntegerOps

sealed trait StringOps extends Expression

case class Concat(
  lhs: Expression,
  rhs: Expression
) extends IntegerOps

sealed trait Primitive extends Expression

case class ValuedStructureLiteral(
  structureTypeName: StructureType,
  values: List[Primitive]
) extends Primitive // Does it make sense?

case class IntegerValue(
  value: Int
) extends Primitive

case class StringValue(
  value: String
) extends Primitive

sealed trait Type extends AST {
  def name: TypeName
}

case class TypeParameter(
  typeName: String
) extends Type {
  def name: TypeName = AnyTypeName(typeName)
}

case class AnyNamedType(
  typeName: TypeName,
  types: List[Type]
) extends Type {
  def name: TypeName = typeName
}

sealed abstract class AbstractStructureType(
  val structureTypeName: StructureTypeName,
  val types: List[Type]
) extends Type {
  def name: TypeName = structureTypeName
}

object AbstractStructureType {
  case class StructureType(
    override val structureTypeName: StructureTypeName,
    override val types: List[Type]
  ) extends AbstractStructureType(
    structureTypeName, types
  )

  case object IntegerType extends AbstractStructureType(
    StructureTypeName("int"), Nil
  )

  case object StringType extends AbstractStructureType(
    StructureTypeName("string"), Nil
  )
}

case class InterfaceType(
  interfaceTypeName: InterfaceTypeName,
  types: List[Type]
) extends Type {
  def name: TypeName = interfaceTypeName
}

case class TypeFormal(
  typeParameter: TypeParameter,
  interfaceType: InterfaceType
) extends AST

case class TypeActual(
  types: List[Type]
) extends AST