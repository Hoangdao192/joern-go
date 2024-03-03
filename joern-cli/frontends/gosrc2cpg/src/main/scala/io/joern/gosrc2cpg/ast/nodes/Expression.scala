package io.joern.gosrc2cpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonProperty, JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable.ListBuffer

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "nodeType")
@JsonSubTypes(Array(
    new JsonSubTypes.Type(value = classOf[CallExpression], name = "CallExpression"),
    new JsonSubTypes.Type(value = classOf[Identifier], name = "Identifier"),
    new JsonSubTypes.Type(value = classOf[BadExpression], name = "BadExpression"),
    new JsonSubTypes.Type(value = classOf[BasicLiteralExpression], name = "BasicLiteral"),
    new JsonSubTypes.Type(value = classOf[BinaryExpression], name = "BinaryExpression"),
    new JsonSubTypes.Type(value = classOf[EllipsisExpression], name = "EllipsisExpression"),
    new JsonSubTypes.Type(value = classOf[IndexExpression], name = "IndexExpression"),
    new JsonSubTypes.Type(value = classOf[IndexListExpression], name = "IndexListExpression"),
    new JsonSubTypes.Type(value = classOf[KeyValueExpression], name = "KeyValueExpression"),
    new JsonSubTypes.Type(value = classOf[ParenthesizedExpression], name = "ParenthesizedExpression"),
    new JsonSubTypes.Type(value = classOf[SelectorExpression], name = "SelectorExpression"),
    new JsonSubTypes.Type(value = classOf[SliceExpression], name = "SliceExpression"),
    new JsonSubTypes.Type(value = classOf[StarExpression], name = "StarExpression"),
    new JsonSubTypes.Type(value = classOf[TypeAssertExpression], name = "TypeAssertExpression"),
    new JsonSubTypes.Type(value = classOf[UnaryExpression], name = "UnaryExpression"),
    new JsonSubTypes.Type(value = classOf[FunctionLiteral], name = "FunctionLiteral"),
    new JsonSubTypes.Type(value = classOf[CompositeLiteral], name = "CompositeLiteral"),

    new JsonSubTypes.Type(value = classOf[FunctionType], name = "FunctionType"),
    new JsonSubTypes.Type(value = classOf[ArrayType], name = "ArrayType"),
    new JsonSubTypes.Type(value = classOf[ChanelType], name = "ChanelType"),
    new JsonSubTypes.Type(value = classOf[InterfaceType], name = "InterfaceType"),
    new JsonSubTypes.Type(value = classOf[MapType], name = "MapType"),
    new JsonSubTypes.Type(value = classOf[StructType], name = "StructType")
))
abstract class Expression extends Node

class Identifier extends Expression {
    var namePosition: Int = 0
    var name: Option[String] = None
}

class CallExpression extends Expression {
    var function: Option[Expression] = None
    var lparen: Int = 0
    var rparen: Int = 0
    var args: ListBuffer[Expression] = new ListBuffer()
    var ellipsis: Int = 0
}

class BadExpression extends Expression {
    var from: Int = 0
    var to: Int = 0
}

class BasicLiteralExpression extends Expression {
    var valuePosition: Int = 0
    var kind: Int = 0
    var value: String = ""
}

class BinaryExpression extends Expression {
    var leftExpression: Option[Expression] = None
    var rightExpression: Option[Expression] = None
    var operatorPosition: Int = 0
    var operator: Int = 0
}

class EllipsisExpression extends Expression {
    var ellipsis: Int = 0
    var element: Option[Expression] = None
}

class IndexExpression extends Expression {
    var expression: Option[Expression] = None
    var leftBracket: Int = 0
    var rightBracket: Int = 0
    var index: Option[Expression] = None
}

class IndexListExpression extends Expression {
    var expression: Option[Expression] = None
    var leftBracket: Int = 0
    var rightBracket: Int = 0
    var indicates: ListBuffer[Expression] = new ListBuffer()
}

class KeyValueExpression extends Expression {
    var key: Option[Expression] = None
    var value: Option[Expression] = None
    var colon: Int = 0
}

class ParenthesizedExpression extends Expression {
    var lparen: Int = 0
    var rparen: Int = 0
    var expression: Option[Expression] = None
}

class SelectorExpression extends Expression {
    var expression: Option[Expression] = None
    var selector: Option[Identifier] = None
}

class SliceExpression extends Expression {
    var expression: Option[Expression] = None
    var low: Option[Expression] = None
    var high: Option[Expression] = None
    var max: Option[Expression] = None
    var slice3: Boolean = false
    var leftBracket: Int = 0
    var rightBracket: Int = 0
}

class StarExpression extends Expression {
    var star: Int = 0
    var expression: Option[Expression] = None
}

class TypeAssertExpression extends Expression {
    var expression: Option[Expression] = None
    var typeExpression: Option[Expression] = None
    var lparen: Int = 0
    var rparen: Int = 0
}

class UnaryExpression extends Expression {
    var operatorPosition: Int = 0
    var operator: Int = 0
    var expression: Option[Expression] = None
}

class FunctionLiteral extends Expression {
    @JsonProperty("type")
    var functionType: Option[FunctionType] = None
    var body: Option[BlockStatement] = None
}

class CompositeLiteral extends Expression {
    var typeExpression: Option[Expression] = None
    var lbrace: Int = -1
    var rbrace: Int = -1
    var elements: ListBuffer[Expression] = ListBuffer()
    var incomplete: Boolean = false
}





