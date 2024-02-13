package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable.ListBuffer

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "nodeType")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[AssignStatement], name = "AssignStatement"),
  new JsonSubTypes.Type(value = classOf[BadStatement], name = "BadStatement"),
  new JsonSubTypes.Type(value = classOf[BlockStatement], name = "BlockStatement"),
  new JsonSubTypes.Type(value = classOf[BranchStatement], name = "BranchStatement"),
  new JsonSubTypes.Type(value = classOf[DeclarationStatement], name = "DeclarationStatement"),
  new JsonSubTypes.Type(value = classOf[DeferStatement], name = "DeferStatement"),
  new JsonSubTypes.Type(value = classOf[EmptyStatement], name = "EmptyStatement"),
  new JsonSubTypes.Type(value = classOf[ExpressionStatement], name = "ExpressionStatement"),
  new JsonSubTypes.Type(value = classOf[ForStatement], name = "ForStatement"),
  new JsonSubTypes.Type(value = classOf[GoStatement], name = "GoStatement"),
  new JsonSubTypes.Type(value = classOf[IfStatement], name = "IfStatement"),
  new JsonSubTypes.Type(value = classOf[IncrementDecrementStatement], name = "IncrementDecrementStatement"),
  new JsonSubTypes.Type(value = classOf[LabeledStatement], name = "LabeledStatement"),
  new JsonSubTypes.Type(value = classOf[RangeStatement], name = "RangeStatement"),
  new JsonSubTypes.Type(value = classOf[ReturnStatement], name = "ReturnStatement"),
  new JsonSubTypes.Type(value = classOf[SelectStatement], name = "SelectStatement"),
  new JsonSubTypes.Type(value = classOf[SendStatement], name = "SendStatement"),
  new JsonSubTypes.Type(value = classOf[SwitchStatement], name = "SwitchStatement"),
  new JsonSubTypes.Type(value = classOf[TypeSwitchStatement], name = "TypeSwitchStatement")
))
abstract class Statement extends Node("Statement")

class AssignStatement extends Statement {
  var lhs: ListBuffer[Expression] = new ListBuffer()
  var rhs: ListBuffer[Expression] = new ListBuffer()
  var tokenPosition: Int = 0
  var token: Int = 0
}

class BadStatement extends Statement {
  var from: Int = 0
  var to: Int = 0
}

class BlockStatement extends Statement {
  var lbrace: Int = 0
  var rbrace: Int = 0
  var statements: ListBuffer[Statement] = new ListBuffer()
}

class BranchStatement extends Statement {
  var tokenPosition: Int = 0
  var token: Int = 0
  var label: Option[Identifier] = None
}

class DeclarationStatement extends Statement {
  var declaration: Option[Declaration] = None
}

class DeferStatement extends Statement {
  var defer: Int = 0
  var call: Option[CallExpression] = None
}

class EmptyStatement extends Statement {
  var semicolon: Int = 0
  var isImplicit: Boolean = false
}

class ExpressionStatement extends Statement {
  var expression: Option[Expression] = None
}

class ForStatement extends Statement {
  var forPosition: Int = 0
  var initialization: Option[Statement] = None
  var condition: Option[Expression] = None
  var post: Option[Statement] = None
  var body: Option[BlockStatement] = None
}

class GoStatement extends Statement {
  var goPosition: Int = 0
  var call: Option[CallExpression] = None
}

class IfStatement extends Statement {
  var ifPosition: Int = 0
  var initialization: Option[Statement] = None
  var condition: Option[Expression] = None
  var body: Option[BlockStatement] = None
  var elseStatement: Option[Statement] = None
}

class IncrementDecrementStatement extends Statement {
  var expression: Option[Expression] = None
  var tokenPosition: Int = 0
  var token: Int = 0
}

class LabeledStatement extends Statement {
  var statement: Option[Statement] = None
  var label: Option[Identifier] = None
  var colon: Int = 0
}

class RangeStatement extends Statement {
  var forPosition: Int = 0
  var key: Option[Expression] = None
  var value: Option[Expression] = None
  var tokenPosition: Int = 0
  var token: Int = 0
  var range: Int = 0
  var expression: Option[Expression] = None
  var body: Option[BlockStatement] = None
}

class ReturnStatement extends Statement {
  var returnPosition: Int = 0
  var results: ListBuffer[Expression] = new ListBuffer()
}

class SelectStatement extends Statement {
  var select: Int = 0
  var body: Option[BlockStatement] = None
}

class SendStatement extends Statement {
  var chanel: Option[Expression] = None
  var arrow: Int = 0
  var value: Option[Expression] = None
}

class SwitchStatement extends Statement {
  var switchPosition: Int = 0
  var initialization: Option[Statement] = None
  var tag: Option[Expression] = None
  var body: Option[BlockStatement] = None
}

class TypeSwitchStatement extends Statement {
  var switchPosition: Int = 0
  var initialization: Option[Statement] = None
  var assign: Option[Statement] = None
  var body: Option[BlockStatement] = None
}
