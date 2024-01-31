package io.joern.gosrccpg.ast.nodes

import scala.collection.mutable.ListBuffer

abstract class Expression extends Node("Expression")

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






