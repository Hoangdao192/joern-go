package io.joern.gosrccpg.ast.nodes

import scala.collection.mutable.ListBuffer

class Field extends Node("Field") {
  var documentation: Option[CommentGroup] = None
  var names: ListBuffer[Identifier] = new ListBuffer()
  var typeExpression: Option[Expression] = None
  var tag: Option[BasicLiteralExpression] = None
  var comment: Option[CommentGroup] = None
}

class FieldList extends Node("FieldList") {
  var opening: Int = 0
  var fields: ListBuffer[Field] = new ListBuffer()
  var closing: Int = 0
}
