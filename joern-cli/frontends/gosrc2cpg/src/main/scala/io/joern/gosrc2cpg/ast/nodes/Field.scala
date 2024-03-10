package io.joern.gosrc2cpg.ast.nodes

import com.fasterxml.jackson.annotation.JsonProperty

import scala.collection.mutable.ListBuffer

class Field extends Node  {
  var documentation: Option[CommentGroup] = None
  var names: ListBuffer[Identifier] = new ListBuffer()
  var typeExpression: Option[Expression] = None
  var tag: Option[BasicLiteralExpression] = None
  var comment: Option[CommentGroup] = None
}

class FieldList extends Node {
  var opening: Int = 0
  @JsonProperty("list")
  var fields: ListBuffer[Field] = new ListBuffer()
  var closing: Int = 0
}
