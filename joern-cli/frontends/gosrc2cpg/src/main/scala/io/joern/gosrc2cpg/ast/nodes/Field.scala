package io.joern.gosrc2cpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonProperty, JsonSetter, Nulls}

import scala.collection.mutable.ListBuffer

class Field extends Node  {
  var documentation: Option[CommentGroup] = None
  @JsonSetter(nulls = Nulls.SKIP)
  var names: ListBuffer[Identifier] = new ListBuffer()
  @JsonProperty("type")
  var typeExpression: Option[Expression] = None
  var tag: Option[BasicLiteralExpression] = None
  var comment: Option[CommentGroup] = None
  @JsonSetter(nulls = Nulls.SKIP)
  var typeFullNames: ListBuffer[String] = ListBuffer()
}

class FieldList extends Node {
  var opening: Int = 0
  @JsonProperty("list")
  var fields: ListBuffer[Field] = new ListBuffer()
  var closing: Int = 0
}
