package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable.ListBuffer

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "nodeType")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[ImportSpecification], name = "ImportSpecification"),
  new JsonSubTypes.Type(value = classOf[TypeSpecification], name = "TypeSpecification"),
  new JsonSubTypes.Type(value = classOf[ValueSpecification], name = "ValueSpecification")
))
abstract class Specification extends Node("Specification")

class ImportSpecification extends Specification {
  var documentation: Option[CommentGroup] = None
  var name: Option[Identifier] = None
  var path: Option[BasicLiteralExpression] = None
  var comment: Option[CommentGroup] = None
  var endPosition: Int = 0
}

class TypeSpecification extends Specification {
  var documentation: Option[CommentGroup] = None
  var name: Option[Identifier] = None
  var typeParams: Option[FieldList] = None
  var assign: Int = 0
  var typeExpression: Option[Expression] = None
  var comment: Option[CommentGroup] = None
}

class ValueSpecification extends Specification {
  var documentation: Option[CommentGroup] = None
  var names: ListBuffer[Identifier] = new ListBuffer()
  var typeExpression: Option[Expression] = None
  var values: ListBuffer[Expression] = new ListBuffer()
  var comment: Option[CommentGroup] = None
}


