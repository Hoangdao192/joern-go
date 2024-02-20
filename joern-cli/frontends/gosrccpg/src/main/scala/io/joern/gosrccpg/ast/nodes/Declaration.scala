package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonProperty, JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable.ListBuffer

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "nodeType")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[FunctionDeclaration], name = "FunctionDeclaration"),
  new JsonSubTypes.Type(value = classOf[GenericDeclaration], name = "GenericDeclaration")
))
abstract class Declaration extends Node("Declaration")

class BadDeclaration extends Declaration {
  var from: Int = 0
  var to: Int = 0
}

class FunctionDeclaration extends Declaration {
  var documentation: Option[CommentGroup] = None
  var receiver: Option[FieldList] = None
  var name: Option[Identifier] = None
  @JsonProperty("type")
  var functionType: Option[FunctionType] = None
  var body: Option[BlockStatement] = None
}

class GenericDeclaration extends Declaration {
  var documentation: Option[CommentGroup] = None
  var tokenPosition: Int = 0
  var token: Int = 0
  var lparen: Int = 0
  var rparen: Int = 0
  var specifications: ListBuffer[Specification] = new ListBuffer()
}


