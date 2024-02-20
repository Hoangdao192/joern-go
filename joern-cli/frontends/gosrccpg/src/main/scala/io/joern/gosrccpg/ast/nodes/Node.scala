package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonProperty, JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "nodeType")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[EmptyNode], name = ""),
  new JsonSubTypes.Type(value = classOf[FileNode], name = "File"),
  new JsonSubTypes.Type(value = classOf[Package], name = "Package"),
  new JsonSubTypes.Type(value = classOf[Field], name = "Field"),
  new JsonSubTypes.Type(value = classOf[FieldList], name = "FieldList"),
))
abstract class Node(val nodeType: String) {
  protected var start: Int = 0
  protected var end: Int = 0
  protected val children: List[Node] = List()
  @JsonProperty("code")
  protected var _code: String = ""

  def code: String = _code
}

class EmptyNode extends Node("")
