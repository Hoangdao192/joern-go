package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "nodeType"
)
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[FunctionType], name = "FunctionType"),
  new JsonSubTypes.Type(value = classOf[ArrayType], name = "ArrayType"),
  new JsonSubTypes.Type(value = classOf[ChanelType], name = "ChanelType"),
  new JsonSubTypes.Type(value = classOf[InterfaceType], name = "InterfaceType"),
  new JsonSubTypes.Type(value = classOf[MapType], name = "MapType"),
  new JsonSubTypes.Type(value = classOf[StructType], name = "StructType")
))
abstract class Type extends Node("Type")

class FunctionType extends Type {
  var function: Int = 0
  var typeParams: Option[FieldList] = None
  var params: Option[FieldList] = None
  var results: Option[FieldList] = None
}

class ArrayType extends Type {
  var leftBracket: Int = 0
  var length: Option[Expression] = None
  var element: Option[Expression] = None
}

class ChanelType extends Type {
  var begin: Int = 0
  var arrow: Int = 0
  var direction: Int = 0
  var value: Option[Expression] = None
}

class InterfaceType extends Type {
  var interface: Int = 0
  var methods: Option[FieldList] = None
  var incomplete: Boolean = false
}

class MapType extends Type {
  var map: Int = 0
  var key: Option[Expression] = None
  var value: Option[Expression] = None
}

class StructType extends Type {
  var struct: Int = 0
  var fields: Option[FieldList] = None
  var incomplete: Boolean = false
}
