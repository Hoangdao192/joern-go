package io.joern.gosrc2cpg.ast.nodes

import com.fasterxml.jackson.annotation.{JsonProperty, JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable.ListBuffer

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    visible = true,
    property = "nodeType")
@JsonSubTypes(Array(
    new JsonSubTypes.Type(value = classOf[EmptyNode], name = ""),
    new JsonSubTypes.Type(value = classOf[FileNode], name = "File"),
    new JsonSubTypes.Type(value = classOf[Package], name = "Package"),
    new JsonSubTypes.Type(value = classOf[Field], name = "Field"),
    new JsonSubTypes.Type(value = classOf[FieldList], name = "FieldList"),
))
abstract class Node {
    @JsonProperty("nodeType")
    var nodeType: String = ""
    protected var id: String = ""
    protected var start: Int = 0
    protected var end: Int = 0
    protected val children: ListBuffer[Node] = ListBuffer()
    protected var parent: Option[Node] = None
    @JsonProperty("code")
    protected var _code: String = ""

    def code: String = _code

    def setParent(parent: Node): Unit = {
        this.parent = Option(parent)
    }
    
    def addChild(node: Node): Unit = {
        children.addOne(node)
    }

//    def nodeType: String = _nodeType
}

class EmptyNode extends Node
