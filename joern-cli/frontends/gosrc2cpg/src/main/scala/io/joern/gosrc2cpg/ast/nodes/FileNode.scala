package io.joern.gosrc2cpg.ast.nodes

import com.fasterxml.jackson.annotation.JsonProperty

import scala.collection.mutable.ListBuffer


class FileNode extends Node {
  var documentation: Option[CommentGroup] = None
  @JsonProperty("package")
  var packagePosition: Int = 0
  var packagePath: String = ""
  var name: Option[Identifier] = None
  var declarations: ListBuffer[Declaration] = new ListBuffer[Declaration]
  var fileStart: Int = 0
  var fileEnd: Int = 0
  var imports: ListBuffer[ImportSpecification] = new ListBuffer()
  var unresolved: ListBuffer[Identifier] = new ListBuffer()
  var comments: ListBuffer[CommentGroup] = new ListBuffer()
  var goVersion: String = ""
  var filePath: String = ""
}
