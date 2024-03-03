package io.joern.gosrc2cpg.ast.nodes

class Package extends Node {
  var name: String = ""
  var files: Map[String, FileNode] = Map()
}
