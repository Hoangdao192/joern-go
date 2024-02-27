package io.joern.gosrc2cpg.ast.nodes

class Package extends Node("Package") {
  var name: String = ""
  var files: Map[String, FileNode] = Map()
}
