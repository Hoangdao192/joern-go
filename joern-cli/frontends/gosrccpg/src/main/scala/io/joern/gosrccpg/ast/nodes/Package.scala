package io.joern.gosrccpg.ast.nodes

class Package extends Node("Package") {
  var name: String = ""
  var files: Map[String, FileNode] = Map()
}
