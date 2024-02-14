package io.joern.gosrccpg.ast.nodes

abstract class Node(val nodeType: String) {
  protected var start: Int = 0
  protected var end: Int = 0
  protected val children: List[Node] = List()
  protected val code: String = ""
}
