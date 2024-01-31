package io.joern.gosrccpg.ast.nodes

import scala.collection.mutable.ListBuffer

abstract class Node(val nodeType: String) {
  protected var start: Int = 0
  protected var end: Int = 0
  protected val children: List[Node] = List()
}
