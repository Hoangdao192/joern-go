package io.joern.gosrccpg.ast.nodes

import com.fasterxml.jackson.annotation.JsonProperty

abstract class Node(val nodeType: String) {
  protected var start: Int = 0
  protected var end: Int = 0
  protected val children: List[Node] = List()
  @JsonProperty("code")
  protected var _code: String = ""

  def code: String = _code
}
