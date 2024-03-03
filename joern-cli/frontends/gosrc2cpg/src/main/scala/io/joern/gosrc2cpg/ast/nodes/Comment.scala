package io.joern.gosrc2cpg.ast.nodes

import scala.collection.mutable.ListBuffer

class Comment extends Node {
  var slash: Int = 0
  var text: String = ""
}

class CommentGroup extends Node {
  var comments: ListBuffer[Comment] = new ListBuffer()
}
