package io.joern.gosrccpg.ast.nodes

import scala.collection.mutable.ListBuffer

class Comment extends Node("Comment") {
  var slash: Int = 0
  var text: String = ""
}

class CommentGroup extends Node("CommentGroup") {
  var comments: ListBuffer[Comment] = new ListBuffer()
}
