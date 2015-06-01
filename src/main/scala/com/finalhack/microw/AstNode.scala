package com.finalhack.microw

import scala.collection.mutable

case class AstNode(value: Token = Token(Token.DELIMITER)) {
  var parent: AstNode = null
  var visited = false
  var children: List[AstNode] = List()
  def addChild(child: Token) = {
    children = children :+ new AstNode(child).setParent(this)
  }

  def setParent(parent: AstNode): AstNode = {
    this.parent = parent
    this
  }

  def copy: AstNode = {
    val rootCopy = AstNode(value)
    for(child <- children)
      rootCopy.children = rootCopy.children :+ child.copy.setParent(rootCopy)
    for(child <- rootCopy.children)
      child.parent = rootCopy
    rootCopy
  }

  def traverseToBottomRight: AstNode = {
    var track = this
    while(track.children != Nil) track = track.children(track.children.size - 1)
    track
  }

  def traverseToBottomLeft: AstNode = {
    var track = this
    while(track.children != Nil) track = track.children(0)
    track
  }

  def markAllVisited(visited: Boolean): Unit = {
    this.visited = visited
    for (child <- children)
      child.markAllVisited(visited)
  }

  def getUnvisitedChild: AstNode = {
    var unvisitedNode: AstNode = null
    for (child <- children)
      if (unvisitedNode == null && !child.visited)
        unvisitedNode = child
    unvisitedNode
  }

  def getNextUnvisitedNode: AstNode = {
    var result: AstNode = this
    if (!visited)
      result = this
    else {
      result = getUnvisitedChild
      if (result == null && parent == null)
        result = null
      else if (result == null)
        result = parent.getNextUnvisitedNode
    }
    if (result != null)
      result.visited = true
    result
  }

  def makeStack: mutable.Stack[AstNode] = {
    val stack = mutable.Stack[AstNode]()
    val queue = mutable.Queue[AstNode]()
    queue.enqueue(this)
    while (!queue.isEmpty) {
      val astNode = queue.dequeue()
      stack.push(astNode)
      for(child <- astNode.children)
        queue.enqueue(child)
    }
    stack
  }

}
