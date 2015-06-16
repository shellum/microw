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

  var set = Set[String]()

  // TODO: Change from DFS to BFS
  def makeStack: mutable.Stack[AstNode] = {
    val traversalStack = mutable.Stack[AstNode]()
    val stack = mutable.Stack[AstNode]()
    var node = this
    traversalStack.push(node)
    while (!traversalStack.isEmpty) {
      node = traversalStack.pop()
      if (!node.visited) {
        node.visited = true
        stack.push(node)
        for(child <- node.children.reverse)
          traversalStack.push(child)
      }

    }
    stack
  }

}
