package com.finalhack.microw

case class AstNode(value: Token = Token(Token.DELIMITER)) {
  var parent: AstNode = null
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
}
