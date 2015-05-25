package com.finalhack.microw

case class AstNode(value: Token = Token(Token.DELIMITER)) {
  var children: List[AstNode] = List()
  def addChild(child: Token) = {
    children = children :+ new AstNode(child)
  }

  def copy: AstNode = {
    val rootCopy = AstNode(value)
    for(child <- children)
      rootCopy.children = rootCopy.children :+ child.copy
    rootCopy
  }

  def traverseToBottomRight: AstNode = {
    var track = this
    while(track.children != Nil) track = track.children(track.children.size - 1)
    track
  }
}
