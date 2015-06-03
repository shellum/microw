package com.finalhack.microw

import scala.collection.mutable

/*
CFG:
   expr ->
         exprIf            | IF ( expr ) expr
         exprCompoundOpExpr| OPERATOR expr expr
         exprNum           | NUM
         exprId            | ID
 */
class Parser {

  var queue = mutable.Queue[Token]()
  var parseTree = AstNode()
  var parseTreePointer = parseTree
  var next = 0
  var save = 0
  private var tokens: List[Token] = _

  def setTokens(tokens: List[Token]) = {
    this.tokens = changeInfixOperatorsToPrefixOperators(tokens)
  }

  def getTokens = tokens

  def changeInfixOperatorsToPrefixOperators(tokens: List[Token]): List[Token] = {
    var reorganizedTokens: List[Token] = tokens

    tokens.zipWithIndex.foreach { case (token, i) =>
      if (i > 0 && token.`type` == Token.TYPE_OPERATOR) {
        val first = tokens(i-1)
        val second = tokens(i)
        reorganizedTokens = reorganizedTokens.patch(i-1, Seq(second, first), 2)
      }
    }
    reorganizedTokens
  }
  
  def term(value: String): Boolean = {
    var ret = false
    if (next < tokens.size)
      ret = tokens(next).`type` == value
    if (ret) {
      queue.enqueue(tokens(next))
      parseTreePointer.addChild(tokens(next))
    }
    next += 1
    ret
  }

  def pointToLastAddedElement: Boolean = {
    parseTreePointer = parseTreePointer.children(parseTreePointer.children.size - 1)
    true
  }

  def exprIf: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next

    val numChildren = parseTreePointer.children.size
    var ret = term(Token.TYPE_IF)
    val t = ret
    ret = ret && pointToLastAddedElement && term(Token.TYPE_LEFT_PARENTHESES) && expr && term(Token.TYPE_RIGHT_PARENTHESES) && expr
    if (t) parseTreePointer = parseTreePointer.parent // If it was an if, its children have been added, so go back up to the if's parent
    if (!ret) parseTreePointer.children = parseTreePointer.children.take(numChildren)
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def exprCompoundOpExpr: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next

    val numChildren = parseTreePointer.children.size
    var ret = term(Token.TYPE_OPERATOR)
    val t = ret
    ret = ret && pointToLastAddedElement && expr && expr
    if (t) parseTreePointer = parseTreePointer.parent // If it was an if, its children have been added, so go back up to the if's parent
    if (!ret) parseTreePointer.children = parseTreePointer.children.take(numChildren)
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def expr: Boolean = {
    save = next
    addDelimiter
    val ret = exprIf || exprCompoundOpExpr || exprNum || exprId || error
    addDelimiter
    ret
  }

  def exprNum: Boolean = {
    next = save
    val ret = term(Token.TYPE_NUMBER)
    ret
  }

  def exprId: Boolean = {
    next = save
    val ret = term(Token.TYPE_VARIABLE)
    ret
  }

  def error: Boolean = {
    next = save
    queue.enqueue(Token(Token.TYPE_ERROR, "error with token #" + next))
    next += 1
    true
  }

  def rollBack(savedQueueSize: Int, savedTokenIndex: Int) = {
    // Rollback Queue
    while (queue.size > savedQueueSize) {
      queue = queue.reverse
      queue.dequeue()
      queue = queue.reverse
    }
    // Rollback token pointer
    next = savedTokenIndex
    save = savedTokenIndex
  }

  def hasMoreTokens: Boolean = {
    next < tokens.length
  }

  def getProcessedToken: Token = {
    queue.dequeue()
  }

  def addDelimiter = {
    if (queue.size == 0 || queue.reverse.front.`type` != Token.DELIMITER)
      queue.enqueue(Token(Token.DELIMITER))
  }

}
