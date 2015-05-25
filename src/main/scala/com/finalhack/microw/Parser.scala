package com.finalhack.microw

import scala.collection.mutable


class Parser {

  var queue = mutable.Queue[Token]()
  var next = 0
  var save = 0
  var tokens: List[Token] = _

  /*
  CFG:
     expr ->
           exprIf            | IF ( expr ) expr
           exprCompoundNum   | NUM exprCompound  if multiple ops, next=save else save = next
           exprCompoundId    | ID exprCompound
           exprNum           | NUM
           exprId            | ID
     exprCompound->
           exprCompoundOpExpr| OPERATION expr
   */
  def term(value: String): Boolean = {
    var ret = false
    if (next < tokens.size)
      ret = tokens(next).`type` == value
    if (ret) {
      queue.enqueue(tokens(next))
    }
    next += 1
    ret
  }

  def exprIf: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next
    val ret = term(Token.TYPE_IF) && term(Token.TYPE_LEFT_PARENTHESES) && expr && term(Token.TYPE_RIGHT_PARENTHESES) && expr
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def exprCompoundOpExpr: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next
    val ret = term(Token.TYPE_OPERATOR) && expr
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def exprCompoundNum: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next
    val ret = term(Token.TYPE_NUMBER) && exprCompoundOpExpr
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def exprCompoundId: Boolean = {
    val localSave = save
    val savedQueueSize = queue.size
    save = next
    val ret = term(Token.TYPE_VARIABLE) && exprCompoundOpExpr
    if (!ret) rollBack(savedQueueSize, localSave)
    // Delimiter handled recursively
    ret
  }

  def expr: Boolean = {
    save = next
    addDelimiter
    val ret = exprIf || exprCompoundNum || exprCompoundId || exprNum || exprId || error
    addDelimiter
    ret
  }

  def exprCompound: Boolean = {
    next = save
    val ret = exprCompoundOpExpr
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
    queue.enqueue(Token(Token.TYPE_ERROR, "error with token #" + next + ": " + tokens(next)))
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
