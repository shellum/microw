package com.finalhack.microw

import scala.collection.mutable

class Parser {

  var queue = mutable.Queue[Token]()
  var next = 0
  var save = 0
  var tokens: List[Token] = _

  /*
     expr ->
           exprCompound|  exprb
           exprNum     |  num
     exprCompound->
           exprCompoundOpNum| operation num
   */
  def term(value: String): Boolean = {
    val ret = tokens(next).`type` == value
    if (ret) {
      queue.enqueue(tokens(next))
    }
    next += 1
    ret
  }

  def exprCompoundNum: Boolean = {
    next = save
    val ret = term(Token.TYPE_OPERATOR) && term(Token.TYPE_NUMBER)
    if (ret) {
      queue.enqueue(Token(Token.DELIMITER))
    }
    ret
  }

  def expr: Boolean = {
      save = next
      exprCompound || expr2 || error
  }

  def exprCompound: Boolean = {
    next = save
    val ret = exprCompoundNum
    ret
  }

  def expr2: Boolean = {
    next = save
    val ret = term(Token.TYPE_NUMBER)
    if (ret) {
      queue.enqueue(Token(Token.DELIMITER))
    }
    ret
  }

  def error: Boolean = {
    next = save
    queue.enqueue(Token(Token.TYPE_ERROR,"error with token #" + next + ": " + tokens(next)))
    queue.enqueue(Token(Token.DELIMITER))
    next += 1
    true
  }

}
