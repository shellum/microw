package com.finalhack.microw

import scala.collection.mutable

object Parse {
  def main(args: Array[String]) = {
    val p = new Parser()
    while (p.next < p.tokens.length) p.expr
    for(s <- p.queue)
      println(s.`type` + " " + s.value)
  }

}

class Parser {

  var queue = mutable.Queue[Token]()
  var next = 0
  var save = 0
  var tokens = List(
    Token(Token.TYPE_VARIABLE,"i"),
    Token(Token.TYPE_OPERATOR,"*"),
    Token(Token.TYPE_NUMBER,"1"),
    Token(Token.TYPE_OPERATOR,"-"),
    Token(Token.TYPE_NUMBER,"8")
  )


  /*
     expr ->
           expr3|  exprb
           expr2|  num
           error|  error
     exprb->
           exprb1| operation num
   */
  def term(value: String): Boolean = {
    val ret = tokens(next).`type` == value
    if (ret) {
      queue.enqueue(tokens(next))
    }
    next += 1
    ret
  }

  def exprb: Boolean = {
    save = next
    exprb1
  }

  def exprb1: Boolean = {
    next = save
    val ret = term(Token.TYPE_OPERATOR) && term(Token.TYPE_NUMBER)
    if (ret) {
      queue.enqueue(Token(Token.DELIMITER,"....."))
    }
    ret
  }

  def expr: Boolean = {
   // if (next < tokens.length) {
      save = next
      expr3 || expr2 || error
   //   true
   // }
   // else
   //   false
  }

  def expr3: Boolean = {
    next = save
    val ret = exprb
    ret
  }

  def expr2: Boolean = {
    next = save
    val ret = term(Token.TYPE_NUMBER)
    if (ret) {
      queue.enqueue(Token(Token.DELIMITER,"....."))
    }
    ret
  }

  def error: Boolean = {
    next = save
    queue.enqueue(Token(Token.TYPE_ERROR,"error with token #" + next + ": " + tokens(next)))
    queue.enqueue(Token(Token.DELIMITER,"....."))
    next += 1
    true
  }

}
