package com.finalhack.microw

import scala.collection.mutable

object Parse {
  def main(args: Array[String]) = {
    val p = new Parser()
    while (p.next < p.tokens.length) p.expr
    for(s <- p.queue)
      println(s)
  }

}

class Parser {

  var queue = mutable.Queue[String]()
  var next = 0
  var save = 0
  var tokens = List(
    Token(Token.TYPE_NUMBER,"5"),
    Token(Token.TYPE_OPERATOR,"*"),
    Token(Token.TYPE_NUMBER,"1"),
    Token(Token.TYPE_OPERATOR,"-"),
    Token(Token.TYPE_NUMBER,"8")
  )


  /*
     expr ->
           expr3|  exprb
           expr2|  num
           expr1|  error
     exprb->
           exprb1| operation num
   */
  def term(value: String): Boolean = {
    val ret = tokens(next).`type` == value
    if (ret) queue.enqueue(tokens(next).value)
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
      queue.enqueue("op")
      queue.enqueue("num")
      queue.enqueue("........")
    }
    ret
  }

  def expr: Boolean = {
   // if (next < tokens.length) {
      save = next
      expr3 || expr2 || expr1
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
      queue.enqueue("num")
      queue.enqueue("........")
    }
    ret
  }

  def expr1: Boolean = {
    next = save
    val ret = term(Token.TYPE_ERROR)
    if (ret) {
      queue.enqueue("error")
      queue.enqueue("........")
    }
    ret
  }

}
