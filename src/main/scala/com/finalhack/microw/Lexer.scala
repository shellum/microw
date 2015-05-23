package com.finalhack.microw

case class Token(`type`: String, value: String = "")
object Token {
  val TYPE_EXPRESSION = "expression"
  val TYPE_OPERATOR = "operator"
  val TYPE_ASSIGNMENT = "assignment"
  val TYPE_NUMBER = "number"
  val TYPE_VARIABLE = "variable"
  val TYPE_BLOCK = "block"
  val TYPE_WHITESPACE = "whitespace"
  val TYPE_DIGIT = "digit"
  val TYPE_ERROR = "error"
  val DELIMITER = "....."
}

trait LexerSpec {
  val input = new Input
}

class Lexer extends LexerSpec {

  def getTokenType(codePart: String): String = {
    codePart match {
      case "+" | "*" | "-" | "/" | "++" | "--" => Token.TYPE_OPERATOR
      case "=" => Token.TYPE_ASSIGNMENT
      case "1" => Token.TYPE_NUMBER
      case _ => Token.TYPE_VARIABLE
    }
  }

  def createToken(codePart: String): Token = {
    Token(getTokenType(codePart), codePart)
  }

  def getNextCodePart: Option[Token] = {
    val codePart = input.consumeCodePart
    codePart match {
      case Some(codePart) => Option(createToken(codePart))
      case _ => None
    }
  }

  def getAllTokens(tokenList: List[Token] = List[Token]()): List[Token] = {
    input.consumeCodePart match {
      case Some(codePart) => getAllTokens(tokenList ++ List(createToken(codePart)))
      case None => tokenList
    }
  }

}
