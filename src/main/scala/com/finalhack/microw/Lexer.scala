package com.finalhack.microw

trait LexerSpec {
  val input = new Input
}

class Lexer extends LexerSpec {

  def getTokenType(codePart: String): String = {
    val REGEX_NUMBER = "[0-9]+".r
    codePart match {
      case "+" | "*" | "-" | "/" => Token.TYPE_OPERATOR
      case "if" => Token.TYPE_IF
      case "(" => Token.TYPE_LEFT_PARENTHESES
      case ")" => Token.TYPE_RIGHT_PARENTHESES
      case "=" => Token.TYPE_ASSIGNMENT
      case REGEX_NUMBER() => Token.TYPE_NUMBER
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
