package com.finalhack.microw

case class Token(`type`: String, value: Object)

trait LexerSpec {
  val input = new Input
}

class Lexer extends LexerSpec {

  def getTokenType(codePart: String): String = {
    codePart match {
      case "+" => "operator"
      case "=" => "assignment"
      case "1" => "number"
      case _ => "variable"
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

}
