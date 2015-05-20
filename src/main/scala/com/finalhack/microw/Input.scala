package com.finalhack.microw

import scala.io.Source

trait InputSpec {
  def code: String

  var nextCharIndex = 0

  def consumeCodeChar: Option[Char] = {
    if (nextCharIndex == code.length) {
      None
    }
    else {
      val nextChar = code(nextCharIndex)
      nextCharIndex += 1
      Option(nextChar)
    }
  }

  def movePastWhitespace = {
    val whitespace = Array(' ', '\n', '\t')
    while (nextCharIndex < code.length && whitespace.contains(code(nextCharIndex))) nextCharIndex += 1
  }

  def getNextCharType: Option[String] = {
    if (nextCharIndex == code.length)
      None
    else
      code(nextCharIndex) match {
        case '{' | '}' => Option(Token.TYPE_BLOCK)
        case '+' | '*' | '-' | '/' => Option(Token.TYPE_OPERATOR)
        case '=' => Option(Token.TYPE_ASSIGNMENT)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => Option(Token.TYPE_DIGIT)
        case ' ' | '\n' | '\t' => Option(Token.TYPE_WHITESPACE)
        case _ => Option(Token.TYPE_VARIABLE)
      }
  }

  def consumeCodePart: Option[String] = {
    movePastWhitespace

    val codePartType = getNextCharType

    codePartType match {
      case None => None
      case Some(codePartTypeValue) =>
        var codePart = ""
        while (getNextCharType.isDefined && getNextCharType.get == codePartTypeValue) codePart += consumeCodeChar.get
        Option(codePart)
    }
  }

}

class Input extends InputSpec {
  def code = Source.fromFile("").getLines().mkString
}
