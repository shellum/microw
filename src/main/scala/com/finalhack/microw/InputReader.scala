package com.finalhack.microw

import scala.io.Source

// TODO: rename file to Input
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
    while (whitespace.contains(code(nextCharIndex))) nextCharIndex += 1
  }

  def getNextCharType: Option[String] = {
    if (nextCharIndex == code.length)
      None
    else
      code(nextCharIndex) match {
        case '{' | '}' => Option("block")
        case '+' | '*' | '-' | '/' => Option("operator")
        case '=' => Option("assignment")
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => Option("digit")
        case ' ' | '\n' | '\t' => Option("whitespace")
        case _ => Option("variable")
      }
  }

  def consumeCodePart: Option[String] = {
    // Fast forward through whitespace
    movePastWhitespace

    val codePartType = getNextCharType

    codePartType match {
      case None => None
      case Some(codePartTypeValue) =>
        // Remember all characters of a type
        var codePart = ""
        while (getNextCharType.isDefined && getNextCharType.get == codePartTypeValue) codePart += consumeCodeChar.get
        Option(codePart)
    }
  }

}

class Input extends InputSpec {
  def code = Source.fromFile("").getLines().mkString
}
