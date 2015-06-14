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
    val a = "[{}]".r
    if (nextCharIndex == code.length)
      None
    else
      (code(nextCharIndex) + "") match {
        case s if s.matches("[{}]") => Option(Token.TYPE_BLOCK)
        case s if s.matches("[(]") => Option(Token.TYPE_LEFT_PARENTHESES)
        case s if s.matches("[)]") => Option(Token.TYPE_RIGHT_PARENTHESES)
        case s if s.matches("[+*-/]") => Option(Token.TYPE_OPERATOR)
        case s if s.matches("=") => Option(Token.TYPE_ASSIGNMENT)
        case s if s.matches("[1234567890]") => Option(Token.TYPE_DIGIT)
        case s if s.matches("[ \n\t]") => Option(Token.TYPE_WHITESPACE)
        case _ => Option(Token.TYPE_VARIABLE)
      }
  }

  def consumeCodePart: Option[String] = {
    movePastWhitespace
    val REGEX_BLOCK = "([{}]+).*".r
    val REGEX_LEFT_PARENTHESES = "([(]+).*".r
    val REGEX_RIGHT_PARENTHESES = "([)]+).*".r
    val REGEX_OPERATOR = "([+*-/]).*".r
    val REGEX_ASSIGNMENT = "([=]).*".r
    val REGEX_DIGIT = "([0-9]+).*".r
    val REGEX_WHITESPACE = "(\\s+).*".r
    val REGEX_VARIABLE = "([a-z]+[a-z0-9]*).*".r
    val REGEX_METHOD_START = "(->).*".r
    val REGEX_ALL = ".*".r

    nextCharIndex < code.length() match {

      case true =>
        code.substring(nextCharIndex) match {
          case REGEX_BLOCK(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_LEFT_PARENTHESES(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_RIGHT_PARENTHESES(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_METHOD_START(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_OPERATOR(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_ASSIGNMENT(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_DIGIT(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_WHITESPACE(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_VARIABLE(str) =>
            nextCharIndex += str.length;
            Option(str)
          case REGEX_ALL(str) =>
            Option("" + str.length())
        }
      case false => None
    }
  }

}

class Input extends InputSpec {
  def code = Source.fromFile("").getLines().mkString
}
