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
  val TYPE_IF = "if"
  val TYPE_METHOD_OPEN = "method open"
  val TYPE_METHOD_CLOSE = "method close"
  val TYPE_LEFT_PARENTHESES = "("
  val TYPE_RIGHT_PARENTHESES = ")"
}