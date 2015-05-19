package com.finalhack.microw

import scala.io.Source

trait InputSpec {
  def code: String
  var nextCharIndex = 0

  def consumeCodeChar = {
    val nextChar = code(nextCharIndex)
    nextCharIndex += 1
    nextChar
  }

}

class Input extends InputSpec {
  def code = Source.fromFile("").getLines().mkString
}
