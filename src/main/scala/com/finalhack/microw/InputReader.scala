package com.finalhack.microw

import scala.io.Source

trait InputSpec {
  def code: String
}

class Input extends InputSpec {
  def code = Source.fromFile("").getLines().mkString
}
