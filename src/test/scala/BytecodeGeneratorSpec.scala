import com.finalhack.microw._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.collection.mutable

class BytecodeGeneratorSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  "A BytecodeGenerator" should "generate opcodes from a stack" in {
    val numberTokenA = AstNode(Token(Token.TYPE_NUMBER, "3"))
    val numberTokenB = AstNode(Token(Token.TYPE_NUMBER, "4"))
    val addToken = AstNode(Token(Token.TYPE_OPERATOR, "*"))
    val stack = mutable.Stack(numberTokenA, numberTokenB, addToken)
    BytecodeGenerator.generateOpCodes(stack) should be(Array(0x10, 0x03, 0x10, 0x04, 104))
  }

  "A BytecodeGenerator" should "compile to opcodes" in {
    val bytecode = BytecodeGenerator.compile("4*3")
    bytecode should be(Array(0x10, 0x03, 0x10, 0x04, 104))
  }

}
