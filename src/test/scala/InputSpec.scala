import com.finalhack.microw.{Token, Input}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers with MockFactory {

  val testCode = " \n\t  asdf\nasdf2->".replace('\n',' ')

  val inputReader = new Input {
    override val code = testCode
  }

  "Number of lines input" should "match the input count" in {
    inputReader.code.length should be(testCode.length)
  }

  "Consuming each char" should "return each chars" in {
    for ((x,i) <- inputReader.code.zipWithIndex)
      inputReader.consumeCodeChar.get should be(testCode(i))
  }

  "The code pointer" should "skip over whitespace" in {
    inputReader.nextCharIndex = 0
    inputReader.movePastWhitespace
    inputReader.nextCharIndex should be(5)
  }

  "Input" should "understand what character type is next" in {
    inputReader.nextCharIndex = 4
    inputReader.getNextCharType.get should be(Token.TYPE_WHITESPACE)
    inputReader.nextCharIndex += 1
    inputReader.getNextCharType.get should be(Token.TYPE_VARIABLE)
  }

  "Input" should "get the next code part" in {
    inputReader.nextCharIndex = 0
    inputReader.consumeCodePart.get should be("asdf")
  }
  "Input" should "recognize a method start" in {
    inputReader.nextCharIndex = 0
    inputReader.consumeCodePart.get should be("asdf")
    inputReader.consumeCodePart.get should be("asdf2")
    inputReader.consumeCodePart.get should be("->")
  }

}
