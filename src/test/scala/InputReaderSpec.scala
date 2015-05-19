import com.finalhack.microw.Input
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class InputReaderSpec extends FlatSpec with Matchers with MockFactory {

  val testCode = "asdf\nasdf2"

  val inputReader = new Input {
    override def code = testCode
  }

  "Number of lines input" should "match the input count" in {
    inputReader.code.length should be(testCode.length)
  }

  "Consuming each char" should "return each chars" in {
    for ((x,i) <- inputReader.code.zipWithIndex)
      inputReader.consumeCodeChar should be(testCode(i))
  }

}
