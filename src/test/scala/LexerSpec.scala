import com.finalhack.microw.{Input, Lexer, Token}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class LexerSpec extends FlatSpec with Matchers with MockFactory {

  val lexer = new Lexer {
    override val input = new Input {
      override val code = "asdf = asdf + 1"
    }
  }

  "A Lexer" should "create an operator token" in {
    val token: Token = lexer.createToken("+")
    token.`type` should be(Token.TYPE_OPERATOR)
    token.value should be("+")
  }

  "A Lexer" should "create a variable token" in {
    val token: Token = lexer.createToken("asdf")
    token.`type` should be(Token.TYPE_VARIABLE)
    token.value should be("asdf")
  }

  "A Lexer" should "create an assignment token" in {
    val token: Token = lexer.createToken("=")
    token.`type` should be(Token.TYPE_ASSIGNMENT)
    token.value should be("=")
  }

  "A Lexer" should "create a number token" in {
    val token: Token = lexer.createToken("1")
    token.`type` should be(Token.TYPE_NUMBER)
    token.value should be("1")
  }

  "A Lexer" should "pull out a token from code" in {
    var token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_VARIABLE)
    token.value should be("asdf")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_ASSIGNMENT)
    token.value should be("=")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_VARIABLE)
    token.value should be("asdf")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_OPERATOR)
    token.value should be("+")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_NUMBER)
    token.value should be("1")
  }

}
