import com.finalhack.microw.{Input, Lexer, Token}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class LexerSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  var lexer: Lexer = _

  before {
    lexer = new Lexer {
      override val input = new Input {
        override val code = """
          |asdf = asdf + 1
          |asdf+2
          | """.stripMargin
      }
    }
  }

  "A Lexer" should "create an operator token" in {
    val token: Token = lexer.createToken("+")
    token.`type` should be(Token.TYPE_OPERATOR)
    token.value should be("+")
  }

  "A Lexer" should "create an if token" in {
    val token: Token = lexer.createToken("if")
    token.`type` should be(Token.TYPE_IF)
    token.value should be("if")
  }

  "A Lexer" should "create a left parentheses token" in {
    val token: Token = lexer.createToken("(")
    token.`type` should be(Token.TYPE_LEFT_PARENTHESES)
    token.value should be("(")
  }

  "A Lexer" should "create a right parentheses token" in {
    val token: Token = lexer.createToken(")")
    token.`type` should be(Token.TYPE_RIGHT_PARENTHESES)
    token.value should be(")")
  }

  it should "create a variable token" in {
    val token: Token = lexer.createToken("asdf")
    token.`type` should be(Token.TYPE_VARIABLE)
    token.value should be("asdf")
  }

  it should "create an assignment token" in {
    val token: Token = lexer.createToken("=")
    token.`type` should be(Token.TYPE_ASSIGNMENT)
    token.value should be("=")
  }

  it should "create a number token" in {
    val token: Token = lexer.createToken("1")
    token.`type` should be(Token.TYPE_NUMBER)
    token.value should be("1")
  }

  it should "pull out a token from code" in {
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

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_VARIABLE)
    token.value should be("asdf")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_OPERATOR)
    token.value should be("+")

    token = lexer.getNextCodePart.get
    token.`type` should be(Token.TYPE_NUMBER)
    token.value should be("2")

    val ending = lexer.getNextCodePart
    ending should be(None)
  }

  it should "create the correct number of tokens" in {
    val tokens = lexer.getAllTokens()
    tokens.size should be(8)
  }

}
