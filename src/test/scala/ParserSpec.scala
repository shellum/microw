import com.finalhack.microw.{Parser, Token}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.collection.mutable

class ParserSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  var queue: mutable.Queue[String] = _
  var next: Int = _
  var save: Int = _
  var tokens: List[Token] = _

  before {
    queue = mutable.Queue[String]()
    next = 0
    save = 0
  }

  "A Parser" should "parse simple math" in {
    val p = new Parser()

    p.tokens = List(
      Token(Token.TYPE_NUMBER, "5"),
      Token(Token.TYPE_OPERATOR, "*"),
      Token(Token.TYPE_NUMBER, "1"),
      Token(Token.TYPE_OPERATOR, "-"),
      Token(Token.TYPE_NUMBER, "8")
    )

    while (p.hasMoreTokens) p.expr

    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_NUMBER,"5"))
    p.getProcessedToken should be(Token(Token.TYPE_OPERATOR,"*"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_NUMBER,"1"))
    p.getProcessedToken should be(Token(Token.TYPE_OPERATOR,"-"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_NUMBER,"8"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
  }

  "A Parser" should "understand errors" in {
    val p = new Parser()

    p.tokens = List(
      Token(Token.TYPE_OPERATOR, "*"),
      Token(Token.TYPE_OPERATOR, "*"),
      Token(Token.TYPE_NUMBER, "1"),
      Token(Token.TYPE_OPERATOR, "-"),
      Token(Token.TYPE_NUMBER, "8")
    )

    while (p.hasMoreTokens) p.expr

    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken.`type` should be(Token.TYPE_ERROR)
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken.`type` should be(Token.TYPE_ERROR)
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_NUMBER,"1"))
    p.getProcessedToken should be(Token(Token.TYPE_OPERATOR,"-"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_NUMBER,"8"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
  }

  "A Parser" should "understand if" in {
    val p = new Parser()

    p.tokens = List(
      Token(Token.TYPE_IF, "if"),
      Token(Token.TYPE_LEFT_PARENTHESES, "("),
      Token(Token.TYPE_VARIABLE, "a"),
      Token(Token.TYPE_RIGHT_PARENTHESES, ")"),
      Token(Token.TYPE_VARIABLE, "b")
    )

    while (p.hasMoreTokens) p.expr

    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken.`type` should be(Token.TYPE_IF)
    p.getProcessedToken.`type` should be(Token.TYPE_LEFT_PARENTHESES)
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_VARIABLE,"a"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken.`type` should be(Token.TYPE_RIGHT_PARENTHESES)
    p.getProcessedToken should be(Token(Token.DELIMITER))
    p.getProcessedToken should be(Token(Token.TYPE_VARIABLE,"b"))
    p.getProcessedToken should be(Token(Token.DELIMITER))
  }

  "A Parser" should "build a parse tree for if" in {
    val p = new Parser()

    p.tokens = List(
      Token(Token.TYPE_IF, "if"),
      Token(Token.TYPE_LEFT_PARENTHESES, "("),
      Token(Token.TYPE_VARIABLE, "a"),
      Token(Token.TYPE_RIGHT_PARENTHESES, ")"),
      Token(Token.TYPE_VARIABLE, "b")
    )

    while (p.hasMoreTokens) p.expr

    p.parseTree.children.size should be(1)
    p.parseTree.children(0).value.value should be("if")
    p.parseTree.children(0).children.size should be(4)
    p.parseTree.children(0).children(0).value.value should be("(")
    p.parseTree.children(0).children(1).value.value should be("a")
    p.parseTree.children(0).children(2).value.value should be(")")
    p.parseTree.children(0).children(3).value.value should be("b")
  }

}
