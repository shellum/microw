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

    while (p.next < p.tokens.length) p.expr
    p.queue.dequeue() should be(Token(Token.TYPE_NUMBER,"5"))
    p.queue.dequeue() should be(Token(Token.TYPE_OPERATOR,"*"))
    p.queue.dequeue() should be(Token(Token.TYPE_NUMBER,"1"))
    p.queue.dequeue() should be(Token(Token.TYPE_OPERATOR,"-"))
    p.queue.dequeue() should be(Token(Token.TYPE_NUMBER,"8"))
    p.queue.dequeue() should be(Token(Token.DELIMITER))
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

    while (p.next < p.tokens.length) p.expr
    p.queue.dequeue().`type` should be(Token.TYPE_ERROR)
    p.queue.dequeue() should be(Token(Token.DELIMITER))
    p.queue.dequeue().`type` should be(Token.TYPE_ERROR)
    p.queue.dequeue() should be(Token(Token.DELIMITER))
    p.queue.dequeue() should be(Token(Token.TYPE_NUMBER,"1"))
    p.queue.dequeue() should be(Token(Token.TYPE_OPERATOR,"-"))
    p.queue.dequeue() should be(Token(Token.TYPE_NUMBER,"8"))
    p.queue.dequeue() should be(Token(Token.DELIMITER))
  }
}
