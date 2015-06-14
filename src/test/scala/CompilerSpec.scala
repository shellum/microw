import com.finalhack.microw.{Parser, AstNode, Input, Lexer}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class CompilerSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  "A Compiler" should "compile" in {
    val lexer = new Lexer {
      override val input = new Input {
        override val code = """
                              |1+2*3
                              | """.stripMargin.replace('\n',' ')
      }
    }

    val tokenList = lexer.getAllTokens()

    val parser = new Parser()
    parser.setTokens(tokenList)
    while (parser.hasMoreTokens)
      parser.expr

    var stack = parser.parseTree.makeStack

    // TODO: figure out what stack ordering is needed
    // TODO: think about embedded ifs
  }

}
