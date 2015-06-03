import com.finalhack.microw.{AstNode, Token}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class AstNodeSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  def setupAst: AstNode = {
    //           ...
    //          / | \
    //         /  |  \
    //        /   |   \
    //       1    2    3
    //      /    /|\    \
    //     4    5 6 7    9
    //              |
    //              8
    val root = AstNode()
    root.addChild(Token(Token.TYPE_DIGIT, "1"))
    root.addChild(Token(Token.TYPE_DIGIT, "2"))
    root.addChild(Token(Token.TYPE_DIGIT, "3"))
    root.children(0).addChild(Token(Token.TYPE_DIGIT, "4"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT, "5"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT, "6"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT, "7"))
    root.children(1).children(2).addChild(Token(Token.TYPE_DIGIT, "8"))
    root.children(2).addChild(Token(Token.TYPE_DIGIT, "9"))
    root
  }

  "An AstNode" should "travers to the bottom right" in {
    val root = setupAst
    root.traverseToBottomRight.value should be(Token(Token.TYPE_DIGIT, "9"))
  }

  "An AstNode" should "travers to the bottom left" in {
    val root = setupAst
    root.traverseToBottomLeft.value should be(Token(Token.TYPE_DIGIT, "4"))
  }

  "An AstNode" should "copy itself" in {
    val root = setupAst

    val rootCopy = root.copy

    rootCopy should be(root)
    rootCopy.children should be(root.children)
    rootCopy.children(0).parent should be(rootCopy)
    rootCopy.children(0) should be(root.children(0))
    rootCopy.children(1) should be(root.children(1))
    rootCopy.children(2) should be(root.children(2))
    rootCopy.children(0).children should be(root.children(0).children)
    rootCopy.children(1).children(0).parent should be(rootCopy.children(1))
    rootCopy.children(1).children should be(root.children(1).children)
    rootCopy.children(1).children(2).children should be(root.children(1).children(2).children)
    rootCopy.children(2).children should be(root.children(2).children)

    rootCopy.children should be(root.children)
  }

  "An AstNode" should "make a pseudo code stack" in {
    val root = setupAst
    val stack = root.makeStack

    stack.pop() should be(root.children(1).children(2).children(0))
    stack.pop() should be(root.children(2).children(0))
    stack.pop() should be(root.children(1).children(2))
    stack.pop() should be(root.children(1).children(1))
    stack.pop() should be(root.children(1).children(0))
    stack.pop() should be(root.children(0).children(0))
    stack.pop() should be(root.children(2))
    stack.pop() should be(root.children(1))
    stack.pop() should be(root.children(0))
    stack.pop() should be(root)
  }
}
