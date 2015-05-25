import com.finalhack.microw.{AstNode, Parser, Token}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.collection.mutable

class AstNodeSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  "An AstNode" should "travers to the bottom right" in {
    val root = AstNode()
    root.addChild(Token(Token.TYPE_DIGIT,"1"))
    root.addChild(Token(Token.TYPE_DIGIT,"2"))
    root.addChild(Token(Token.TYPE_DIGIT,"3"))

    root.children(1).addChild(Token(Token.TYPE_DIGIT,"4"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT,"5"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT,"6"))

    root.children(2).addChild(Token(Token.TYPE_DIGIT,"7"))

    root.traverseToBottomRight.value should be(Token(Token.TYPE_DIGIT,"7"))
  }

  "An AstNode" should "copy itself" in {
    val root = AstNode()
    root.addChild(Token(Token.TYPE_DIGIT,"1"))
    root.addChild(Token(Token.TYPE_DIGIT,"2"))
    root.addChild(Token(Token.TYPE_DIGIT,"3"))

    root.children(1).addChild(Token(Token.TYPE_DIGIT,"4"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT,"5"))
    root.children(1).addChild(Token(Token.TYPE_DIGIT,"6"))

    root.children(1).children(2).addChild(Token(Token.TYPE_DIGIT,"7"))

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
    rootCopy.children(2).children should be(root.children(2).children)

    rootCopy.children should be(root.children)
  }

}
