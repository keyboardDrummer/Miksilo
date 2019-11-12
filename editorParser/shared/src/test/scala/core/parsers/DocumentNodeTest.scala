package core.parsers

import org.scalatest.FunSuite
import org.scalatest.funsuite.AnyFunSuite

class DocumentNodeTest extends AnyFunSuite {

  val tree = NoNode.
    insertValue(2,4,"four").
    insertValue(1,3, "lll").
    insertValue(5,2,"rr")

  test("insert before everything") {
    val result = tree.insertText(0,2)
    val expectation = NoNode.
      insertValue(4,4,"four").
      insertValue(3,3, "lll").
      insertValue(7,2,"rr")
    assertResult(expectation)(result)
  }

  test("insert inside left") {
    val result = tree.insertText(2,1)
    val expectation = NoNode.
      insertValue(3,4,"four").
      insertValue(6,2,"rr")
    assertResult(expectation)(result)
  }

  test("insert inside left and middle") {
    val result = tree.insertText(3,1)
    val expectation = NoNode.
      insertValue(6,2,"rr")
    assertResult(expectation)(result)
  }

  test("insert value at 0") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(0,4,"four")
    assertResult(ParentNode(0, 4,"four", NoNode, NoNode))(tree)
  }

  test("insert value at 2") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(2,4,"four")
    assertResult(ParentNode(2, 4,"four", NoNode, NoNode))(tree)
  }

  test("insert node after node") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(2,4,"four")
    tree = tree.insertValue(6,1,"x")
    assertResult(ParentNode(2, 4, "four", NoNode, ParentNode(4, 1, "x", NoNode, NoNode)))(tree)
  }

  test("insert value before node") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(2,4,"four")
    tree = tree.insertText(1, 1)
    assertResult(ParentNode(3, 4, "four", NoNode, NoNode))(tree)
  }

  test("insert value after node") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(2,4,"four")
    tree = tree.insertText(6, 1)
    assertResult(ParentNode(2, 4, "four", NoNode, NoNode))(tree)
  }

  test("insert value in node") {
    var tree: DocumentNode = NoNode
    tree = tree.insertValue(2,4,"four")
    tree = tree.insertText(3, 1)
    assertResult(NoNode)(tree)
  }
}
