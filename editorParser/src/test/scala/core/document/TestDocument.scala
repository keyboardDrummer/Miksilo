package core.document

import org.scalatest.FunSuite

class TestDocument extends FunSuite {

  val lineSep = System.lineSeparator()

  test("EmptyWithExclamation") {
    val expected = "!"
    val text = new LeftRight(Empty, new Text("!"))
    assertResult(expected)(text.render())
  }

  test("Text")
  {
    val expected: String = "hallo"
    val text = new Text(expected)
    assertResult(expected)(text.render())
  }

  test("LeftRight")
  {
    val expected = "hallo" + "daar"
    val document = ("hallo": Document) ~ "daar"
    assertResult(expected)(document.render())
  }

  test("TopBottom")
  {
    val expected = "a" + lineSep + "b"
    val document = ("a" : Document) % "b"
    assertResult(expected)(document.render())
  }

  test("LeftRightLeftHigher")
  {
    val expected = "ab" + lineSep + "a"
    val document = (new Text("a") % "a") ~ "b"
    assertResult(expected)(document.render())
  }

  test("LeftRightRightHigher")
  {
    val expected = "ab" + lineSep + " b"
    val document = new Text("a") ~ (new Text("b") % "b")
    assertResult(expected)(document.render())
  }

  test("TopBottomTopWider")
  {
    val expected = "aa" + lineSep + "b"
    val document = new Text("aa") % "b"
    assertResult(expected)(document.render())
  }

  test("TopBottomBottomWider")
  {
    val expected = "a" + lineSep + "bb"
    val document = new Text("a") % "bb"
    assertResult(expected)(document.render())
  }
}
