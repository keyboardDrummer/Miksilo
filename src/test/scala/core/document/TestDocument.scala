package core.document

import org.junit.{Assert, Test}

class TestDocument {

  val lineSep = System.lineSeparator()

  @Test
  def testEmptyWithExclamation() {
    val expected = "!"
    val text = new LeftRight(Empty, new Text("!"))
    Assert.assertEquals(expected, text.render())
  }

  @Test
  def testText()
  {
    val expected: String = "hallo"
    val text = new Text(expected)
    Assert.assertEquals(expected, text.render())
  }

  @Test
  def testLeftRight()
  {
    val expected = "hallo" + "daar"
    val document = ("hallo": Document) ~ "daar"
    Assert.assertEquals(expected, document.render())
  }

  @Test
  def testTopBottom()
  {
    val expected = "a" + lineSep + "b"
    val document = ("a" : Document) % "b"
    Assert.assertEquals(expected, document.render())
  }

  @Test
  def testLeftRightLeftHigher()
  {
    val expected = "ab" + lineSep + "a"
    val document = (new Text("a") % "a") ~ "b"
    Assert.assertEquals(expected, document.render())
  }

  @Test
  def testLeftRightRightHigher()
  {
    val expected = "ab" + lineSep + " b"
    val document = new Text("a") ~ (new Text("b") % "b")
    Assert.assertEquals(expected, document.render())
  }

  @Test
  def testTopBottomTopWider()
  {
    val expected = "aa" + lineSep + "b"
    val document = new Text("aa") % "b"
    Assert.assertEquals(expected, document.render())
  }

  @Test
  def testTopBottomBottomWider()
  {
    val expected = "a" + lineSep + "bb"
    val document = new Text("a") % "bb"
    Assert.assertEquals(expected, document.render())
  }
}
