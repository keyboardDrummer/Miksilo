package core.responsiveDocument

import core.responsiveDocument.ResponsiveDocument._
import org.junit.{Assert, Test}

class TestResponsiveDocument {

  val newLine = System.lineSeparator()

  @Test
  def testText()
  {
    val expected: String = "hallo"
    val document: ResponsiveDocument = text(expected)
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testLeftRight()
  {
    val expected = "hallo" + "daar"
    val document = ("hallo": ResponsiveDocument) ~ "daar"
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testTopBottom()
  {
    val expected = "a" + newLine + "b"
    val document = ("a" : ResponsiveDocument) % "b"
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testLeftRightLeftHigher()
  {
    val expected = "ab" + newLine + "a"
    val document = (text("a") % "a") ~ "b"
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testLeftRightRightHigher()
  {
    val expected = "ab" + newLine + " b"
    val document = text("a") ~ (text("b") % "b")
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testTopBottomTopWider()
  {
    val expected = "aa" + newLine + "b"
    val document = text("aa") % "b"
    Assert.assertEquals(expected, document.renderString())
  }

  @Test
  def testTopBottomBottomWider()
  {
    val expected = "a" + newLine + "bb"
    val document = text("a") % "bb"
    Assert.assertEquals(expected, document.renderString())
  }
}
