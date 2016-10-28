package core.responsiveDocument

import core.responsiveDocument.ResponsiveDocument._
import org.junit.{Assert, Test}
import org.scalatest.FunSuite

class TestResponsiveDocument extends FunSuite {

  val newLine = System.lineSeparator()


  def testText()
  {
    val expected: String = "hallo"
    val document: ResponsiveDocument = text(expected)
    assertResult(expected)(document.renderString())
  }


  def testLeftRight()
  {
    val expected = "hallo" + "daar"
    val document = ("hallo": ResponsiveDocument) ~ "daar"
    assertResult(expected)(document.renderString())
  }


  def testTopBottom()
  {
    val expected = "a" + newLine + "b"
    val document = ("a" : ResponsiveDocument) % "b"
    assertResult(expected)(document.renderString())
  }


  def testLeftRightLeftHigher()
  {
    val expected = "ab" + newLine + "a"
    val document = (text("a") % "a") ~ "b"
    assertResult(expected)(document.renderString())
  }


  def testLeftRightRightHigher()
  {
    val expected = "ab" + newLine + " b"
    val document = text("a") ~ (text("b") % "b")
    assertResult(expected)(document.renderString())
  }


  def testTopBottomTopWider()
  {
    val expected = "aa" + newLine + "b"
    val document = text("aa") % "b"
    assertResult(expected)(document.renderString())
  }


  def testTopBottomBottomWider()
  {
    val expected = "a" + newLine + "bb"
    val document = text("a") % "bb"
    assertResult(expected)(document.renderString())
  }
}
