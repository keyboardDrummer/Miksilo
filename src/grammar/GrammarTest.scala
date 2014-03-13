package grammar

import org.junit.{Assert, Test}

class GrammarTest {
  @Test
  def testParse() {
    val line = "weAreTheChampions".toStream
    val parser = Sequence(Consume("we".toSeq),Sequence(Consume("Are".toSeq),Sequence(Consume("The".toSeq),Consume("Champions".toSeq))))
    val parseResult = parser.parse(line)
    Assert.assertEquals(ParseFailure, parser.parse("randomLine".toStream))
    Assert.assertEquals(ParseSuccess(Seq(),Stream()), parseResult)
    Assert.assertEquals(ParseSuccess("weAreTheChampions".toSeq,Stream()), parser.printer.parse(Stream()))
  }

  @Test
  def testChoice {
    val parser = Choice(Consume("you".toSeq),Consume("me".toSeq))
    Assert.assertEquals(ParseSuccess(Seq(),Stream()), parser.parse("you".toStream))
    Assert.assertEquals(ParseSuccess(Seq(),Stream()), parser.parse("me".toStream))
    Assert.assertEquals(ParseSuccess("you".toSeq,Stream()), parser.printer.parse(Stream()))
  }
}
