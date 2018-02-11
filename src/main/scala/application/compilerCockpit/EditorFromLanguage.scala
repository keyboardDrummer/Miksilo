package application.compilerCockpit

import java.awt.CardLayout
import java.io.ByteArrayInputStream
import java.net.URL
import java.nio.charset.StandardCharsets
import javax.swing.JPanel

import core.language.node.Node
import core.language.{Language, NoSourceException, ParseException}
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rsyntaxtextarea.parser._
import org.fife.ui.rtextarea.RTextScrollPane

import scala.util.{Failure, Try}

class EditorFromLanguage(language: Language) extends JPanel(new CardLayout()) {

  val factory = new TokenMakerFactoryFromGrammar(language.grammars.root)

  val inputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  inputDocument.setTokenMakerFactory(factory)

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: (.*)\n\n""".r

  val inputTextArea = new RSyntaxTextArea(inputDocument)
  inputTextArea.addParser(new Parser() {
    override def parse(doc: RSyntaxDocument, style: String): ParseResult = {
      val text = doc.getText(0, doc.getLength)
      val stream = new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8.name()))
      val parseResult: Try[Node] = language.parse(stream)
      val result = new DefaultParseResult(this)
      parseResult match {
        case Failure(NoSourceException) =>
        case Failure(ParseException(message)) =>
          val notice: DefaultParserNotice = getNoticeFromParseException(text, message)
          result.addNotice(notice)
        case _ =>
      }
      result
    }

    override def isEnabled: Boolean = true

    override def getImageBase: URL = null

    override def getHyperlinkListener: ExtendedHyperlinkListener = null

    private def getNoticeFromParseException(text: String, message: String) = {
      val messageMatch = rowColumnRegex.findFirstMatchIn(message).get
      val row = messageMatch.group(1).toInt
      val column = messageMatch.group(2).toInt
      val lineLengths = text.split("\n").map(line => line.length + 1)
      val offset = lineLengths.take(row - 1).sum + column - 1
      new DefaultParserNotice(this, messageMatch.group(3), row, offset, 1)
    }
  })

  add(new RTextScrollPane(inputTextArea))
}
