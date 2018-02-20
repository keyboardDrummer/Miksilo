package application.compilerCockpit

import java.awt.{CardLayout, event}
import java.io.ByteArrayInputStream
import java.net.URL
import java.nio.charset.StandardCharsets
import javax.swing._

import core.language.node.{Node, Position}
import core.language.{LanguageServer, NoSourceException, ParseException}
import org.fife.ui.rsyntaxtextarea.parser._
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane

import scala.util.{Failure, Try}

class MiksiloTextEditor(server: LanguageServer, document: RSyntaxDocument) extends RSyntaxTextArea(document) {

  def currentPosition: Position = {
    val offset = this.getCaret.getDot
    Position(offset)
  }


  var gotoDefinitionItem: JMenuItem = _
  override def createPopupMenu(): JPopupMenu = {
    val popupMenu = super.createPopupMenu()

    popupMenu.addSeparator()
    gotoDefinitionItem = new JMenuItem("Go to definition")
    gotoDefinitionItem.addActionListener((e: event.ActionEvent) => {
      val position = server.go(currentPosition)
      MiksiloTextEditor.this.getCaret.setDot(position.end.offset)
      MiksiloTextEditor.this.getCaret.moveDot(position.start.offset)
    })
    popupMenu.add(gotoDefinitionItem)
    popupMenu
  }

  override def configurePopupMenu(popupMenu: JPopupMenu): Unit = {
    super.configurePopupMenu(popupMenu)

    gotoDefinitionItem.setEnabled(server.isReference(currentPosition))
  }
}


class EditorFromLanguage(server: LanguageServer) extends JPanel(new CardLayout()) {

  private def language = server.language
  val factory = new TokenMakerFactoryFromGrammar(language.grammars.root)

  val inputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  inputDocument.setTokenMakerFactory(factory)

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: (.*)\n\n""".r

  val inputTextArea = new MiksiloTextEditor(server, inputDocument)
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
