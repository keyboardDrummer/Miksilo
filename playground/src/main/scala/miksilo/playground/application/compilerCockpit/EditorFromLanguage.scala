package playground.application.compilerCockpit

import java.awt.CardLayout
import java.io.ByteArrayInputStream
import java.net.URL
import java.nio.charset.StandardCharsets

import core.deltas.LanguageFromDeltas
import core.deltas.grammars.LanguageGrammars
import javax.swing._
import languageServer.MiksiloLanguageServer
import org.fife.ui.rsyntaxtextarea.parser._
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane

class EditorFromLanguage(language: LanguageFromDeltas) extends JPanel(new CardLayout()) {

  val factory = new TokenMakerFactoryFromGrammar(LanguageGrammars.grammars.get(language).root)

  val inputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  inputDocument.setTokenMakerFactory(factory)

  val inputTextArea = new MiksiloTextEditor(inputDocument)
  val server = new MiksiloLanguageServer(language)
  inputTextArea.setServer(server)

  inputTextArea.addParser(new Parser() {
    override def parse(doc: RSyntaxDocument, style: String): ParseResult = {
      val text = doc.getText(0, doc.getLength)

      val result = new DefaultParseResult(this)
      val compilation = language.compileString(text)
      for(diagnostic <- compilation.diagnosticsForFile(inputTextArea.freshTextDocumentReference.uri)) {

        val lineLengths = text.split("\n").map(line => line.length + 1)
        val row = diagnostic.range.start.line
        val offset = lineLengths.take(row).sum + diagnostic.range.start.character
        val notice = new DefaultParserNotice(this, diagnostic.message, row, offset, 1)
        result.addNotice(notice)
      }
      result
    }

    override def isEnabled: Boolean = true

    override def getImageBase: URL = null

    override def getHyperlinkListener: ExtendedHyperlinkListener = null
  })

  add(new RTextScrollPane(inputTextArea))
}
