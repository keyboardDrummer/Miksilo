package playground.application.compilerCockpit

import java.awt.event

import core.parsers.editorParsers.Position
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{JMenuItem, JPopupMenu}
import languageServer._
import _root_.lsp.{LanguageServer, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier, _}
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, RSyntaxTextArea}

class MiksiloTextEditor(document: RSyntaxDocument) extends RSyntaxTextArea(document) {

  def currentPosition: Position = {
    Position(this.getCaretLineNumber + 1, this.getCaretOffsetFromLineStart + 1)
  }

  var server: MiksiloLanguageServer = _

  val freshTextDocumentReference = new TextDocumentIdentifier("space")

  var gotoDefinitionItem: JMenuItem = _
  override def createPopupMenu(): JPopupMenu = {
    val popupMenu = super.createPopupMenu()

    popupMenu.addSeparator()
    gotoDefinitionItem = new JMenuItem("Go to definition")
    gotoDefinitionItem.addActionListener((e: event.ActionEvent) => {
      val range = server.gotoDefinition(DocumentPosition(freshTextDocumentReference, currentPosition)).head.range
      def positionToOffset(position: Position) = MiksiloTextEditor.this.getLineStartOffset(position.line - 1) + position.character - 1
      MiksiloTextEditor.this.getCaret.setDot(positionToOffset(range.start))
      MiksiloTextEditor.this.getCaret.moveDot(positionToOffset(range.end))
    })
    popupMenu.add(gotoDefinitionItem)
    popupMenu
  }

  override def configurePopupMenu(popupMenu: JPopupMenu): Unit = {
    super.configurePopupMenu(popupMenu)

    gotoDefinitionItem.setEnabled(server.gotoDefinition(DocumentPosition(freshTextDocumentReference, currentPosition)).nonEmpty)
  }

  def setServer(languageServer: LanguageServer): Unit = {
    server = languageServer.asInstanceOf[MiksiloLanguageServer]

    val freshTextDocument = new TextDocumentItem("space", "language", 1, "")
    val documentId = new VersionedTextDocumentIdentifier("space", 1)
    server.didOpen(freshTextDocument)

    document.addDocumentListener(new DocumentListener {

      def changeAll(e: DocumentEvent): Unit = {
        val text = e.getDocument.getText(0, e.getDocument.getLength)
        server.didChange(DidChangeTextDocumentParams(documentId,
          Seq(TextDocumentContentChangeEvent(None, None, text))))
      }
      override def removeUpdate(e: DocumentEvent): Unit = changeAll(e)

      override def changedUpdate(e: DocumentEvent): Unit = changeAll(e)

      override def insertUpdate(e: DocumentEvent): Unit = changeAll(e)
    })
  }
}
