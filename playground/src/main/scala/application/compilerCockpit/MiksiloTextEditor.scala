package application.compilerCockpit

import java.awt.event
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{JMenuItem, JPopupMenu}

import langserver.types
import langserver.types.{Position, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier}
import lsp._
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, RSyntaxTextArea}

class MiksiloTextEditor(document: RSyntaxDocument) extends RSyntaxTextArea(document) with Connection {

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
      val range = server.gotoDefinitionRequest(freshTextDocumentReference, currentPosition).params.head.range
      def positionToOffset(position: Position) = MiksiloTextEditor.this.getLineStartOffset(position.line - 1) + position.character - 1
      MiksiloTextEditor.this.getCaret.setDot(positionToOffset(range.start))
      MiksiloTextEditor.this.getCaret.moveDot(positionToOffset(range.end))
    })
    popupMenu.add(gotoDefinitionItem)
    popupMenu
  }

  override def configurePopupMenu(popupMenu: JPopupMenu): Unit = {
    super.configurePopupMenu(popupMenu)

    gotoDefinitionItem.setEnabled(server.gotoDefinitionRequest(freshTextDocumentReference, currentPosition).params.nonEmpty)
  }

  override def setServer(languageServer: LanguageServer): Unit = {
    server = languageServer.asInstanceOf[MiksiloLanguageServer]

    val freshTextDocument = new TextDocumentItem("space", "language", 1, "")
    val documentId = new VersionedTextDocumentIdentifier("space", 1)
    notifySubscribers(DidOpenTextDocumentParams(freshTextDocument))

    document.addDocumentListener(new DocumentListener {


      def changeAll(e: DocumentEvent): Unit = {
        val text = e.getDocument.getText(0, e.getDocument.getLength)
        notifySubscribers(DidChangeTextDocumentParams(documentId,
          Seq(types.TextDocumentContentChangeEvent(None, None, text))))
      }
      override def removeUpdate(e: DocumentEvent): Unit = changeAll(e)

      override def changedUpdate(e: DocumentEvent): Unit = changeAll(e)

      override def insertUpdate(e: DocumentEvent): Unit = changeAll(e)
    })
  }
}
