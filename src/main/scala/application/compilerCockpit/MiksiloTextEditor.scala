package application.compilerCockpit

import java.awt.event
import javax.swing.{JMenuItem, JPopupMenu}

import core.language.LanguageServer
import core.language.node.Position
import org.fife.ui.rsyntaxtextarea.{RSyntaxDocument, RSyntaxTextArea}

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
