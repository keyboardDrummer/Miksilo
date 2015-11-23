package application

import java.awt.{Font, Insets}
import javax.swing._
import javax.swing.border.TitledBorder

object StyleSheet {

  val presentationMode: Boolean = true
  val hugeFont = new Font("Courier New", Font.BOLD, 24)
  val codeFont = new Font("Courier New", Font.PLAIN, if (presentationMode) 22 else 14)
  val defaultInsets: Insets = new Insets(3, 3, 3, 3)

  def setTitleBorder(component: JComponent, title: String) {
    component.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), title.toUpperCase,
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, hugeFont))
  }


  def getAnyListVisuals[T](list: JList[T]): JScrollPane = {
    list.setDragEnabled(true)
    list.setFont(StyleSheet.codeFont)
    list.setBorder(BorderFactory.createLoweredBevelBorder())
    val scrollPane = new JScrollPane()
    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
    scrollPane.getViewport.setView(list)
    scrollPane.setAutoscrolls(true)
    scrollPane
  }
}
