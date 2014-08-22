package application.compilerBuilder

import java.awt._
import java.awt.datatransfer.{DataFlavor, Transferable}
import java.awt.dnd.DnDConstants
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.RowFilter.Entry
import javax.swing.TransferHandler.TransferSupport
import javax.swing._
import javax.swing.border.{BevelBorder, TitledBorder}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.Document

import application.{StyleSheet, ExampleListCellRenderer}
import application.compilerBuilder.compilerCockpit.CompilerCockpit
import core.transformation.sillyCodePieces.Injector
import org.jdesktop.swingx.JXList
import transformations.javac.JavaCompiler

object CompilerBuilderPanel {

  def getCompilerBuilderPanel: java.awt.Component = {
    val panel = new JPanel()
    panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
    panel.setAutoscrolls(true)
    val layout = new GridBagLayout()
    panel.setLayout(layout)

    val presetsPanel: JPanel = getPresetsPanel
    val presetsConstraints = getConstraints

    layout.setConstraints(presetsPanel, presetsConstraints)
    panel.add(presetsPanel)

    val availableScrollPane = getAvailableScrollPane
    val availableListConstraints: GridBagConstraints = getConstraints
    panel.add(availableScrollPane, availableListConstraints)


    val programPanel: JPanel = getCompilerPanel
    val programPanelConstraints = getConstraints
    programPanelConstraints.weightx = 2
    panel.add(programPanel, programPanelConstraints)

    panel
  }

  def getSeparatorConstraints: GridBagConstraints = {
    val separatorConstraints = new GridBagConstraints()
    separatorConstraints.fill = GridBagConstraints.BOTH
    separatorConstraints.insets = defaultInsets
    separatorConstraints
  }

  def getPresetsPanel: JPanel = {
    val presetsPanel = new JPanel()

    val layout: GridBagLayout = new GridBagLayout()
    presetsPanel.setLayout(layout)
    val listConstraints: GridBagConstraints = new GridBagConstraints()
    listConstraints.fill = GridBagConstraints.BOTH
    listConstraints.weightx = 1
    listConstraints.weighty = 1
    listConstraints.gridx = 0
    listConstraints.insets = defaultInsets

    setTitleBorder(presetsPanel, "Presets")
    val presetsList = new JList[String]()
    val model: DefaultListModel[String] = new DefaultListModel[String]()
    model.add(0, "woep")
    presetsList.setModel(model)
    presetsPanel.add(getAnyListVisuals(presetsList), listConstraints)

    val buttonConstraints: GridBagConstraints = new GridBagConstraints()
    buttonConstraints.fill = GridBagConstraints.HORIZONTAL
    buttonConstraints.weightx = 1
    buttonConstraints.gridx = 0
    buttonConstraints.insets = defaultInsets
    val applyButton = new JButton()
    applyButton.setText("Apply")
    presetsPanel.add(applyButton, buttonConstraints)

    val secondButton = new JButton()
    secondButton.setText("Apply2")
    presetsPanel.add(secondButton, buttonConstraints)

    presetsPanel
  }

  def setTitleBorder(presetsPanel: JComponent, titleString: String) {
    presetsPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), titleString.toUpperCase,
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, StyleSheet.hugeFont))
  }

  def defaultInsets: Insets = {
    new Insets(3, 3, 3, 3)
  }

  def getAvailableScrollPane = {
    val availableItems: Seq[Injector] = JavaCompiler.javaCompilerTransformations
    val availableList = new JXList(availableItems.toArray.asInstanceOf[Array[Object]])

    availableList.setTransferHandler(new DragHandler(availableList, availableItems))
    availableList.setDragEnabled(true)

    val result = getInjectorListVisuals(availableList)
    setTitleBorder(result, "Available")
    result
  }

  def getInjectorListVisuals(list: JXList) = {

    val result = new JPanel(new BorderLayout())

    list.setAutoCreateRowSorter(true)
    val listener = new SearchFieldListener(list)
    val searchField = new JTextField(20)
    searchField.getDocument.addDocumentListener(listener)
    result.add(searchField, BorderLayout.PAGE_START)

    val scrollPane: JScrollPane = getAnyListVisuals(list)
    list.setCellRenderer(new ExampleListCellRenderer())
    scrollPane

    result.add(scrollPane, BorderLayout.CENTER)

    result
  }

  class ContactRowFilter(val compare: String) extends RowFilter[ListModel[_], java.lang.Integer] {
    override def include(entry: Entry[_ <: ListModel[_], _ <: Integer]): Boolean = {
      val index = entry.getIdentifier
      val contact = entry.getValue(index).asInstanceOf[Injector]
      contact.name.contains(compare)
    }
  }

  class SearchFieldListener(val list: JXList) extends DocumentListener {

    def updateFilter(doc: Document) = {
      val text = doc.getText(0, doc.getLength)
      list.setRowFilter(if (text.length > 0) new ContactRowFilter(text) else null)
    }

    override def insertUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)

    override def changedUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)

    override def removeUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)
  }

  def getAnyListVisuals[T](list: JList[T]): JScrollPane = {
    list.setDragEnabled(true)
    list.setBorder(BorderFactory.createLoweredBevelBorder())
    val scrollPane = new JScrollPane()
    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
    scrollPane.getViewport.setView(list)
    scrollPane.setAutoscrolls(true)
    scrollPane
  }

  def getCompilerPanel: JPanel = {
    val layout = new GridBagLayout()
    val compilerPanel = new JPanel(layout)
    setTitleBorder(compilerPanel, "Compiler")

    val firstPanel: JPanel = getCompilerTopPanel

    val constraints = getConstraints
    constraints.gridx = 0
    constraints.weighty = 2
    compilerPanel.add(firstPanel, constraints)

    compilerPanel.add(new JSeparator(SwingConstants.HORIZONTAL), getSeparatorConstraints)
    val console = new JTextArea()
    console.setBorder(BorderFactory.createLoweredBevelBorder())
    val consoleWrapper = new JPanel(new BorderLayout())
    consoleWrapper.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Console"))
    consoleWrapper.add(console)

    constraints.weighty = 1
    compilerPanel.add(consoleWrapper, constraints)

    val actionButtonsLayout = new FlowLayout()
    actionButtonsLayout.setAlignment(FlowLayout.RIGHT)
    val actionButtons = new JPanel(actionButtonsLayout)
    val buildCompilerButton = new JButton("Build Compiler")
    buildCompilerButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val cockpit = new CompilerCockpit(JavaCompiler.javaCompilerTransformations)
        cockpit.pack()
        cockpit.maximize()
        cockpit.visible = true
      }
    })
    actionButtons.add(buildCompilerButton)

    constraints.weighty = 0
    constraints
    compilerPanel.add(actionButtons, constraints)

    compilerPanel
  }

  def getCompilerTopPanel: JPanel = {
    val firstPanel = new JPanel(new GridBagLayout())

    val compilerItems = new DefaultListModel[Injector]()
    val compilerList = new JXList()
    compilerList.setTransferHandler(new MyTransferHandler(compilerItems))
    compilerList.setDropMode(DropMode.INSERT)
    compilerList.setModel(compilerItems)
    val compilerListPanel = getInjectorListVisuals(compilerList)
    compilerListPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Chosen"))
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    val dependentItems = new DefaultListModel[Injector]()
    val dependentList = new JXList()
    dependentList.setModel(dependentItems)
    val dependentPanel = getInjectorListVisuals(dependentList)
    dependentPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Automatically added"))
    val dependentConstraints = getConstraints
    dependentConstraints.gridx = 1
    firstPanel.add(dependentPanel, dependentConstraints)
    firstPanel
  }

  class DragHandler(val availableList: JList[_], val model: Seq[Injector]) extends TransferHandler {

    @Override
    override def createTransferable(comp: JComponent): Transferable = {
      val index = availableList.getSelectedIndex
      new ListItemTransferable(model(index))
    }

    override def getSourceActions(c: JComponent): Int = DnDConstants.ACTION_COPY_OR_MOVE
  }

  val LIST_ITEM_DATA_FLAVOR: DataFlavor = new DataFlavor(classOf[Injector], "java/ListItem")

  class ListItemTransferable(listItem: Injector) extends Transferable {

    @Override
    def getTransferDataFlavors: Array[DataFlavor] = {
      Array(LIST_ITEM_DATA_FLAVOR)
    }

    @Override
    def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
      flavor.equals(LIST_ITEM_DATA_FLAVOR)
    }

    override def getTransferData(flavor: DataFlavor): AnyRef = listItem
  }

  class MyTransferHandler(val model: DefaultListModel[Injector]) extends TransferHandler {
    override def canImport(support: TransferSupport): Boolean = {
      val injector = support.getTransferable.getTransferData(LIST_ITEM_DATA_FLAVOR)
      injector != null && !model.contains(injector)
    }

    override def importData(support: TransferSupport): Boolean = {
      val injector = support.getTransferable.getTransferData(LIST_ITEM_DATA_FLAVOR).asInstanceOf[Injector]
      val location = support.getDropLocation.asInstanceOf[JList.DropLocation]
      model.add(location.getIndex, injector)
      true
    }
  }


  def getConstraints: GridBagConstraints = {
    val cons = new GridBagConstraints()
    cons.fill = GridBagConstraints.BOTH
    cons.weightx = 1
    cons.weighty = 1
    cons
  }
}
