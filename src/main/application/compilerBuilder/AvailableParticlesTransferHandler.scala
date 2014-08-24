package application.compilerBuilder

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.awt.dnd.DnDConstants
import javax.swing.{JComponent, TransferHandler, JList}

import core.transformation.sillyCodePieces.Injector


class AvailableParticlesTransferHandler(val availableList: JList[_], val model: Seq[Injector]) extends TransferHandler {

  @Override
  override def createTransferable(comp: JComponent): Transferable = {
    val index = availableList.getSelectedIndex
    new ListItemTransferable(model(index))
  }

  override def getSourceActions(c: JComponent): Int = DnDConstants.ACTION_COPY_OR_MOVE
}

class ListItemTransferable(listItem: Injector) extends Transferable {

  @Override
  def getTransferDataFlavors: Array[DataFlavor] = {
    Array(CompilerBuilderPanel.LIST_ITEM_DATA_FLAVOR)
  }

  @Override
  def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
    flavor.equals(CompilerBuilderPanel.LIST_ITEM_DATA_FLAVOR)
  }

  override def getTransferData(flavor: DataFlavor): AnyRef = listItem
}