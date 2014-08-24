package application.compilerBuilder

import javax.swing.TransferHandler.TransferSupport
import javax.swing.{DefaultListModel, JList, TransferHandler}

import core.transformation.sillyCodePieces.Injector

class ChosenParticlesTransferHandler(val model: DefaultListModel[Injector]) extends TransferHandler {
  override def canImport(support: TransferSupport): Boolean = {
    val injector = support.getTransferable.getTransferData(CompilerBuilderPanel.LIST_ITEM_DATA_FLAVOR)
    injector != null && !model.contains(injector)
  }

  override def importData(support: TransferSupport): Boolean = {
    val injector = support.getTransferable.getTransferData(CompilerBuilderPanel.LIST_ITEM_DATA_FLAVOR).asInstanceOf[Injector]
    val location = support.getDropLocation.asInstanceOf[JList.DropLocation]
    model.add(location.getIndex, injector)
    true
  }
}
