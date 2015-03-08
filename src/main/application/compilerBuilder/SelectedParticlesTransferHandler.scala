package application.compilerBuilder

import java.awt.dnd.DnDConstants
import javax.swing.TransferHandler.TransferSupport
import javax.swing.{DefaultListModel, JComponent, JList}

import core.transformation.Particle

class SelectedParticlesTransferHandler(availableList: JList[_], val model: DefaultListModel[Particle]) extends ParticleProviderTransferHandler(availableList) {
  override def canImport(support: TransferSupport): Boolean = {
    val injectors = getInjectors(support)
    injectors != null
  }

  override def importData(support: TransferSupport): Boolean = {
    val injectors = getInjectors(support)
    var location = support.getDropLocation.asInstanceOf[JList.DropLocation].getIndex
    val injectorSet = injectors.toSet

    var index = 0
    while(index < model.size())
    {
      val element = model.getElementAt(index)
      if (injectorSet.contains(element))
      {
        model.removeElementAt(index)
        if (index < location)
          location -= 1
      }
      else
      {
        index += 1
      }
    }
    for(injector <- injectors.reverse)
      model.add(location, injector)
    true
  }

  def getInjectors(support: TransferSupport): Seq[Particle] = {
    support.getTransferable.getTransferData(ListItemTransferable.LIST_ITEM_DATA_FLAVOR).asInstanceOf[Seq[Particle]]
  }

  override def getSourceActions(c: JComponent): Int = DnDConstants.ACTION_MOVE
}
