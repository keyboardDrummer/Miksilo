package application.compilerBuilder

import java.awt.datatransfer.Transferable
import java.awt.dnd.DnDConstants
import javax.swing.TransferHandler.TransferSupport
import javax.swing.{DefaultListModel, JComponent, JList}

import core.particles.Delta

class SelectedParticlesTransferHandler(availableList: JList[_], val model: DefaultListModel[ParticleInstance])
  extends ParticleProviderTransferHandler(availableList) {
  override def canImport(support: TransferSupport): Boolean = {
    val injectors = getInjectors(support.getTransferable)
    injectors != null
  }

  override def importData(support: TransferSupport): Boolean = {
    val injectors = getInjectors(support.getTransferable)
    val location = support.getDropLocation.asInstanceOf[JList.DropLocation].getIndex

    for(injector <- injectors.reverse)
      model.add(location, new ParticleInstance(injector.particle))
    true
  }

  override def exportDone(source: JComponent, data: Transferable, action: Int): Unit = {
    val injectors = getInjectors(data)
    val injectorSet = injectors.toSet

    if (source == availableList)
    {
      var index = model.size() - 1
      while(index >= 0)
      {
        val element = model.getElementAt(index)
        if (injectorSet.contains(element))
        {
          model.removeElementAt(index)
        }
        index -= 1
      }
    }

    super.exportDone(source, data, action)
  }

  def getInjectors(transferable: Transferable): Seq[ParticleInstance] = {
    transferable.getTransferData(ListItemTransferable.LIST_ITEM_DATA_FLAVOR).asInstanceOf[Seq[_]].collect({
      case particle: Delta => new ParticleInstance(particle)
      case instance: ParticleInstance => instance
    })
  }

  override def getSourceActions(c: JComponent): Int = DnDConstants.ACTION_MOVE
}
