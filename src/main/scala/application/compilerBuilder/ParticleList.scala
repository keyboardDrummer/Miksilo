package application.compilerBuilder

import java.awt.event.MouseEvent

import core.particles.Particle
import org.jdesktop.swingx.JXList

class ParticleList() extends JXList() {
  override def getToolTipText(event: MouseEvent): String = {
    val index = this.locationToIndex(event.getPoint)
    val model = this.getModel
    if (index >= 0)
    {
      model.getElementAt(index).asInstanceOf[Particle].description
    }
    else
      ""
  }
}
