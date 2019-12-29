package application.compilerBuilder

import javax.swing.JPanel

import core.deltas.{Contract, Delta}
import util.CachedValue

import scala.collection.mutable

class DeltaLabelPainter(container: JPanel, availableDeltas: Set[Delta]) {
  val dependants: mutable.Map[Contract, mutable.Set[Contract]] = getDependants
  var selection: Seq[Delta] = Seq.empty
  val dependenciesCache = new CachedValue[Set[Contract]](() => selection.flatMap(s => s.dependencies).toSet)
  val dependantsCache = new CachedValue[Set[Contract]](() => selection.flatMap(s => dependants(s)).toSet)

  private def getDependants: mutable.Map[Contract, mutable.Set[Contract]] = {
    val dependants = new mutable.HashMap[Contract, mutable.Set[Contract]]()

    for (delta <- availableDeltas) {
      for (dependency <- delta.dependencies) {
        dependants.getOrElseUpdate(dependency, mutable.Set.empty).add(delta)
      }
    }
    dependants
  }

  def select(selection: Seq[Delta]): Unit =  {
    this.selection = selection
    dependenciesCache.clear()
    container.repaint()
  }

  def isDependency(particle: Delta) : Boolean = {
    if (selection == null)
      return false

    dependenciesCache.get.contains(particle)
  }

  def isDependant(particle: Delta): Boolean = {
    if (selection == null)
      return false

    dependantsCache.get.contains(particle)
  }
}
