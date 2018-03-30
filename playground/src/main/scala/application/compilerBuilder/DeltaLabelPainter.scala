package application.compilerBuilder

import javax.swing.JPanel

import core.deltas.{Contract, Delta}
import util.Cache

import scala.collection.mutable

class DeltaLabelPainter(container: JPanel, availableDeltas: Set[Delta]) {
  val dependants: mutable.HashMap[Contract, mutable.Set[Contract]] with mutable.MultiMap[Contract, Contract] = getDependants
  var selection: Seq[Delta] = Seq.empty
  val dependenciesCache = new Cache[Set[Contract]](() => selection.flatMap(s => s.dependencies).toSet)
  val dependantsCache = new Cache[Set[Contract]](() => selection.flatMap(s => dependants(s)).toSet)

  private def getDependants = {
    val dependants = new mutable.HashMap[Contract, mutable.Set[Contract]]()
      with mutable.MultiMap[Contract, Contract]

    for (delta <- availableDeltas) {
      for (dependency <- delta.dependencies) {
        dependants.addBinding(dependency, delta)
      }
    }
    dependants
  }

  def select(selection: Seq[Delta]) {
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
