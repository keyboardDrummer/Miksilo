package application.compilerBuilder

import javax.swing.JPanel

import core.deltas.{Contract, Delta}
import util.Cache

import scala.collection.mutable

class ParticleLabelPainter(container: JPanel, availableDeltas: Set[Delta]) {
  val dependants = getDependants
  var selection: Seq[Delta] = Seq.empty
  val dependenciesCache = new Cache[Set[Contract]](() => selection.flatMap(s => s.dependencies).toSet)
  val dependantsCache = new Cache[Set[Contract]](() => selection.flatMap(s => dependants(s)).toSet)

  private def getDependants = {
    val dependants = new mutable.HashMap[Contract, mutable.Set[Contract]]()
      with mutable.MultiMap[Contract, Contract]

    for (particle <- availableDeltas) {
      for (dependency <- particle.dependencies) {
        dependants.addBinding(dependency, particle)
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
