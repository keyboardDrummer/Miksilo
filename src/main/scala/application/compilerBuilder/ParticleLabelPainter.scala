package application.compilerBuilder

import javax.swing.JPanel

import core.deltas.Delta
import util.Cache

import scala.collection.mutable

class ParticleLabelPainter(container: JPanel, availableDeltas: Set[Delta]) {
  val dependants = getDependants
  var selection: Seq[Delta] = Seq.empty
  val dependenciesCache = new Cache[Set[Delta]](() => selection.flatMap(s => s.dependencies2).toSet)
  val dependantsCache = new Cache[Set[Delta]](() => selection.flatMap(s => dependants(s)).toSet)

  def getDependants = {
    val dependants = new mutable.HashMap[Delta, mutable.Set[Delta]]()
      with mutable.MultiMap[Delta, Delta]

    for (particle <- availableDeltas) {
      for (dependency <- particle.dependencies2) {
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
