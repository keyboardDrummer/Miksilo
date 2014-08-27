package application.compilerBuilder

import javax.swing.JPanel

import core.transformation.sillyCodePieces.Injector
import util.Cache

import scala.collection.mutable

class ParticleLabelPainter(container: JPanel, availableParticles: Seq[Injector]) {
  val dependants = getDependants
  var selection: Seq[Injector] = Seq.empty
  val dependenciesCache = new Cache[Set[Injector]](() => selection.flatMap(s => s.dependencies2).toSet)
  val dependantsCache = new Cache[Set[Injector]](() => selection.flatMap(s => dependants(s)).toSet)

  def getDependants = {
    val dependants = new mutable.HashMap[Injector, mutable.Set[Injector]]()
      with mutable.MultiMap[Injector, Injector]

    for (particle <- availableParticles) {
      for (dependency <- particle.dependencies2) {
        dependants.addBinding(dependency, particle)
      }
    }
    dependants
  }

  def select(selection: Seq[Injector]) {
    this.selection = selection
    dependenciesCache.clear()
    container.repaint()
  }

  def isDependency(particle: Injector) : Boolean = {
    if (selection == null)
      return false

    dependenciesCache.get.contains(particle)
  }

  def isDependant(particle: Injector): Boolean = {
    if (selection == null)
      return false

    dependantsCache.get.contains(particle)
  }
}
