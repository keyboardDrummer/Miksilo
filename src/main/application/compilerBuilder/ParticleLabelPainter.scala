package application.compilerBuilder

import javax.swing.JPanel
import core.particles.Particle
import util.Cache

import scala.collection.mutable

class ParticleLabelPainter(container: JPanel, availableParticles: Seq[Particle]) {
  val dependants = getDependants
  var selection: Seq[Particle] = Seq.empty
  val dependenciesCache = new Cache[Set[Particle]](() => selection.flatMap(s => s.dependencies2).toSet)
  val dependantsCache = new Cache[Set[Particle]](() => selection.flatMap(s => dependants(s)).toSet)

  def getDependants = {
    val dependants = new mutable.HashMap[Particle, mutable.Set[Particle]]()
      with mutable.MultiMap[Particle, Particle]

    for (particle <- availableParticles) {
      for (dependency <- particle.dependencies2) {
        dependants.addBinding(dependency, particle)
      }
    }
    dependants
  }

  def select(selection: Seq[Particle]) {
    this.selection = selection
    dependenciesCache.clear()
    container.repaint()
  }

  def isDependency(particle: Particle) : Boolean = {
    if (selection == null)
      return false

    dependenciesCache.get.contains(particle)
  }

  def isDependant(particle: Particle): Boolean = {
    if (selection == null)
      return false

    dependantsCache.get.contains(particle)
  }
}
