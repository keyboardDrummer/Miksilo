package core.transformation

import scala.collection.mutable

trait ParticleWithState {
  type State
  class ClassRegistry[Registration] extends mutable.HashMap[Any, Registration]

  def createState: State
  def getState(state: CompilationState) = state.data.getOrElseUpdate(this, createState).asInstanceOf[State]
}

trait Particle extends Contract {

  def inject(state: CompilationState) = {

  }

  final def dependencies2: Set[Particle] = dependencies.collect({case x: Particle => x})

  def description: String
}
