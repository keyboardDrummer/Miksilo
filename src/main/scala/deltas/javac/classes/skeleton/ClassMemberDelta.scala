package deltas.javac.classes.skeleton

import core.particles.{Compilation, DeltaWithGrammar, Language}
import core.particles.node.Node

trait ClassMemberDelta extends DeltaWithGrammar {
  def bind(compilation: Compilation, clazz: ClassSignature, member: Node)
  def compile(compilation: Compilation, member: Node): Unit

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getRegistry(state).members ::= this
    super.inject(state)
  }
}
