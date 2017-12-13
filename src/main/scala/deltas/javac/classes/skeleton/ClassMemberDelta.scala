package deltas.javac.classes.skeleton

import core.deltas.{Compilation, DeltaWithGrammar, Language}
import core.deltas.node.Node

trait ClassMemberDelta extends DeltaWithGrammar {
  def bind(compilation: Compilation, classSignature: ClassSignature, member: Node)
  def compile(compilation: Compilation, member: Node): Unit

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getRegistry(state).members ::= this
    super.inject(state)
  }
}
