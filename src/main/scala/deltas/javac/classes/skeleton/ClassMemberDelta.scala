package deltas.javac.classes.skeleton

import core.deltas.{Compilation, DeltaWithGrammar}
import core.deltas.node.Node
import core.language.Language

trait ClassMemberDelta extends DeltaWithGrammar {
  def bind(compilation: Compilation, classSignature: ClassSignature, member: Node)
  def compile(compilation: Compilation, member: Node): Unit

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getRegistry(state).members ::= this
    super.inject(state)
  }
}
