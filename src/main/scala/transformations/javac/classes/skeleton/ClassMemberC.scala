package transformations.javac.classes.skeleton

import core.particles.{Language, DeltaWithGrammar}
import core.particles.node.Node

trait ClassMemberC extends DeltaWithGrammar {
  def bind(state: Language, clazz: ClassSignature, member: Node)
  def compile(state: Language, member: Node)

  override def inject(state: Language): Unit = {
    JavaClassSkeleton.getState(state).members ::= this
    super.inject(state)
  }
}
