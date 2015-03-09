package transformations.javac.expressions

import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}

trait ExpressionInstance extends ParticleWithGrammar {
  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    ExpressionSkeleton.getExpressionToLines(state).put(key, (expression: MetaObject) => toByteCode(expression, state))
    ExpressionSkeleton.getGetTypeRegistry(state).put(key, (expression: MetaObject) => getType(expression, state))
    super.inject(state)
  }

  def toByteCode(expression: MetaObject, state: CompilationState): Seq[MetaObject]

  def getType(expression: MetaObject, state: CompilationState): MetaObject

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
