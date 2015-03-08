package transformations.javac.statements

import core.particles.{ParticleWithGrammar, Contract, MetaObject, CompilationState}

trait StatementInstance extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    StatementSkeleton.getStatementToLines(state).put(key, (expression: MetaObject) => toByteCode(expression, state))
    super.inject(state)
  }

  val key: AnyRef

  def toByteCode(statement: MetaObject, state: CompilationState): Seq[MetaObject]

  override def dependencies: Set[Contract] = Set(StatementSkeleton) ++ super.dependencies
}
