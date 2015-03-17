package transformations.javac.expressions

import core.particles._
import transformations.javac.methods.MethodC
import transformations.javac.statements.StatementSkeleton

trait ExpressionInstance extends ParticleWithGrammar {
  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    ExpressionSkeleton.getToInstructionsRegistry(state).put(key, (expression: MetaObjectWithOrigin) => toByteCode(expression, state))
    ExpressionSkeleton.getGetTypeRegistry(state).put(key, (expression: MetaObjectWithOrigin) => getType(expression, state))
    super.inject(state)
  }

  def toByteCode(expression: MetaObjectWithOrigin, state: CompilationState): Seq[MetaObject]

  def getType(expression: MetaObjectWithOrigin, state: CompilationState): MetaObject

  def getVariables(state: CompilationState, obj: MetaObjectWithOrigin) = {
    val instances = StatementSkeleton.getState(state).instances
    val statement = obj.origin.ancestors.filter(ancestor => instances.contains(ancestor.clazz)).head
    MethodC.getMethodCompiler(state).variablesPerStatement(statement)
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
