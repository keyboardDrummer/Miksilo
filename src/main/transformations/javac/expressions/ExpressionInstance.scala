package transformations.javac.expressions

import core.particles._
import transformations.javac.methods.MethodC
import transformations.javac.statements.StatementSkeleton

trait ExpressionInstance extends ParticleWithGrammar {
  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    ExpressionSkeleton.getToInstructionsRegistry(state).put(key, (expression: Origin) => toByteCode(expression, state))
    ExpressionSkeleton.getGetTypeRegistry(state).put(key, (expression: Origin) => getType(expression, state))
    super.inject(state)
  }

  def toByteCode(expression: Origin, state: CompilationState): Seq[MetaObject]

  def getType(expression: Origin, state: CompilationState): MetaObject

  def getVariables(state: CompilationState, obj: Origin) = {
    val instances = StatementSkeleton.getState(state).instances
    val statement = obj.ancestors.filter(ancestor => instances.contains(ancestor.clazz)).head
    MethodC.getMethodCompiler(state).variablesPerStatement(statement)
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
