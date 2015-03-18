package transformations.javac.expressions

import core.particles._
import core.particles.node.MetaObject
import core.particles.path.Path
import transformations.javac.methods.MethodC
import transformations.javac.statements.StatementSkeleton

trait ExpressionInstance extends ParticleWithGrammar {
  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    ExpressionSkeleton.getToInstructionsRegistry(state).put(key, (expression: Path) => toByteCode(expression, state))
    ExpressionSkeleton.getGetTypeRegistry(state).put(key, (expression: Path) => getType(expression, state))
    super.inject(state)
  }

  def toByteCode(expression: Path, state: CompilationState): Seq[MetaObject]

  def getType(expression: Path, state: CompilationState): MetaObject

  def getVariables(state: CompilationState, obj: Path) = {
    val instances = StatementSkeleton.getState(state).instances
    val statement = obj.ancestors.filter(ancestor => instances.contains(ancestor.clazz)).head
    MethodC.getMethodCompiler(state).variablesPerStatement(statement)
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
