package deltas.javac.methods

import core.language.exceptions.BadInputException
import core.language.node.Node
import core.deltas.path.{NodePath, PathRoot}
import core.language.Compilation
import deltas.bytecode.types.QualifiedObjectTypeDelta
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods.MethodDelta._
import deltas.javac.statements.StatementToByteCodeSkeleton
import deltas.javac.statements.locals.LocalsAnalysis

case class MethodCompiler(compilation: Compilation, method: Method[Node]) {
  val classCompiler: ClassCompiler = JavaClassSkeleton.getClassCompiler(compilation)

  private val initialVariables: VariablePool = getInitialVariables

  val localAnalysis = new LocalsAnalysis(compilation, method.body, initialVariables)
  val variablesPerStatement: Map[NodePath, VariablePool] = localAnalysis.run()

  def getInitialVariables: VariablePool = {
    var result = VariablePool(compilation)
    if (!method.isStatic)
      result = result.add("this", QualifiedObjectTypeDelta.neww(classCompiler.fullyQualify(classCompiler.currentClassInfo.name)))
    for (parameter <- method.parameters)
      result = result.add(parameter.name, getParameterType(PathRoot(parameter), classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: NodePath) extends BadInputException
  {
    override def toString = s"the following statement is unreachable:\n$statement"
  }

  def getVariables(node: NodePath): VariablePool = {
    val instances = StatementToByteCodeSkeleton.instances.get(compilation)
    val statement = node.ancestors.filter(ancestor => instances.contains(ancestor.shape)).head
    val variablesPerStatement: Map[NodePath, VariablePool] = MethodDelta.getMethodCompiler(compilation).variablesPerStatement
    try
    {
      variablesPerStatement(statement)
    } catch
      {
        case e: NoSuchElementException => throw StatementWasNotFoundDuringLocalsAnalysis(statement)
      }
  }
}
