package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.languageServer.core.language.Compilation
import miksilo.languageServer.core.language.exceptions.BadInputException
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.types.QualifiedObjectTypeDelta
import miksilo.modularLanguages.deltas.javac.classes.ClassCompiler
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.MethodDelta.Method
import miksilo.modularLanguages.deltas.statement.{LocalsAnalysis, StatementDelta}

case class MethodCompiler(compilation: Compilation, method: Method[Node]) {
  val classCompiler: ClassCompiler = JavaClassDelta.getClassCompiler(compilation)

  private val initialVariables: VariablePool = getInitialVariables

  val localAnalysis = new LocalsAnalysis(compilation, method.body, initialVariables)
  val variablesPerStatement: Map[NodePath, VariablePool] = localAnalysis.run()

  def getInitialVariables: VariablePool = {
    var result = VariablePool(compilation)
    if (!method.isStatic)
      result = result.add("this", QualifiedObjectTypeDelta.neww(classCompiler.fullyQualify(classCompiler.currentClassInfo.name)))
    for (parameter <- method.parameters)
      result = result.add(parameter.name, MethodDelta.getParameterType(PathRoot(parameter), classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: NodePath) extends BadInputException
  {
    override def toString = s"the following statement is unreachable:\n$statement"
  }

  def getVariables(node: NodePath): VariablePool = {
    val instances = StatementDelta.instances.get(compilation)
    val statement: NodePath = node.ancestors.filter(ancestor => instances.contains(ancestor.shape)).head
    val statementStartingAtBlock = statement.stopAt(p => p.parentOption.exists(
      maybeMethod => maybeMethod.shape == method.shape))
    try
    {
      variablesPerStatement(statementStartingAtBlock)
    } catch
      {
        case e: NoSuchElementException => throw StatementWasNotFoundDuringLocalsAnalysis(statement)
      }
  }
}
