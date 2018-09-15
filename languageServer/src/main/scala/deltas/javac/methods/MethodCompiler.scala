package deltas.javac.methods

import core.language.exceptions.BadInputException
import core.language.node.Node
import core.deltas.path.{NodePath, PathRoot}
import core.language.Compilation
import deltas.bytecode.types.QualifiedObjectTypeDelta
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods.MethodDelta._
import deltas.javac.statements.ByteCodeStatementSkeleton
import deltas.javac.statements.locals.LocalsAnalysis

case class MethodCompiler(compilation: Compilation, method: Method[Node]) {
  val parameters: Seq[Node] = method.parameters
  val classCompiler: ClassCompiler = JavaClassSkeleton.getClassCompiler(compilation)

  private val initialVariables = getInitialVariables

  val localAnalysis = new LocalsAnalysis(compilation, method)
  private val intermediate = new Method(PathRoot(method)).body
  val firstInstruction: NodePath = intermediate.head
  val variablesPerStatement: Map[NodePath, VariablePool] = localAnalysis.run(firstInstruction, initialVariables)

  def getInitialVariables: VariablePool = {
    var result = VariablePool(compilation)
    if (!method.isStatic)
      result = result.add("this", QualifiedObjectTypeDelta.neww(classCompiler.fullyQualify(classCompiler.currentClassInfo.name)))
    for (parameter <- parameters)
      result = result.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: NodePath) extends BadInputException
  {
    override def toString = s"the following statement is unreachable:\n$statement"
  }

  def getVariables(obj: NodePath): VariablePool = {
    val instances = ByteCodeStatementSkeleton.instances.get(compilation)
    val statement = obj.ancestors.filter(ancestor => instances.contains(ancestor.shape)).head
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
