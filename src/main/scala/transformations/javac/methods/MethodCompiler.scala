package transformations.javac.methods

import core.particles.exceptions.BadInputException
import core.particles.node.Node
import core.particles.path.{Path, PathRoot}
import core.particles.{Compilation, Language}
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.methods.MethodDelta._
import transformations.javac.statements.StatementSkeleton
import transformations.javac.statements.locals.LocalsAnalysis
import transformations.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import transformations.javac.classes.ClassCompiler

case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: Node)

case class VariablePool(state: Language, typedVariables: Map[String, Node] = Map.empty) {
  private var variables = Map.empty[String, VariableInfo]
  var offset = 0
  for(typedVariable <- typedVariables)
    privateAdd(typedVariable._1, typedVariable._2)

  def localCount = offset

  def get(name: String) = variables.get(name)

  def apply(name: String) = variables.getOrElse(name, throw VariableDoesNotExist(name))

  def contains(name: String) = variables.contains(name)

  private def privateAdd(variable: String, _type: Node) {
    variables = variables.updated(variable, VariableInfo(offset, _type))
    offset += TypeSkeleton.getTypeSize(_type, state)
  }

  def add(variable: String, _type: Node): VariablePool = {
    VariablePool(state, typedVariables.updated(variable, _type))
  }
}

case class MethodCompiler(compilation: Compilation, method: Node) {
  val parameters: Seq[Node] = getMethodParameters(method)
  val classCompiler: ClassCompiler = JavaClassSkeleton.getClassCompiler(compilation)

  private val initialVariables = getInitialVariables

  val localAnalysis = new LocalsAnalysis(compilation, method)
  private val intermediate = getMethodBody[Path](PathRoot(method))
  val firstInstruction: Path = intermediate.head
  val variablesPerStatement: Map[Path, VariablePool] = localAnalysis.run(firstInstruction, initialVariables)

  def getInitialVariables: VariablePool = {
    var result = VariablePool(compilation)
    if (!getMethodStatic(method))
      result = result.add("this", ObjectTypeDelta.objectType(classCompiler.currentClassInfo.name))
    for (parameter <- parameters)
      result = result.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: Path) extends BadInputException
  {
    override def toString = s"the following statement is unreachable:\n$statement"
  }

  def getVariables(obj: Path): VariablePool = {
    val instances = StatementSkeleton.getRegistry(compilation).instances
    val statement = obj.ancestors.filter(ancestor => instances.contains(ancestor.clazz)).head
    val variablesPerStatement: Map[Path, VariablePool] = MethodDelta.getMethodCompiler(compilation).variablesPerStatement
    try
    {
      variablesPerStatement(statement)
    } catch
      {
        case e: NoSuchElementException => throw StatementWasNotFoundDuringLocalsAnalysis(statement)
      }
  }
}
