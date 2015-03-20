package transformations.javac.methods

import core.exceptions.BadInputException
import core.particles.node.Node
import core.particles.path.{Path, Root}
import core.particles.CompilationState
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.methods.MethodC._
import transformations.javac.statements.StatementSkeleton
import transformations.javac.statements.locals.LocalsAnalysis
import transformations.types.{ObjectTypeC, TypeSkeleton}
case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: Node)

case class VariablePool(state: CompilationState, typedVariables: Map[String, Node] = Map.empty) {
  private var variables = Map.empty[String, VariableInfo]
  var offset = 0
  for(typedVariable <- typedVariables)
    privateAdd(typedVariable._1, typedVariable._2)

  def localCount = offset

  def get(name: String) = variables.get(name)

  def apply(name: String) = variables.getOrElse(name, throw new VariableDoesNotExist(name))

  def contains(name: String) = variables.contains(name)

  private def privateAdd(variable: String, _type: Node) {
    variables = variables.updated(variable, new VariableInfo(offset, _type))
    offset += TypeSkeleton.getTypeSize(_type, state)
  }

  def add(variable: String, _type: Node): VariablePool = {
    new VariablePool(state, typedVariables.updated(variable, _type))
  }
}

case class MethodCompiler(state: CompilationState, method: Node) {
  val parameters = getMethodParameters(method)
  val classCompiler = JavaClassSkeleton.getClassCompiler(state)

  private val initialVariables = getInitialVariables

  val localAnalysis = new LocalsAnalysis(state, method)
  val firstInstruction = getMethodBody[Path](new Root(method)).head
  val variablesPerStatement = localAnalysis.run(firstInstruction, initialVariables)

  def getInitialVariables = {
    var result = new VariablePool(state)
    if (!getMethodStatic(method))
      result = result.add("this", ObjectTypeC.objectType(classCompiler.currentClassInfo.name))
    for (parameter <- parameters)
      result = result.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: Path) extends Exception
  {
    override def toString = s"the following statement was not found during locals analysis:\n$statement"
  }

  def getVariables(obj: Path): VariablePool = {
    val instances = StatementSkeleton.getState(state).instances
    val statement = obj.ancestors.filter(ancestor => instances.contains(ancestor.clazz)).head
    val variablesPerStatement: Map[Path, VariablePool] = MethodC.getMethodCompiler(state).variablesPerStatement
    try
    {
      variablesPerStatement(statement)
    } catch
      {
        case e: NoSuchElementException => throw new StatementWasNotFoundDuringLocalsAnalysis(statement)
      }
  }
}
