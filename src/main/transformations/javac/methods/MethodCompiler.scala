package transformations.javac.methods

import core.exceptions.BadInputException
import core.particles.path.Root
import core.particles.{Path$, CompilationState, MetaObject}
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.methods.MethodC._
import transformations.javac.statements.locals.LocalsAnalysis
import transformations.types.{ObjectTypeC, TypeSkeleton}
case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: MetaObject)

case class VariablePool(state: CompilationState, typedVariables: Map[String, MetaObject] = Map.empty) {
  private var variables = Map.empty[String, VariableInfo]
  var offset = 0
  for(typedVariable <- typedVariables)
    privateAdd(typedVariable._1, typedVariable._2)

  def localCount = offset

  def get(name: String) = variables.get(name)

  def apply(name: String) = variables.getOrElse(name, throw new VariableDoesNotExist(name))

  def contains(name: String) = variables.contains(name)

  private def privateAdd(variable: String, _type: MetaObject) {
    variables = variables.updated(variable, new VariableInfo(offset, _type))
    offset += TypeSkeleton.getTypeSize(_type, state)
  }

  def add(variable: String, _type: MetaObject): VariablePool = {
    new VariablePool(state, typedVariables.updated(variable, _type))
  }
}

case class MethodCompiler(state: CompilationState, method: MetaObject) {
  val parameters = getMethodParameters(method)
  val classCompiler = JavaClassSkeleton.getClassCompiler(state)

  private val initialVariables = getInitialVariables

  val localAnalysis = new LocalsAnalysis(state, method)
  val firstInstruction = getMethodBody[Path](new Root(method))(0)
  val variablesPerStatement = localAnalysis.run(firstInstruction, initialVariables)

  def getInitialVariables = {
    var result = new VariablePool(state)
    if (!getMethodStatic(method))
      result = result.add("this", ObjectTypeC.objectType(classCompiler.currentClassInfo.name))
    for (parameter <- parameters)
      result = result.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
    result
  }
}
