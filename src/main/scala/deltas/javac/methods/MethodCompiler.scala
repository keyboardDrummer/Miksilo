package deltas.javac.methods

import core.deltas.exceptions.BadInputException
import core.deltas.node.Node
import core.deltas.path.{NodePath, NodePathRoot}
import core.deltas.Compilation
import core.language.Language
import core.nabl.BindingsAndTypes
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods.MethodDelta._
import deltas.javac.statements.StatementSkeleton
import deltas.javac.statements.locals.LocalsAnalysis
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import deltas.javac.classes.ClassCompiler

case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: Node)

case class VariablePool(language: Language, typedVariables: Map[String, Node] = Map.empty) {
  private var variables = Map.empty[String, VariableInfo]
  var offset = 0
  for(typedVariable <- typedVariables)
    privateAdd(typedVariable._1, typedVariable._2)

  def localCount: Int = offset

  def get(name: String): Option[VariableInfo] = variables.get(name)

  def apply(name: String): VariableInfo = variables.getOrElse(name, throw VariableDoesNotExist(name))

  def contains(name: String): Boolean = variables.contains(name)

  private def privateAdd(variable: String, _type: Node) {
    variables = variables.updated(variable, VariableInfo(offset, _type))
    offset += TypeSkeleton.getTypeSize(_type, language)
  }

  def add(variable: String, _type: Node): VariablePool = {
    VariablePool(language, typedVariables.updated(variable, _type))
  }
}

case class MethodCompiler(compilation: Compilation, method: Method[Node]) {
  val parameters: Seq[Node] = method.parameters
  val classCompiler: ClassCompiler = JavaClassSkeleton.getClassCompiler(compilation)

  private val initialVariables = getInitialVariables

  val localAnalysis = new LocalsAnalysis(compilation, method)
  private val intermediate = new Method(NodePathRoot(method)).body
  val firstInstruction: NodePath = intermediate.head
  val variablesPerStatement: Map[NodePath, VariablePool] = localAnalysis.run(firstInstruction, initialVariables)

  def getInitialVariables: VariablePool = {
    var result = VariablePool(compilation)
    if (!method.isStatic)
      result = result.add("this", ObjectTypeDelta.objectType(classCompiler.currentClassInfo.name))
    for (parameter <- parameters)
      result = result.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
    result
  }

  case class StatementWasNotFoundDuringLocalsAnalysis(statement: NodePath) extends BadInputException
  {
    override def toString = s"the following statement is unreachable:\n$statement"
  }

  var bindingsAndTypes: BindingsAndTypes = ???

  def getVariables(obj: NodePath): VariablePool = {
    val instances = StatementSkeleton.getRegistry(compilation).instances
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
