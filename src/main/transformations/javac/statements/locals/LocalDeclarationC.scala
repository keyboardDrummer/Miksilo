package transformations.javac.statements.locals

import core.exceptions.BadInputException
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.statements.{StatementInstance, StatementSkeleton}
import transformations.bytecode.types.TypeSkeleton

object LocalDeclarationC extends StatementInstance {

  def getDeclarationType(declaration: Node) = declaration(DeclarationType).asInstanceOf[Node]

  def getDeclarationName(declaration: Node) = declaration(DeclarationName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val parseDeclaration = typeGrammar ~~ identifier <~ ";" ^^ parseMap(DeclarationKey, DeclarationType, DeclarationName)
    statement.addOption(parseDeclaration)
  }

  def declaration(name: String, _type: Node): Node = {
    new Node(DeclarationKey, DeclarationName -> name, DeclarationType -> _type)
  }

  case class VariableAlreadyDefined(variable: String) extends BadInputException
  {
    override def toString = s"variable '$variable' was defined more than once."
  }

  object DeclarationKey

  object DeclarationName

  object DeclarationType

  override val key: AnyRef = DeclarationKey

  override def toByteCode(declaration: Path, state: CompilationState): Seq[Node] = {
    Seq.empty[Node]
  }

  override def definedVariables(state: CompilationState, declaration: Node): Map[String, Node] = {
    val _type = getDeclarationType(declaration)
    JavaClassSkeleton.fullyQualify(_type, JavaClassSkeleton.getClassCompiler(state))
    val name: String = getDeclarationName(declaration)
    Map(name -> _type)
  }

  override def description: String = "Enables declaring a local variable."
}
