package deltas.javac.statements.locals

import core.particles._
import core.particles.exceptions.BadInputException
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements.{StatementInstance, StatementSkeleton}

object LocalDeclarationC extends StatementInstance {

  def getDeclarationType(declaration: Node) = declaration(Type).asInstanceOf[Node]

  def getDeclarationName(declaration: Node) = declaration(Name).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementSkeleton.StatementGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val parseDeclaration = typeGrammar.as(Type) ~~ identifier.as(Name) ~< ";" asNode DeclarationKey
    statement.addOption(parseDeclaration)
  }

  def declaration(name: String, _type: Node): Node = {
    new Node(DeclarationKey, Name -> name, Type -> _type)
  }

  case class VariableAlreadyDefined(variable: String) extends BadInputException
  {
    override def toString = s"variable '$variable' was defined more than once."
  }

  object DeclarationKey extends NodeClass
  object Name extends NodeField
  object Type extends NodeField

  override val key = DeclarationKey

  override def toByteCode(declaration: Path, compilation: Compilation): Seq[Node] = {
    Seq.empty[Node]
  }

  override def definedVariables(compilation: Compilation, declaration: Node): Map[String, Node] = {
    val _type = getDeclarationType(declaration)
    JavaClassSkeleton.fullyQualify(_type, JavaClassSkeleton.getClassCompiler(compilation))
    val name: String = getDeclarationName(declaration)
    Map(name -> _type)
  }

  override def description: String = "Enables declaring a local variable."
}
