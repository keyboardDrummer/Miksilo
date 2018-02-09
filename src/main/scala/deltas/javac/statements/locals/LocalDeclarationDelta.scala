package deltas.javac.statements.locals

import core.deltas._
import core.deltas.exceptions.BadInputException
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{NodePath, NodePathRoot, Path}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements.{StatementInstance, StatementSkeleton}

object LocalDeclarationDelta extends StatementInstance {

  implicit class LocalDeclaration[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def name: String = node.getValue(Name)
  }

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

  object DeclarationKey extends NodeShape
  object Name extends NodeField
  object Type extends NodeField

  override val key = DeclarationKey

  override def toByteCode(declaration: NodePath, compilation: Compilation): Seq[Node] = {
    Seq.empty[Node]
  }

  override def definedVariables(compilation: Compilation, declaration: Node): Map[String, Node] = {
    val localDeclaration = LocalDeclaration[NodePath](NodePathRoot(declaration))
    val _type = localDeclaration._type
    JavaClassSkeleton.fullyQualify(_type, JavaClassSkeleton.getClassCompiler(compilation))
    val name: String = declaration.name
    Map(name -> _type)
  }

  override def description: String = "Enables declaring a local variable."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val _languageType = statement(Type).asInstanceOf[NodePath]
    val _type = TypeSkeleton.getType(compilation, builder, _languageType, parentScope)
    builder.declare(statement.name, statement(Name).asInstanceOf[Path], parentScope, Some(_type))
  }
}
