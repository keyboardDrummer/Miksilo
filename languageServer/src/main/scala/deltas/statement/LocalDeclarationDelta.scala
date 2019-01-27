package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.exceptions.BadInputException
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.types.TypeSkeleton

object LocalDeclarationDelta extends StatementInstance
  with DeltaWithGrammar {

  import deltas.HasNameDelta._

  object WithoutSemiColon extends GrammarKey

  implicit class LocalDeclaration[T <: NodeLike](val node: T) extends NodeWrapper[T] with HasName[T] {
    def _type: T = node(Type).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(StatementDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val parseDeclaration = create(WithoutSemiColon, typeGrammar.as(Type) ~~ find(Name)) ~< ";" asLabelledNode Shape
    statement.addAlternative(parseDeclaration)
  }

  def declaration(name: String, _type: Node): Node = {
    new Node(Shape, Name -> name, Type -> _type)
  }

  case class VariableAlreadyDefined(variable: String) extends BadInputException
  {
    override def toString = s"variable '$variable' was defined more than once."
  }

  object Shape extends NodeShape
  object Type extends NodeField

  override val shape = Shape

  override def definedVariables(compilation: Compilation, declaration: Node): Map[String, Node] = {
    val localDeclaration: LocalDeclaration[Node] = declaration
    Map(localDeclaration.name -> localDeclaration._type)
  }

  override def description: String = "Enables declaring a local variable."

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val declaration: LocalDeclaration[NodePath] = statement
    val _type = TypeSkeleton.getType(compilation, builder, declaration._type, parentScope)
    builder.declare(declaration.name, parentScope, statement.getSourceElement(Name), Some(_type))
  }
}
