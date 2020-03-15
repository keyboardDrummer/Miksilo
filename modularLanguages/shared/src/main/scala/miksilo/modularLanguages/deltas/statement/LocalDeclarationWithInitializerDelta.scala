package miksilo.modularLanguages.deltas.statement

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{NodeChildPath, NodePath, NodeSequenceElement, PathRoot}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, VariableDelta}
import miksilo.modularLanguages.deltas.javac.statements.ExpressionAsStatementDelta
import miksilo.modularLanguages.deltas.statement.LocalDeclarationDelta.{LocalDeclaration, Type}
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta
import miksilo.modularLanguages.deltas.HasNameDelta.Name

object LocalDeclarationWithInitializerDelta extends DeltaWithGrammar with DeltaWithPhase {


  override def dependencies: Set[Contract] = Set(SimpleAssignmentDelta, ExpressionAsStatementDelta, LocalDeclarationDelta)

  implicit class LocalDeclarationWithInitializer[T <: NodeLike](node: T) extends LocalDeclaration[T](node) {
    def initializer: T = node(Initializer).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val typeName = find(LocalDeclarationDelta.WithoutSemiColon)
    val parseDeclarationWithInitializer = typeName ~~ ("=" ~~> expression.as(Initializer)) ~< ";" asLabelledNode Shape
    statement.addAlternative(parseDeclarationWithInitializer)
  }

  def declarationWithInitializer(name: String, _type: Node, initializer: Node) = new Node(Shape,
    Name -> name, Type -> _type, Initializer -> initializer)

  object Shape extends NodeShape

  object Initializer extends NodeField

  override def description: String = "Enables declaring a local and initializing it in one statement."

  def transformDeclarationWithInitializer(path: NodePath, state: Language): Unit = {
    val withInitializer: LocalDeclarationWithInitializer[NodePath] = path
    val name: String = withInitializer.name
    path.shape = LocalDeclarationDelta.Shape
    val target = VariableDelta.neww(name)
    val assignment = SimpleAssignmentDelta.Shape.createWithData(
      SimpleAssignmentDelta.Target -> target,
      SimpleAssignmentDelta.Value -> path.getFieldData(Initializer))
    path.removeField(Initializer)

    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    val statements = Seq(path.current, assignmentStatement)
    withInitializer.node match {
      case sequenceElement: NodeSequenceElement =>
        sequenceElement.replaceWith(statements)
      case element: NodeChildPath =>
        throw new Exception("Local declaration with initialization must occur in a sequence context.")
    }
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, obj => transformDeclarationWithInitializer(obj, compilation))
  }
}
