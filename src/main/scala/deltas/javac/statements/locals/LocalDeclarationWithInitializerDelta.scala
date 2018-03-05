package deltas.javac.statements.locals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.{NodePath, PathRoot, SequenceElement}
import core.language.{Compilation, Language}
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.VariableDelta
import deltas.javac.methods.assignment.AssignmentSkeleton
import deltas.javac.statements.locals.LocalDeclarationDelta.{LocalDeclaration, Name, Type}
import deltas.javac.statements.{ExpressionAsStatementDelta, StatementSkeleton}

object LocalDeclarationWithInitializerDelta extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, LocalDeclarationDelta)

  implicit class LocalDeclarationWithInitializer[T <: NodeLike](node: T) extends LocalDeclaration[T](node) {
    def initializer: T = node(Initializer).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementSkeleton.StatementGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val parseDeclarationWithInitializer = create(Shape,
      (typeGrammar.as(Type) ~~ identifier.as(Name) ~~ ("=" ~~> expression.as(Initializer)) ~< ";").
      asNode(Shape))
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
    val target = VariableDelta.variable(name)
    val assignment = AssignmentSkeleton.Shape.createWithSource(
      AssignmentSkeleton.Target -> target,
      AssignmentSkeleton.Value -> path.getWithSource(Initializer))
    path.removeField(Initializer)

    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    val originSequence = withInitializer.node.asInstanceOf[SequenceElement]
    originSequence.replaceWith(Seq(path.current, assignmentStatement))
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, obj => transformDeclarationWithInitializer(obj, compilation))
  }
}
