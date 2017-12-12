package deltas.javac.statements.locals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import core.deltas.path.{Path, PathRoot, SequenceElement}
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.VariableDelta
import deltas.javac.methods.assignment.AssignmentSkeleton
import deltas.javac.statements.locals.LocalDeclarationDelta.{Name, Type}
import deltas.javac.statements.{ExpressionAsStatementDelta, StatementSkeleton}

object LocalDeclarationWithInitializerC extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, LocalDeclarationDelta)

  def getInitializer(withInitializer: Node) = withInitializer(Initializer).asInstanceOf[Node]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementSkeleton.StatementGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val parseDeclarationWithInitializer = create(Clazz,
      (typeGrammar.as(Type) ~~ identifier.as(Name) ~~ ("=" ~~> expression.as(Initializer)) ~< ";").
      asNode(Clazz))
    statement.addOption(parseDeclarationWithInitializer)
  }

  def declarationWithInitializer(name: String, _type: Node, initializer: Node) = new Node(Clazz,
    Name -> name, Type -> _type, Initializer -> initializer)

  object Clazz extends NodeClass

  object Initializer extends NodeField

  override def description: String = "Enables declaring a local and initializing it in one statement."

  def transformDeclarationWithInitializer(declarationWithInitializer: Path, state: Language): Unit = {
    val name: String = LocalDeclarationDelta.getDeclarationName(declarationWithInitializer)
    val _type = LocalDeclarationDelta.getDeclarationType(declarationWithInitializer)
    val declaration = LocalDeclarationDelta.declaration(name, _type)
    val assignment = AssignmentSkeleton.assignment(VariableDelta.variable(name), getInitializer(declarationWithInitializer))

    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    val originSequence = declarationWithInitializer.asInstanceOf[SequenceElement]
    originSequence.replaceWith(Seq(declaration, assignmentStatement))
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    PathRoot(program).visit(obj => obj.clazz match {
      case Clazz => transformDeclarationWithInitializer(obj, state)
      case _ =>
    })
  }
}
