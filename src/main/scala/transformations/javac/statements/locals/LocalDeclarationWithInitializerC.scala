package transformations.javac.statements.locals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.{Path, PathRoot, SequenceElement}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.statements.locals.LocalDeclarationC.{DeclarationName, DeclarationType}
import transformations.javac.statements.{ExpressionAsStatementC, StatementSkeleton}
import transformations.bytecode.types.TypeSkeleton

object LocalDeclarationWithInitializerC extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, LocalDeclarationC)

  def getInitializer(withInitializer: Node) = withInitializer(InitializerKey).asInstanceOf[Node]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseDeclarationWithInitializer = grammars.create(this, (typeGrammar ~~ identifier ~~ ("=" ~~> expression) <~ ";").
      asNode(DeclarationWithInitializerKey, DeclarationType, DeclarationName, InitializerKey))
    statement.addOption(parseDeclarationWithInitializer)
  }

  def declarationWithInitializer(name: String, _type: Node, initializer: Node) = new Node(DeclarationWithInitializerKey,
    DeclarationName -> name, DeclarationType -> _type, InitializerKey -> initializer)

  object DeclarationWithInitializerKey extends Key

  object InitializerKey extends Key

  override def description: String = "Enables declaring a local and initializing it in one statement."

  def transformDeclarationWithInitializer(declarationWithInitializer: Path, state: CompilationState): Unit = {
    val name: String = LocalDeclarationC.getDeclarationName(declarationWithInitializer)
    val _type = LocalDeclarationC.getDeclarationType(declarationWithInitializer)
    val declaration = LocalDeclarationC.declaration(name, _type)
    val assignment = AssignmentSkeleton.assignment(VariableC.variable(name), getInitializer(declarationWithInitializer))

    val assignmentStatement = ExpressionAsStatementC.create(assignment)
    val originSequence = declarationWithInitializer.asInstanceOf[SequenceElement]
    originSequence.replaceWith(Seq(declaration, assignmentStatement))
  }

  override def transform(program: Node, state: CompilationState): Unit = {
    new PathRoot(program).visit(obj => obj.clazz match {
      case DeclarationWithInitializerKey => transformDeclarationWithInitializer(obj, state)
      case _ =>
    })
  }
}
