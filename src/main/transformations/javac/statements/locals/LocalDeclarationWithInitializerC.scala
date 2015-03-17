package transformations.javac.statements.locals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.statements.locals.LocalDeclarationC.{DeclarationName, DeclarationType}
import transformations.javac.statements.{ExpressionAsStatementC, StatementSkeleton}
import transformations.types.TypeSkeleton

object LocalDeclarationWithInitializerC extends ParticleWithGrammar with ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, LocalDeclarationC)

  def getInitializer(withInitializer: MetaObject) = withInitializer(InitializerKey).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.TypeGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseDeclarationWithInitializer = typeGrammar ~~ identifier ~~ ("=" ~~> expression) <~ ";" ^^
      parseMap(DeclarationWithInitializerKey, DeclarationType, DeclarationName, InitializerKey)
    statement.addOption(parseDeclarationWithInitializer)
  }

  def declarationWithInitializer(name: String, _type: MetaObject, initializer: MetaObject) = new MetaObject(DeclarationWithInitializerKey,
    DeclarationName -> name, DeclarationType -> _type, InitializerKey -> initializer)

  object DeclarationWithInitializerKey

  object InitializerKey

  override def description: String = "Enables declaring a local and initializing it in one statement."

  def transformDeclarationWithInitializer(declarationWithInitializer: MetaObjectWithOrigin, state: CompilationState): Unit = {
    val name: String = LocalDeclarationC.getDeclarationName(declarationWithInitializer)
    val _type = LocalDeclarationC.getDeclarationType(declarationWithInitializer)
    val declaration = new MetaObjectWithOrigin(LocalDeclarationC.declaration(name, _type),declarationWithInitializer.origin)
    val assignment = AssignmentSkeleton.assignment(VariableC.variable(name), getInitializer(declarationWithInitializer))

    val assignmentStatement = new MetaObjectWithOrigin(ExpressionAsStatementC.asStatement(assignment), declarationWithInitializer.origin)
    val originSequence = declarationWithInitializer.origin.asInstanceOf[SequenceSelection]
    originSequence.replaceWith(Seq(declaration.obj, assignmentStatement.obj))
  }

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    new MetaObjectWithOrigin(program).transform[MetaObjectWithOrigin](obj => obj.clazz match {
      case DeclarationWithInitializerKey => transformDeclarationWithInitializer(obj, state)
      case _ =>
    })
  }
}
