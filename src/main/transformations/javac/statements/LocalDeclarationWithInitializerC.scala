package transformations.javac.statements

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, CompilationState}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.statements.LocalDeclarationC.{DeclarationName, DeclarationType}
import transformations.types.TypeSkeleton

object LocalDeclarationWithInitializerC extends StatementInstance {

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

  override val key: AnyRef = DeclarationWithInitializerKey

  override def toByteCode(declarationWithInitializer: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val name: String = LocalDeclarationC.getDeclarationName(declarationWithInitializer)
    val _type = LocalDeclarationC.getDeclarationType(declarationWithInitializer)
    val declaration = LocalDeclarationC.declaration(name, _type)
    val assignment = AssignmentSkeleton.assignment(VariableC.variable(name), getInitializer(declarationWithInitializer))

    val toInstructions = StatementSkeleton.getToInstructions(state)
    toInstructions(declaration) ++ toInstructions(ExpressionAsStatementC.asStatement(assignment)) //TODO maybe translate to statements instead of bytecode.
  }

  override def description: String = "Enables declaring a local and initializing it in one statement."
}
