package transformations.javac.statements

import core.grammar._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.expressions.ExpressionC
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentC
import transformations.javac.statements.DeclarationC.{DeclarationName, DeclarationType}
import transformations.types.TypeC

object DeclarationWithInitializerC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(AssignmentC, DeclarationC)

  def getInitializer(withInitializer: MetaObject) = withInitializer(InitializerKey).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val parseDeclarationWithInitializer: Grammar = typeGrammar ~ identifier ~ ("=" ~> expression) <~ ";" ^^ { case _type ~ name ~ initializer => declarationWithInitializer(name.asInstanceOf[String], _type.asInstanceOf[MetaObject], initializer.asInstanceOf[MetaObject])}
    statement.inner = statement.inner | parseDeclarationWithInitializer
  }

  def declarationWithInitializer(name: String, _type: MetaObject, initializer: MetaObject) = new MetaObject(DeclarationWithInitializerKey,
    DeclarationName -> name, DeclarationType -> _type, InitializerKey -> initializer)

  object DeclarationWithInitializerKey

  object InitializerKey

  override val key: AnyRef = DeclarationWithInitializerKey

  override def toByteCode(declarationWithInitializer: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val name: String = DeclarationC.getDeclarationName(declarationWithInitializer)
    val _type = DeclarationC.getDeclarationType(declarationWithInitializer)
    val declaration = DeclarationC.declaration(name, _type)
    val assignment = AssignmentC.assignment(VariableC.variable(name), getInitializer(declarationWithInitializer))

    val toInstructions = StatementC.getToInstructions(state)
    toInstructions(declaration) ++ toInstructions(ExpressionAsStatementC.asStatement(assignment)) //TODO maybe translate to statements instead of bytecode.
  }
}
