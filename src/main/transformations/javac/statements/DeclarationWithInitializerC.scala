package transformations.javac.statements

import core.grammar.{Grammar, seqr}
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.expressions.ExpressionC
import transformations.javac.statements.DeclarationC.{DeclarationName, DeclarationType}
import transformations.javac.types.TypeC

object DeclarationWithInitializerC extends GrammarTransformation {


  override def dependencies: Set[Contract] = Set(AssignmentC, DeclarationC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(DeclarationWithInitializerKey, declarationWithInitializer => {
      val name: String = DeclarationC.getDeclarationName(declarationWithInitializer)
      val _type = DeclarationC.getDeclarationType(declarationWithInitializer)
      val declaration = DeclarationC.declaration(name, _type)
      val assignment = AssignmentC.assignment(name, getInitializer(declarationWithInitializer))

      val toInstructions = StatementC.getToInstructions(state)
      toInstructions(declaration) ++ toInstructions(assignment) //TODO maybe translate to statements instead of bytecode.
    })
  }

  def getInitializer(withInitializer: MetaObject) = withInitializer(InitializerKey).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val parseDeclarationWithInitializer: Grammar = typeGrammar ~ identifier ~ ("=" ~> expression) <~ ";" ^^ { case _type seqr name seqr initializer => declarationWithInitializer(name.asInstanceOf[String], _type.asInstanceOf[MetaObject], initializer.asInstanceOf[MetaObject])}
    statement.inner = statement.inner | parseDeclarationWithInitializer
  }

  def declarationWithInitializer(name: String, _type: MetaObject, initializer: MetaObject) = new MetaObject(DeclarationWithInitializerKey,
    DeclarationName -> name, DeclarationType -> _type, InitializerKey -> initializer)

  object DeclarationWithInitializerKey

  object InitializerKey

}
