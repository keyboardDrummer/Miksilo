package transformations.javac.statements

import core.exceptions.BadInputException
import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.base.{JavaMethodAndClassC, VariablePool}
import transformations.javac.types.TypeC

case class VariableAlreadyDefined(variable: String) extends BadInputException

object DeclarationC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(DeclarationKey, declaration => {
      val methodCompiler = JavaMethodAndClassC.getMethodCompiler(state)
      val variables: VariablePool = methodCompiler.variables
      val name: String = getDeclarationName(declaration)

      if (variables.contains(name))
        throw new VariableAlreadyDefined(name)

      variables.add(name, getDeclarationType(declaration))
      Seq.empty[MetaObject]
    })
  }

  def getDeclarationType(declaration: MetaObject) = declaration(DeclarationType).asInstanceOf[MetaObject]

  def getDeclarationName(declaration: MetaObject) = declaration(DeclarationName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(StatementC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    val parseDeclaration: Grammar = typeGrammar ~ identifier <~ ";" ^^ { case _type seqr name => new MetaObject(DeclarationKey, DeclarationName -> name, DeclarationType -> _type)}
    statement.inner = statement.inner | parseDeclaration
  }

  object DeclarationKey

  object DeclarationName

  object DeclarationType

}
