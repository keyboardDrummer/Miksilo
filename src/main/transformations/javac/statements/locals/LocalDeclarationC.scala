package transformations.javac.statements.locals

import core.exceptions.BadInputException
import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.statements.{StatementInstance, StatementSkeleton}
import transformations.types.TypeSkeleton

object LocalDeclarationC extends StatementInstance {

  def getDeclarationType(declaration: MetaObject) = declaration(DeclarationType).asInstanceOf[MetaObject]

  def getDeclarationName(declaration: MetaObject) = declaration(DeclarationName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.TypeGrammar)
    val parseDeclaration = typeGrammar ~~ identifier <~ ";" ^^ parseMap(DeclarationKey, DeclarationType, DeclarationName)
    statement.addOption(parseDeclaration)
  }

  def declaration(name: String, _type: MetaObject): MetaObject = {
    new MetaObject(DeclarationKey, DeclarationName -> name, DeclarationType -> _type)
  }

  case class VariableAlreadyDefined(variable: String) extends BadInputException
  {
    override def toString = s"variable '$variable' was defined more than once."
  }

  object DeclarationKey

  object DeclarationName

  object DeclarationType

  override val key: AnyRef = DeclarationKey

  override def toByteCode(declaration: MetaObjectWithOrigin, state: CompilationState): Seq[MetaObject] = {
    Seq.empty[MetaObject]
  }

  override def definedVariables(state: CompilationState, declaration: MetaObject): Map[String, MetaObject] = {
    val _type = getDeclarationType(declaration)
    JavaClassSkeleton.fullyQualify(_type, JavaClassSkeleton.getClassCompiler(state))
    val name: String = getDeclarationName(declaration)
    Map(name -> _type)
  }

  override def description: String = "Enables declaring a local variable."
}
