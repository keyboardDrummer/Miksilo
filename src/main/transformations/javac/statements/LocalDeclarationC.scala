package transformations.javac.statements

import core.exceptions.BadInputException
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.methods.{VariablePool, MethodC}
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

  override def toByteCode(declaration: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val variables: VariablePool = methodCompiler.variables
    val name: String = getDeclarationName(declaration)

    if (variables.contains(name))
      throw new VariableAlreadyDefined(name)

    val declarationType: MetaObject = getDeclarationType(declaration)
    JavaClassSkeleton.fullyQualify(declarationType, JavaClassSkeleton.getClassCompiler(state))
    variables.add(name, declarationType)
    Seq.empty[MetaObject]
  }

  override def description: String = "Enables declaring a local variable."
}
