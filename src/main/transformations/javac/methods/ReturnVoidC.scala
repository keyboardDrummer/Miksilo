package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.VoidReturnInstructionC
import transformations.javac.statements.{StatementSkeleton, StatementInstance}

object ReturnVoidC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, VoidReturnInstructionC)

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    Seq(VoidReturnInstructionC.voidReturn)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = ("return" ~ ";") ~> produce(_return)
    statement.inner = statement.inner | returnExpression
  }

  def _return: MetaObject = new MetaObject(ReturnVoidKey)

  object ReturnVoidKey

  override val key: AnyRef = ReturnVoidKey

  override def toByteCode(_return: MetaObject, state: CompilationState): Seq[MetaObject] = {
    Seq(VoidReturnInstructionC.voidReturn)
  }

  override def description: String = "Allows returning void."
}
