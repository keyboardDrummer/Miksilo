package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.VoidReturnInstructionC
import transformations.javac.base.{MethodAndClassC, MethodCompiler, MethodPart}
import transformations.javac.statements.StatementC

object ReturnVoidC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(MethodAndClassC, VoidReturnInstructionC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(ReturnVoid, (_return: MetaObject) => {
      val methodCompiler = MethodPart.getMethodCompiler(state)
      returnToLines(_return, methodCompiler)
    })
  }

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    Seq(VoidReturnInstructionC.voidReturn)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)

    val returnExpression = "return" <~ ";" ^^ (_ => _return())
    statement.inner = statement.inner | returnExpression
  }

  def _return(): MetaObject = new MetaObject(ReturnVoid)

  object ReturnVoid

}
