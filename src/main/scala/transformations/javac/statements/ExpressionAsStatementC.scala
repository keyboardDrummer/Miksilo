package transformations.javac.statements

import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.Path
import core.particles.Language
import transformations.bytecode.coreInstructions.{Pop2Delta, PopDelta}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.bytecode.types.TypeSkeleton

object ExpressionAsStatementC extends StatementInstance {

  object ExpressionAsStatementKey extends NodeClass

  object ExpressionKey extends NodeField

  def create(expression: Node): Node = new Node(ExpressionAsStatementKey, ExpressionKey -> expression)

  override val key = ExpressionAsStatementKey

  override def toByteCode(statement: Path, state: Language): Seq[Node] = {
    val expression = getExpression(statement)
    val _type = ExpressionSkeleton.getType(state)(expression)
    val extra = TypeSkeleton.getTypeSize(_type, state) match {
      case 0 => Seq.empty
      case 1 => Seq(PopDelta.pop)
      case 2 => Seq(Pop2Delta.pop2)
    }
    ExpressionSkeleton.getToInstructions(state)(expression) ++ extra
  }

  def getExpression[T <: NodeLike](statement: T): T = {
    statement(ExpressionKey).asInstanceOf[T]
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val inner = expressionGrammar ~< ";"
    val expressionAsStatement = nodeGrammar(inner, ExpressionAsStatementKey, ExpressionKey)
    statementGrammar.addOption(expressionAsStatement)
  }

  override def description: String = "Enables using an expression as a statement."

}
