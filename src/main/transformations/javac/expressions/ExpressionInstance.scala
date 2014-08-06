package transformations.javac.expressions

import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}

trait ExpressionInstance extends GrammarTransformation {
  val key: AnyRef

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(key, (expression: MetaObject) => toByteCode(expression, state))
    ExpressionC.getGetTypeRegistry(state).put(key, (expression: MetaObject) => getType(expression, state))
  }

  def toByteCode(expression: MetaObject, state: TransformationState): Seq[MetaObject]

  def getType(expression: MetaObject, state: TransformationState): MetaObject

  override def dependencies: Set[Contract] = Set(ExpressionC)
}
