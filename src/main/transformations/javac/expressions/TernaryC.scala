package transformations.javac.expressions

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.{InferredStackFrames, LabelledJumps}

import scala.collection.mutable

object TernaryC extends GrammarTransformation {

  object FalseKey

  object TrueKey

  object ConditionKey

  def ternary(condition: MetaObject, trueBranch: MetaObject, falseBranch: MetaObject) = new MetaObject(TernaryKey) {
    data.put(FalseKey, falseBranch)
    data.put(TrueKey, trueBranch)
    data.put(ConditionKey, condition)
  }

  def falseBranch(metaObject: MetaObject) = metaObject(FalseKey).asInstanceOf[MetaObject]

  def trueBranch(metaObject: MetaObject) = metaObject(TrueKey).asInstanceOf[MetaObject]

  object TernaryKey

  def getCondition(metaObject: MetaObject) = {
    metaObject(ConditionKey).asInstanceOf[MetaObject]
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(TernaryKey, _ternary => {
      val condition = TernaryC.getCondition(_ternary)
      val truePath = TernaryC.trueBranch(_ternary)
      val falsePath = TernaryC.falseBranch(_ternary)
      val falseLabelName = state.getUniqueLabel("falseStart")
      val falseTarget = InferredStackFrames.label(falseLabelName)
      val conditionalBranch = LabelledJumps.ifZero(falseLabelName)
      val endLabelName = state.getUniqueLabel("end")
      val end = InferredStackFrames.label(endLabelName)
      val goToEnd = LabelledJumps.goTo(endLabelName)
      val toInstructions = ExpressionC.getToInstructions(state)
      toInstructions(condition) ++
        Seq(conditionalBranch) ++
        toInstructions(truePath) ++
        Seq(goToEnd, falseTarget) ++
        toInstructions(falsePath) ++
        Seq(end)
    })
  }

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters ++= Seq("?", ":")

  object TernaryExpressionGrammar

  override def transformGrammars(grammars: GrammarCatalogue) {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val parseTernary: Grammar = (expressionGrammar <~ "?") ~ (expressionGrammar <~ ":") ~ expressionGrammar ^^ { case cond seqr onTrue seqr onFalse => ternary(cond.asInstanceOf[MetaObject], onTrue.asInstanceOf[MetaObject], onFalse.asInstanceOf[MetaObject])}
    val ternaryGrammar = grammars.create(TernaryExpressionGrammar, parseTernary | expressionGrammar.inner)
    expressionGrammar.inner = ternaryGrammar
  }
}
