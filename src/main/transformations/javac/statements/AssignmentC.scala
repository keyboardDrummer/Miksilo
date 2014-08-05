package transformations.javac.statements

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.instructions.{StoreAddressC, StoreIntegerC}
import transformations.javac.base.JavaMethodC
import transformations.javac.base.model.JavaTypes.{ArrayTypeKey, IntTypeKey, ObjectTypeKey}
import transformations.javac.expressions.ExpressionC

object AssignmentC extends GrammarTransformation {
  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(AssignmentKey, assignment => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      val value = getAssignmentValue(assignment)
      val valueInstructions = ExpressionC.getToInstructions(state)(value)
      val target = getAssignmentTarget(assignment)
      val variable = methodCompiler.variables(target)
      valueInstructions ++ Seq(variable._type.clazz match {
        case IntTypeKey => StoreIntegerC.integerStore(variable.offset)
        case ObjectTypeKey => StoreAddressC.addressStore(variable.offset)
        case ArrayTypeKey => StoreAddressC.addressStore(variable.offset)
      })
    })
  }

  def getAssignmentTarget(assignment: MetaObject) = assignment(AssignmentTarget).asInstanceOf[String]

  def getAssignmentValue(assignment: MetaObject) = assignment(AssignmentValue).asInstanceOf[MetaObject]

  /** TODO: separate variableC in a expression and package variable. Make variable, assignment and declaration both dependent on some VariablePoolC.
    * Variable and assignment should further depend on expression.
    */
  override def dependencies: Set[Contract] = Set(JavaMethodC, StoreAddressC, StoreIntegerC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val assignmentGrammar: Grammar = (identifier <~ "=") ~ expressionGrammar ^^ { case target seqr value => new MetaObject(AssignmentKey, AssignmentTarget -> target, AssignmentValue -> value)}
    expressionGrammar.inner = expressionGrammar.inner | assignmentGrammar
  }

  object AssignmentKey


  object AssignmentTarget

  object AssignmentValue

}
