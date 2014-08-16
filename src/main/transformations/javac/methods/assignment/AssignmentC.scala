package transformations.javac.methods.assignment

import core.grammar.{Grammar}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.{LoadAddressC, LoadIntegerC, StoreAddressC, StoreIntegerC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.javac.methods.MethodC
import transformations.types.ArrayTypeC.ArrayTypeKey
import transformations.types.IntTypeC.IntTypeKey
import transformations.types.ObjectTypeC.ObjectTypeKey

object AssignmentC extends ExpressionInstance {

  def getAssignmentTarget(assignment: MetaObject) = assignment(AssignmentTarget).asInstanceOf[String]

  def getAssignmentValue(assignment: MetaObject) = assignment(AssignmentValue).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(MethodC, StoreAddressC, StoreIntegerC, AssignmentPrecedence)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val assignmentGrammar: Grammar = (identifier <~ "=") ~ expressionGrammar ^^ { case target seqr value => assignment(target.asInstanceOf[String], value.asInstanceOf[MetaObject])}
    expressionGrammar.inner = expressionGrammar.inner | assignmentGrammar
  }

  def assignment(target: String, value: MetaObject) = new MetaObject(AssignmentKey, AssignmentTarget -> target, AssignmentValue -> value)

  object AssignmentKey

  object AssignmentTarget

  object AssignmentValue

  override val key: AnyRef = AssignmentKey

  override def getType(assignment: MetaObject, state: TransformationState): MetaObject = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val target = getAssignmentTarget(assignment)
    val variable = methodCompiler.variables(target)
    variable._type
  }

  override def toByteCode(assignment: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val value = getAssignmentValue(assignment)
    val valueInstructions = ExpressionC.getToInstructions(state)(value)
    val target = getAssignmentTarget(assignment)
    val variable = methodCompiler.variables(target)
    valueInstructions ++ Seq(variable._type.clazz match {
      case IntTypeKey => StoreIntegerC.integerStore(variable.offset)
      case ObjectTypeKey => StoreAddressC.addressStore(variable.offset)
      case ArrayTypeKey => StoreAddressC.addressStore(variable.offset)
    }) ++ Seq(variable._type.clazz match {
      case IntTypeKey => LoadIntegerC.integerLoad(variable.offset)
      case ObjectTypeKey => LoadAddressC.addressLoad(variable.offset)
      case ArrayTypeKey => LoadAddressC.addressLoad(variable.offset)
    })
  }
}
