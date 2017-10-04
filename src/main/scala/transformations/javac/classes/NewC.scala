package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.node._
import core.particles.path.Path
import transformations.bytecode.coreInstructions.objects.NewByteCodeDelta
import transformations.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import transformations.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import transformations.javac.constructor.SuperCallExpression
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.call.{CallC, CallStaticOrInstanceC}
import transformations.bytecode.types.ObjectTypeDelta

object NewC extends ExpressionInstance {

  object NewCallKey extends NodeClass
  object NewObject extends NodeField

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val objectGrammar = grammars.find(ObjectTypeDelta.ObjectTypeJavaGrammar)
    val callArgumentsGrammar = grammars.find(CallC.CallArgumentsGrammar)
    val newGrammar = ("new" ~~> objectGrammar ~ callArgumentsGrammar).
      asNode(NewCallKey, NewObject, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceC, NewByteCodeDelta, InvokeSpecialDelta) //TODO dependencies to CallStaticOrInstanceC can be made more specific. Contracts required.

  override val key = NewCallKey

  override def getType(expression: Path, state: Language): Node = {
    expression(NewObject).asInstanceOf[Path]
  }

  override def toByteCode(expression: Path, state: Language): Seq[Node] = { //TODO deze method moet een stuk kleiner kunnen.
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val objectType = getNewObject(expression)
    val classInfo: ClassSignature = compiler.findClass(objectType)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = CallC.getCallArguments(expression)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(state)(argument))

    val methodKey = new MethodQuery(classInfo.getQualifiedName, SuperCallExpression.constructorName, callTypes)
    Seq(NewByteCodeDelta.newInstruction(classRef), DuplicateInstructionDelta.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialDelta.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  def getNewObject[T <: NodeLike](expression: T): T = {
    expression(NewObject).asInstanceOf[T]
  }

  override def description: String = "Enables using the new keyword to create a new object."
}
