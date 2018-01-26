package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas._
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import deltas.bytecode.coreInstructions.objects.NewByteCodeDelta
import deltas.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.constructor.SuperCallExpression
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.{CallC, CallStaticOrInstanceDelta}
import deltas.bytecode.types.ObjectTypeDelta

object NewC extends ExpressionInstance {

  object NewCallKey extends NodeShape
  object NewObject extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val objectGrammar = find(ObjectTypeDelta.ObjectTypeJavaGrammar)
    val callArgumentsGrammar = find(CallC.CallArgumentsGrammar)
    val newGrammar = "new" ~~> objectGrammar.as(NewObject) ~ callArgumentsGrammar.as(CallC.CallArguments) asNode NewCallKey
    val expressionGrammar = find(ExpressionSkeleton.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta, NewByteCodeDelta, InvokeSpecialDelta) //TODO dependencies to CallStaticOrInstanceC can be made more specific. Contracts required.

  override val key = NewCallKey

  override def getType(expression: Path, compilation: Compilation): Node = {
    expression(NewObject).asInstanceOf[Path]
  }

  override def toByteCode(expression: Path, compilation: Compilation): Seq[Node] = { //TODO deze method moet een stuk kleiner kunnen.
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val objectType = getNewObject(expression)
    val classInfo: ClassSignature = compiler.findClass(objectType)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = CallC.getCallArguments(expression)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation)(argument))

    val methodKey = MethodQuery(classInfo.getQualifiedName, SuperCallExpression.constructorName, callTypes)
    Seq(NewByteCodeDelta.newInstruction(classRef), DuplicateInstructionDelta.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialDelta.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  def getNewObject[T <: NodeLike](expression: T): T = {
    expression(NewObject).asInstanceOf[T]
  }

  override def description: String = "Enables using the new keyword to create a new object."
}
