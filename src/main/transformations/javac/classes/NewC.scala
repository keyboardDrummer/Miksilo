package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.node.{Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.objects.NewByteCodeC
import transformations.bytecode.coreInstructions.{DuplicateInstructionC, InvokeSpecialC}
import transformations.javac.constructor.SuperCallExpression
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.call.{CallStaticOrInstanceC, CallC}
import transformations.bytecode.types.ObjectTypeC

object NewC extends ExpressionInstance {

  object NewCallKey
  object NewObject

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val objectGrammar = grammars.find(ObjectTypeC.ObjectTypeJavaGrammar)
    val callArgumentsGrammar = grammars.find(CallC.CallArgumentsGrammar)
    val newGrammar = "new" ~~> objectGrammar ~ callArgumentsGrammar ^^
      parseMap(NewCallKey, NewObject, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceC, NewByteCodeC, InvokeSpecialC) //TODO dependencies to CallStaticOrInstanceC can be made more specific. Contracts required.

  override val key: AnyRef = NewCallKey

  override def getType(expression: Path, state: CompilationState): Node = {
    expression(NewObject).asInstanceOf[Path]
  }

  override def toByteCode(expression: Path, state: CompilationState): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val objectType = getNewObject(expression)
    val classInfo: ClassInfo = compiler.findClass(objectType)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = CallC.getCallArguments(expression)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))

    val methodKey = new MethodId(classInfo.getQualifiedName, SuperCallExpression.constructorName)
    Seq(NewByteCodeC.newInstruction(classRef), DuplicateInstructionC.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialC.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  def getNewObject[T <: NodeLike](expression: T): T = {
    expression(NewObject).asInstanceOf[T]
  }

  override def description: String = "Enables using the new keyword to create a new object."
}
