package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, CompilationState}
import transformations.bytecode.coreInstructions.objects.NewByteCodeC
import transformations.bytecode.coreInstructions.{DuplicateInstructionC, InvokeSpecialC}
import transformations.javac.constructor.SuperCallExpression
import transformations.javac.expressions.{ExpressionSkeleton, ExpressionInstance}
import transformations.javac.methods.CallC
import transformations.types.ObjectTypeC

object NewC extends ExpressionInstance {

  object NewCallKey
  object NewObject

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val objectGrammar = grammars.find(ObjectTypeC.ObjectTypeGrammar)
    val callArgumentsGrammar = grammars.find(CallC.CallArgumentsGrammar)
    val newGrammar = "new" ~> objectGrammar ~ callArgumentsGrammar ^^
      parseMap(NewCallKey, NewObject, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallC)

  override val key: AnyRef = NewCallKey

  override def getType(expression: MetaObject, state: CompilationState): MetaObject = {
    expression(NewObject).asInstanceOf[MetaObject]
  }

  override def toByteCode(expression: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val objectType = expression(NewObject).asInstanceOf[MetaObject]
    val classInfo: ClassInfo = compiler.findClass(objectType)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = CallC.getCallArguments(expression)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))

    val methodKey = new MethodId(classInfo.getQualifiedName, SuperCallExpression.constructorName)
    Seq(NewByteCodeC.newInstruction(classRef), DuplicateInstructionC.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialC.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  override def description: String = "Enables using the new keyword to create a new object."
}
