package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.{InvokeSpecialC, DuplicateInstructionC}
import transformations.bytecode.coreInstructions.objects.NewByteCodeC
import transformations.javac.constructor.ConstructorC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
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
    val expressionGrammar = grammars.find(ExpressionC.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallC)

  override val key: AnyRef = NewCallKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = {
    expression(NewObject).asInstanceOf[MetaObject]
  }

  override def toByteCode(expression: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val compiler = ClassC.getClassCompiler(state)
    val expressionToInstruction = ExpressionC.getToInstructions(state)
    val objectType = expression(NewObject).asInstanceOf[MetaObject]
    val classInfo: ClassInfo = compiler.findClass(objectType)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = CallC.getCallArguments(expression)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))

    val methodKey = new MethodId(classInfo.getQualifiedName, ConstructorC.constructorName)
    Seq(NewByteCodeC.newInstruction(classRef), DuplicateInstructionC.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialC.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }
}
