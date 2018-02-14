package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.objects.NewByteCodeDelta
import deltas.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import deltas.bytecode.types.UnqualifiedObjectTypeDelta
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.constructor.SuperCallExpression
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.CallDelta.CallArguments
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}

object NewDelta extends ExpressionInstance {

  object Shape extends NodeShape
  object Type extends NodeField

  implicit class NewCall[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(CallArguments).asInstanceOf[Seq[T]])
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val objectGrammar = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    val callArgumentsGrammar = find(CallDelta.CallArgumentsGrammar)
    val newGrammar = "new" ~~> objectGrammar.as(Type) ~ callArgumentsGrammar.as(CallDelta.CallArguments) asNode Shape
    val expressionGrammar = find(ExpressionSkeleton.CoreGrammar)
    expressionGrammar.addOption(newGrammar)
  }

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta, NewByteCodeDelta, InvokeSpecialDelta) //TODO dependencies to CallStaticOrInstanceC can be made more specific. Contracts required.

  override val shape = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    expression(Type).asInstanceOf[NodePath]
  }

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = { //TODO deze method moet een stuk kleiner kunnen.
    val call: NewCall[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val classInfo: ClassSignature = compiler.findClass(call._type)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation)(argument))

    val methodKey = MethodQuery(classInfo.getQualifiedName, SuperCallExpression.constructorName, callTypes)
    Seq(NewByteCodeDelta.newInstruction(classRef), DuplicateInstructionDelta.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialDelta.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  override def description: String = "Enables using the new keyword to create a new object."
}
