package deltas.javac.classes

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.objects.NewByteCodeDelta
import deltas.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import deltas.expression.{ExpressionDelta, NewDelta}
import deltas.expression.NewDelta.NewCall
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.constructor.SuperCallExpression
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object NewToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = { //TODO deze method moet een stuk kleiner kunnen.
    val call: NewCall[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val classInfo: ClassSignature = compiler.findClass(call._type)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val callTypes = callArguments.map(argument => ExpressionDelta.getType(compilation)(argument))

    val methodKey = MethodQuery(classInfo.getQualifiedName, SuperCallExpression.constructorName, callTypes)
    Seq(NewByteCodeDelta.newInstruction(classRef), DuplicateInstructionDelta.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialDelta.invokeSpecial(compiler.getMethodRefIndex(methodKey)))
  }

  override def shape = NewDelta.Shape

  override def description = "Converts new <className>(..) to bytecode"

  override def dependencies = Set(NewDelta, DuplicateInstructionDelta, NewByteCodeDelta, InvokeSpecialDelta)
}
