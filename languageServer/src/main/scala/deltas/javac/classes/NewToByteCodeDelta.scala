package deltas.javac.classes

import core.deltas.path.{ChildPath, NodePath}
import core.language.Compilation
import core.language.node.Node
import core.smarts.SolveConstraintsDelta
import deltas.bytecode.coreInstructions.objects.NewByteCodeDelta
import deltas.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import deltas.expression.NewDelta
import deltas.expression.NewDelta.NewCall
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassDelta}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call.CallDelta

object NewToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val call: NewCall[NodePath] = path
    val compiler = JavaClassDelta.getClassCompiler(compilation)
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val classInfo: ClassSignature = compiler.findClass(call._type)
    val classRef = compiler.getClassRef(classInfo)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))

    val method: Method[NodePath] = SolveConstraintsDelta.getDeclarationOfReference(call.node.asInstanceOf[ChildPath])

    val methodRefIndex: Node = CallDelta.getMethodRefIndexFromMethod(method)
    Seq(NewByteCodeDelta.newInstruction(classRef), DuplicateInstructionDelta.duplicate) ++ argumentInstructions ++
      Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def shape = NewDelta.Shape

  override def description = "Converts new <className>(..) to bytecode"

  override def dependencies = Set(NewDelta, DuplicateInstructionDelta, NewByteCodeDelta, InvokeSpecialDelta)
}
