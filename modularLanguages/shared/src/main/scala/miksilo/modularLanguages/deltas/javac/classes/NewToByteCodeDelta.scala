package miksilo.modularLanguages.deltas.javac.classes

import core.SolveConstraintsDelta
import miksilo.modularLanguages.core.deltas.path.{ChildPath, NodePath}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.NewByteCodeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{DuplicateInstructionDelta, InvokeSpecialDelta}
import miksilo.modularLanguages.deltas.expression.NewDelta
import miksilo.modularLanguages.deltas.expression.NewDelta.NewCall
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{ClassSignature, JavaClassDelta}
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta.Method
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta

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
