package miksilo.modularLanguages.deltas.javac.constructor

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InvokeSpecialDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.classes.constructor.SuperCallExpression
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.method.MethodDelta.Method
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta.Call

object SuperCallToByteCodeExpression extends ConvertsToByteCodeDelta {
  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)
    transformSuperCall(classCompiler.currentClass, call, compilation)
  }

  def transformSuperCall(clazzNode: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    val clazz: JavaClass[Node] = clazzNode
    transformToByteCode(call, compilation, clazz.parent.get)
  }

  def transformToByteCode(path: NodePath, compilation: Compilation, className: String): Seq[Node] = {
    val call: Call[NodePath] = path
    val callArguments = call.arguments

    val method: Method[NodePath] = SolveConstraintsDelta.getDeclarationOfReference(call.node)

    val methodRefIndex = CallDelta.getMethodRefIndexFromMethod(method)
    val argumentInstructions = callArguments.flatMap(argument => ToByteCodeSkeleton.getToInstructions(compilation)(argument))
    Seq(LoadAddressDelta.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def shape = SuperCallExpression.Shape

  override def description = "Converts call to super to bytecode"

  override def dependencies = Set(SuperCallExpression, InvokeSpecialDelta, LoadAddressDelta)
}
