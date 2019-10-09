package deltas.javac.constructor

import core.SolveConstraintsDelta
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.call.CallDelta.Call

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
