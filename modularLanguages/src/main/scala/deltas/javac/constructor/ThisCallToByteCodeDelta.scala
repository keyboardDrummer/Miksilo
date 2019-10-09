package deltas.javac.constructor

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.expressions.ConvertsToByteCodeDelta

object ThisCallToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)
    transformThisCall(classCompiler.currentClass, call, compilation)
  }

  def transformThisCall(program: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    val clazz: JavaClass[Node] = program
    SuperCallToByteCodeExpression.transformToByteCode(call, compilation, program.name)
  }

  override def shape = ThisCallExpression.Shape

  override def description = "Converts calls to this to bytecode."

  override def dependencies = Set(ThisCallExpression, LoadAddressDelta, InvokeSpecialDelta)
}
