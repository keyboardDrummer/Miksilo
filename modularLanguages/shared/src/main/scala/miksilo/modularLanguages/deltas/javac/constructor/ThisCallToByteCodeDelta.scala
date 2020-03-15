package miksilo.modularLanguages.deltas.javac.constructor

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InvokeSpecialDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta

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
