package deltas.javac.classes

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.GetStaticDelta
import deltas.bytecode.coreInstructions.objects.GetFieldDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta.{MemberSelector, getClassOrObjectReference}

object SelectFieldToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val selector: MemberSelector[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticDelta.getStatic(fieldRefIndex))
    else
    {
      val obj = selector.target
      val objInstructions = ToByteCodeSkeleton.getToInstructions(compilation)(obj)
      objInstructions ++ Seq(GetFieldDelta.construct(fieldRefIndex))
    }
  }

  def getFieldRef(selector: MemberSelector[NodePath], compiler: ClassCompiler, classOrObjectReference: ClassOrObjectReference): Node = {
    val member = selector.member
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRef(fieldInfo)
    fieldRef
  }

  override def shape = MemberSelectorDelta.Shape

  override def description = "Converts field selectors to bytecode."

  override def dependencies: Set[Contract] = Set(SelectFieldDelta, GetStaticDelta)
}
