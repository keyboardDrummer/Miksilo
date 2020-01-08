package deltas.javac.classes

import core.deltas.Contract
import core.deltas.path.{FieldPath, NodePath}
import core.language.Compilation
import core.language.node.Node
import core.smarts.objects.NamedDeclaration
import core.smarts.types.objects.TypeFromDeclaration
import deltas.bytecode.constants.FieldRefConstant
import deltas.bytecode.coreInstructions.GetStaticDelta
import deltas.bytecode.coreInstructions.objects.GetFieldDelta
import deltas.bytecode.types.QualifiedObjectTypeDelta
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta.MemberSelector

object SelectFieldToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val selector: MemberSelector[NodePath] = path
    val fieldRefIndex: Node = getFieldRefIndex(compilation, selector)

    val targetType = ExpressionDelta.getCachedType(compilation, selector.target).asInstanceOf[TypeFromDeclaration]
    val targetDeclaration = compilation.proofs.resolveDeclaration(targetType.declaration).asInstanceOf[NamedDeclaration].origin.get.asInstanceOf[FieldPath].parent
    val wasClass = targetDeclaration.shape == JavaClassDelta.Shape

    if (wasClass)
      Seq(GetStaticDelta.getStatic(fieldRefIndex))
    else
    {
      val obj = selector.target
      val objInstructions = ToByteCodeSkeleton.getToInstructions(compilation)(obj)
      objInstructions ++ Seq(GetFieldDelta.construct(fieldRefIndex))
    }
  }

  def getFieldRefIndex(compilation: Compilation, selector: MemberSelector[NodePath]): Node = {
    val targetClass = QualifiedObjectTypeDelta._clazz(ExpressionDelta.cachedNodeType(compilation, selector.target))
    val qualifiedClassName = JavaClassDelta.getQualifiedClassName(targetClass)
    val nodeType = ExpressionDelta.cachedNodeType(compilation, selector)

    FieldRefConstant.fromPrimitives(qualifiedClassName, selector.member, nodeType)
  }

  override def shape = MemberSelectorDelta.Shape

  override def description = "Converts field selectors to bytecode."

  override def dependencies: Set[Contract] = Set(SelectFieldDelta, GetStaticDelta)
}
