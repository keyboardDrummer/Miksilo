package deltas.javac.classes

import core.deltas.Contract
import core.deltas.path.{FieldPath, NodePath}
import core.language.Compilation
import core.language.node.Node
import core.smarts.SolveConstraintsDelta
import core.smarts.objects.NamedDeclaration
import core.smarts.types.objects.TypeFromDeclaration
import deltas.bytecode.constants.FieldRefConstant
import deltas.bytecode.coreInstructions.GetStaticDelta
import deltas.bytecode.coreInstructions.objects.GetFieldDelta
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta.MemberSelector

object SelectFieldToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val selector: MemberSelector[NodePath] = path
    val fieldRefIndex: Node = getFieldRefIndex(compilation, selector)

    val targetType = ExpressionDelta.getCachedType(compilation, selector.target).asInstanceOf[TypeFromDeclaration]
    val targetDeclaration = compilation.proofs.resolveDeclaration(targetType.declaration).asInstanceOf[NamedDeclaration].origin.get.asInstanceOf[FieldPath].parent
    val wasClass = targetDeclaration.shape == JavaClassSkeleton.Shape

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
    val scopeGraph = compilation.proofs.scopeGraph

    val targetScopeDeclaration = ExpressionDelta.getCachedType(compilation, selector.target).asInstanceOf[TypeFromDeclaration].declaration
    val targetClass = compilation.proofs.resolveDeclaration(targetScopeDeclaration).asInstanceOf[NamedDeclaration].origin.get.asInstanceOf[FieldPath].parent
    val qualifiedClassName = JavaClassSkeleton.getQualifiedClassName(targetClass)

    val fieldDeclaration = SolveConstraintsDelta.referenceDeclaration(selector.getSourceElement(MemberSelectorDelta.Member))
    val fieldType = compilation.proofs.environment(fieldDeclaration)
    val nodeType = TypeSkeleton.fromConstraintType(fieldType)

    FieldRefConstant.fromPrimitives(qualifiedClassName, selector.member, nodeType)
  }

  override def shape = MemberSelectorDelta.Shape

  override def description = "Converts field selectors to bytecode."

  override def dependencies: Set[Contract] = Set(SelectFieldDelta, GetStaticDelta)
}
