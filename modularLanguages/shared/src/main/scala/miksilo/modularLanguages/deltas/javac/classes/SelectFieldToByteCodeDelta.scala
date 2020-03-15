package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.{FieldPath, NodePath}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.smarts.objects.NamedDeclaration
import miksilo.languageServer.core.smarts.types.objects.TypeFromDeclaration
import miksilo.modularLanguages.deltas.bytecode.constants.FieldRefConstant
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.GetStaticDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.GetFieldDelta
import miksilo.modularLanguages.deltas.bytecode.types.QualifiedObjectTypeDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta.MemberSelector

object SelectFieldToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val selector: MemberSelector[NodePath] = path
    val fieldRefIndex: Node = getFieldRefIndex(compilation, selector)

    val targetType = ExpressionDelta.getCachedType(compilation, selector.target).asInstanceOf[TypeFromDeclaration]
    val targetDeclaration = compilation.proofs.resolveDeclaration(targetType.declaration).asInstanceOf[NamedDeclaration].origin.get.asInstanceOf[FieldPath].parent
    val wasClass = targetDeclaration.shape == ClassDelta.Shape

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
