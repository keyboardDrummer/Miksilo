package deltas.javac.classes

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.NodeShape
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods.MemberSelectorDelta.{Member, MemberSelector, Shape}
import deltas.javac.methods.{HasDeclaredScope, HasScopeSkeleton, ResolveNamespaceOrObjectVariableAmbiguity}

object MemberSelectorAsNamespaceReference extends Delta with HasDeclaredScope {
  override def dependencies: Set[Contract] = Set(SelectFieldDelta, JavaClassSkeleton)

  override def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    val memberSelector: MemberSelector[NodePath] = expression
    val target = memberSelector.target
    val targetScope = HasScopeSkeleton.getScope(compilation, builder, target, scope)
    val namespaceOrObjectVariableDeclaration =
      builder.resolve(memberSelector.member, expression.getSourceElement(Member), targetScope)
    val result = builder.declarationVariable()
    builder.add(ResolveNamespaceOrObjectVariableAmbiguity(namespaceOrObjectVariableDeclaration, result))
    result
  }

  override def shape: NodeShape = Shape

  override def description: String = "Enables recognizing the kind of a selection, whether is a class, package or object."
}
