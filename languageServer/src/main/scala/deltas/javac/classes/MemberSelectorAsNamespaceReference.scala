package deltas.javac.classes

import core.deltas._
import core.deltas.path.NodePath
import core.language.node.NodeShape
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton, PackageSignature}
import deltas.javac.methods.{IsNamespaceOrObjectExpression, MemberSelectorDelta, NamespaceOrObjectExpression, ResolveNamespaceOrObjectVariableAmbiguity}
import deltas.javac.methods.MemberSelectorDelta.{Member, MemberSelector, Shape}

object MemberSelectorAsNamespaceReference extends Delta with IsNamespaceOrObjectExpression {
  override def dependencies: Set[Contract] = Set(SelectField, JavaClassSkeleton)

  override def inject(language: Language): Unit = {
    MemberSelectorDelta.referenceKindRegistry.add(language, Shape, (compilation, selector) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(path: NodePath, compiler: ClassCompiler): ReferenceKind = {
    val selector: MemberSelector[NodePath] = path
    val obj = selector.target
    val member = selector.member
    MemberSelectorDelta.getReferenceKind(compiler, obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageSignature => PackageReference(result)
        case result: ClassSignature => ClassOrObjectReference(result, wasClass = true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = compiler.findClass(field._type)
        ClassOrObjectReference(fieldClassType, wasClass = false)
    }
  }

  override def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    val memberSelector: MemberSelector[NodePath] = expression
    val target = memberSelector.target
    val targetScope = NamespaceOrObjectExpression.getScope(compilation, builder, target, scope)
    val namespaceOrObjectVariableDeclaration =
      builder.resolve(memberSelector.member, expression.getMember(Member), targetScope)
    val result = builder.declarationVariable()
    builder.add(ResolveNamespaceOrObjectVariableAmbiguity(namespaceOrObjectVariableDeclaration, result))
    result
  }

  override def shape: NodeShape = Shape

  override def description: String = "Enables recognizing the kind of a selection, whether is a class, package or object."
}
