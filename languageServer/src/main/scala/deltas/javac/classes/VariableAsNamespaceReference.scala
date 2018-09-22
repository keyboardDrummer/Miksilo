package deltas.javac.classes

import core.deltas._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.NodeShape
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.expressions.VariableDelta
import deltas.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature}
import deltas.javac.methods.{IsNamespaceOrObjectExpression, MemberSelectorDelta, ResolveNamespaceOrObjectVariableAmbiguity, VariableToByteCodeDelta}
import deltas.expressions.VariableDelta.{Shape, Variable}

object VariableAsNamespaceReference extends Delta with IsNamespaceOrObjectExpression {

  override def description: String = "Enables recognizing the kind of an identifier, whether is a class, package or object."

  override def inject(language: Language): Unit = {
    super.inject(language)
    MemberSelectorDelta.referenceKindRegistry.add(language, Shape, (compilation, _variable) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      val variable: Variable[NodePath] = _variable
      getReferenceKind(variable, compiler)
    })
  }

  def getReferenceKind(variable: Variable[NodePath], classCompiler: ClassCompiler): ReferenceKind = {

    val isClass = classCompiler.classNames.contains(variable.name)
    if (isClass)
      ClassOrObjectReference(classCompiler.findClass(variable.name), wasClass = true)
    else {
      val mbPackage = classCompiler.javaCompiler.classPath.content.get(variable.name)
      if (mbPackage.isDefined)
        PackageReference(mbPackage.get.asInstanceOf[PackageSignature])
      else {
        MemberSelectorDelta.getReferenceKindFromExpressionType(classCompiler, variable)
      }
    }
  }

  override def dependencies: Set[Contract] = Set(VariableToByteCodeDelta, JavaClassSkeleton)

  override def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, variable: NodePath, scope: Scope): Declaration = {
    val namespaceOrObjectVariableDeclaration =
      builder.resolve(variable.name, variable.getLocation(VariableDelta.Name), scope)
    val result = builder.declarationVariable()
    builder.add(ResolveNamespaceOrObjectVariableAmbiguity(namespaceOrObjectVariableDeclaration, result))
    result
  }

  override def shape: NodeShape = Shape
}
