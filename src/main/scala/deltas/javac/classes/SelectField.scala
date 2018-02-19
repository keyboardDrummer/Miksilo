package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.Node
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.GetStaticDelta
import deltas.bytecode.coreInstructions.objects.GetFieldDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta._

object SelectField extends ExpressionInstance {

  override def description: String = "Enables using the . operator to select a member from a class."

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, GetStaticDelta, MemberSelectorDelta)

  override def getType(path: NodePath, compilation: Compilation): Node = {
    val selector: MemberSelector[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val member = selector.member
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldInfo = classOrObjectReference.info.getField(member)
    fieldInfo._type
  }

  override def toByteCode(selector: NodePath, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticDelta.getStatic(fieldRefIndex))
    else
    {
      val obj = selector.target
      val objInstructions = ExpressionSkeleton.getToInstructions(compilation)(obj)
      objInstructions ++ Seq(GetFieldDelta.construct(fieldRefIndex))
    }
  }

  def getFieldRef(selector: Node, compiler: ClassCompiler, classOrObjectReference: ClassOrObjectReference): Node = {
    val member = selector.member
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRef(fieldInfo)
    fieldRef
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    core.addOption(grammars.find(SelectGrammar))
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, selector: NodePath, _type: Type, parentScope: Scope): Unit = {
    val target = selector.target
    val scopeDeclaration = MemberSelectorDelta.getScopeDeclaration(compilation, builder, target, parentScope)
    val scope = builder.getDeclaredScope(scopeDeclaration)
    val member = selector.member
    builder.resolve(member, selector.getLocation(Member), scope, Some(_type))
  }
}
