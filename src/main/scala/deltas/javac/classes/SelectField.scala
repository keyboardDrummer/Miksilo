package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.deltas.path.Path
import deltas.bytecode.coreInstructions.GetStaticDelta
import deltas.bytecode.coreInstructions.objects.GetFieldDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MemberSelector
import deltas.javac.methods.MemberSelector._

object SelectField extends ExpressionInstance {

  override val key = Clazz

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, GetStaticDelta, MemberSelector)

  override def getType(selector: Path, compilation: Compilation): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val member = getSelectorMember(selector)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldInfo = classOrObjectReference.info.getField(member)
    fieldInfo._type
  }

  override def toByteCode(selector: Path, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticDelta.getStatic(fieldRefIndex))
    else
    {
      val obj = getSelectorObject(selector)
      val objInstructions = ExpressionSkeleton.getToInstructions(compilation)(obj)
      objInstructions ++ Seq(GetFieldDelta.construct(fieldRefIndex))
    }
  }

  def getFieldRef(selector: Node, compiler: ClassCompiler, classOrObjectReference: ClassOrObjectReference) = {
    val member = getSelectorMember(selector)
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRef(fieldInfo)
    fieldRef
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    core.addOption(grammars.find(SelectGrammar))
  }

  override def description: String = "Enables using the . operator to select a member from a class."

}
