package transformations.javac.classes

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.GetStaticDelta
import transformations.bytecode.coreInstructions.objects.GetFieldDelta
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MemberSelector
import transformations.javac.methods.MemberSelector._

object SelectField extends ExpressionInstance {

  override val key = SelectorKey

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

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    core.addOption(grammars.find(SelectGrammar))
  }

  override def description: String = "Enables using the . operator to select a member from a class."

}
