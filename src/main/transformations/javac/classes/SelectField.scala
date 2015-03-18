package transformations.javac.classes

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.GetStaticC
import transformations.bytecode.coreInstructions.objects.GetFieldC
import transformations.javac.methods.MemberSelector
import MemberSelector._
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object SelectField extends ExpressionInstance {

  override val key: AnyRef = SelectorKey

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, GetStaticC, MemberSelector)

  override def getType(selector: Origin, state: CompilationState): MetaObject = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val member = getSelectorMember(selector)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldInfo = classOrObjectReference.info.getField(member)
    fieldInfo._type
  }

  override def toByteCode(selector: Origin, state: CompilationState): Seq[MetaObject] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldRefIndex = getFieldRefIndex(selector, compiler, classOrObjectReference)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticC.getStatic(fieldRefIndex))
    else
    {
      val obj = getSelectorObject(selector)
      val objInstructions = ExpressionSkeleton.getToInstructions(state)(obj)
      objInstructions ++ Seq(GetFieldC.construct(fieldRefIndex))
    }
  }

  def getFieldRefIndex(selector: MetaObject, compiler: ClassCompiler, classOrObjectReference: ClassOrObjectReference): Int = {
    val member = getSelectorMember(selector)
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRefIndex(fieldInfo)
    fieldRef
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    core.addOption(grammars.find(SelectGrammar))
  }

  override def description: String = "Enables using the . operator to select a member from a class."

}
