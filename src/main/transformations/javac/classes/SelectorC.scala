package transformations.javac.classes

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.GetStaticC
import transformations.bytecode.coreInstructions.objects.GetFieldC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object SelectorC extends ExpressionInstance {

  override val key: AnyRef = SelectorKey

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, GetStaticC)

  def selector(_object: Any, member: Any): MetaObject = selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

  def selector(_object: MetaObject, member: String): MetaObject = {
    new MetaObject(SelectorKey) {
      data.put(SelectorObject, _object)
      data.put(SelectorMember, member)
    }
  }

  override def getType(selector: MetaObject, state: CompilationState): MetaObject = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val obj = getSelectorObject(selector)
    val member = getSelectorMember(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val fieldInfo = classOrObjectReference.info.getField(member)
    fieldInfo._type
  }

  def getSelectorObject(selector: MetaObject) = selector(SelectorObject).asInstanceOf[MetaObject]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

  override def toByteCode(selector: MetaObject, state: CompilationState): Seq[MetaObject] = {
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

  def getClassOrObjectReference(selector: MetaObject, compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = getSelectorObject(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    classOrObjectReference
  }

  def getFieldRefIndex(selector: MetaObject, compiler: ClassCompiler, classOrObjectReference: ClassOrObjectReference): Int = {
    val member = getSelectorMember(selector)
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRefIndex(fieldInfo)
    fieldRef
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression <~ ".") ~ identifier ^^ parseMap(SelectorKey, SelectorObject, SelectorMember)
    core.addOption(grammars.create(SelectGrammar, selection))
  }

  object SelectGrammar

  object SelectorKey

  object SelectorObject

  object SelectorMember

  override def description: String = "Enables using the . operator to select a member from a class."
}
