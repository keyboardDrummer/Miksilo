package transformations.javac.classes

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.GetStaticC
import transformations.bytecode.coreInstructions.objects.GetFieldC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}

object SelectorC extends ExpressionInstance {

  override val key: AnyRef = SelectorKey

  override def dependencies: Set[Contract] = Set(ClassC, GetStaticC)

  def selector(_object: Any, member: Any): MetaObject = selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

  def selector(_object: MetaObject, member: String): MetaObject = {
    new MetaObject(SelectorKey) {
      data.put(SelectorObject, _object)
      data.put(SelectorMember, member)
    }
  }

  override def getType(selector: MetaObject, state: TransformationState): MetaObject = {
    val compiler = ClassC.getClassCompiler(state)
    val obj = getSelectorObject(selector)
    val member = getSelectorMember(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val fieldInfo = classOrObjectReference.info.getField(member)
    fieldInfo._type
  }

  def getSelectorObject(selector: MetaObject) = selector(SelectorObject).asInstanceOf[MetaObject]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

  override def toByteCode(selector: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val compiler = ClassC.getClassCompiler(state)
    val classOrObjectReference = getClassOrObjectReference(selector, compiler)
    val fieldRefIndex = getFieldRefIndex(selector, compiler, classOrObjectReference)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticC.getStatic(fieldRefIndex))
    else
    {
      val obj = getSelectorObject(selector)
      val objInstructions = ExpressionC.getToInstructions(state)(obj)
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
    val core = grammars.find(ExpressionC.CoreGrammar)
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val selection = (expression <~ ".") ~ identifier ^^ parseMap(SelectorKey, SelectorObject, SelectorMember)
    core.addOption(grammars.create(SelectGrammar, selection))
  }

  object SelectGrammar

  object SelectorKey

  object SelectorObject

  object SelectorMember

}
