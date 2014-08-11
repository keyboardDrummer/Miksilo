package transformations.javac.methods

import core.grammar.seqr
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.GetStaticC
import transformations.javac.base._
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}

object SelectorC extends ExpressionInstance {

  override val key: AnyRef = SelectorKey

  override def dependencies: Set[Contract] = Set(ClassC, GetStaticC)

  override def inject(state: TransformationState): Unit = {
    ClassC.getReferenceKindRegistry(state).put(SelectorKey, selector => {
      val compiler = ClassC.getClassCompiler(state)
      getReferenceKind(selector, compiler)
    })
    super.inject(state)
  }

  def getReferenceKind(selector: MetaObject, compiler: ClassCompiler): ReferenceKind = {
    val obj = SelectorC.getSelectorObject(selector)
    val member = SelectorC.getSelectorMember(selector)
    compiler.getReferenceKind(obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageInfo => new PackageReference(result)
        case result: ClassInfo => new ClassOrObjectReference(result, true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = compiler.findClass(field._type.asInstanceOf[MetaObject])
        new ClassOrObjectReference(fieldClassType, false)
    }
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val selection = (expression <~ ".") ~ identifier ^^ { case left seqr right => selector(left, right)}
    expression.inner = expression.inner | selection
  }

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

  override def toByteCode(selector: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val compiler = ClassC.getClassCompiler(state)
    val obj = getSelectorObject(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val member = getSelectorMember(selector)
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.getFieldRefIndex(fieldInfo)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticC.getStatic(fieldRef))
    else
      ???
  }

  def getSelectorObject(selector: MetaObject) = selector(SelectorObject).asInstanceOf[MetaObject]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

  object SelectorKey

  object SelectorObject

  object SelectorMember

}
