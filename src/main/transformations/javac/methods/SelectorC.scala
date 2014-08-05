package transformations.javac.methods

import core.grammar.seqr
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.instructions.GetStaticC
import transformations.javac.base._
import transformations.javac.expressions.ExpressionC

object SelectorC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(JavaMethodC, GetStaticC)

  override def inject(state: TransformationState): Unit = {
    JavaMethodC.getReferenceKindRegistry(state).put(SelectorKey, selector => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      getReferenceKind(selector, methodCompiler)
    })
    ExpressionC.getExpressionToLines(state).put(SelectorKey, (selector: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      selectorToLines(selector, methodCompiler)
    })
  }

  def getReferenceKind(selector: MetaObject, methodCompiler: MethodCompiler): ReferenceKind = {
    val obj = SelectorC.getSelectorObject(selector)
    val member = SelectorC.getSelectorMember(selector)
    methodCompiler.getReferenceKind(obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageInfo => new PackageReference(result)
        case result: ClassInfo => new ClassOrObjectReference(result, true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = methodCompiler.classCompiler.findClass(field._type.asInstanceOf[MetaObject])
        new ClassOrObjectReference(fieldClassType, false)
    }
  }

  def selectorToLines(selector: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val obj = getSelectorObject(selector)
    val member = getSelectorMember(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.classCompiler.getFieldRefIndex(fieldInfo)
    if (classOrObjectReference.wasClass)
      Seq(GetStaticC.getStatic(fieldRef))
    else
      ???
  }

  def getSelectorObject(selector: MetaObject) = selector(SelectorObject).asInstanceOf[MetaObject]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

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

  object SelectorKey

  object SelectorObject

  object SelectorMember

}
