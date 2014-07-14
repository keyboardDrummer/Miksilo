package transformations.javac.methods

import core.grammar.seqr
import core.transformation._
import transformations.bytecode.ByteCode
import transformations.javac.base.{ClassOrObjectReference, JavaMethodC, MethodCompiler}
import transformations.javac.expressions.ExpressionC

import scala.collection.mutable

object SelectorC extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaMethodC)

  object SelectorKey

  object SelectorObject

  object SelectorMember

  def selector(_object: MetaObject, member: String): MetaObject = {
    new MetaObject(SelectorKey) {
      data.put(SelectorObject, _object)
      data.put(SelectorMember, member)
    }
  }

  def getSelectorObject(selector: MetaObject) = selector(SelectorObject).asInstanceOf[MetaObject]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(SelectorKey, (selector: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      selectorToLines(selector, methodCompiler)
    })
  }

  def selectorToLines(selector: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val obj = getSelectorObject(selector)
    val member = getSelectorMember(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.classCompiler.getFieldRefIndex(fieldInfo)
    if (classOrObjectReference.wasClass)
      Seq(ByteCode.getStatic(fieldRef))
    else
      ???
  }

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq(".")

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val selection = (expression <~ ".") ~ identifier ^^ { case left seqr right => selector(left, right)}
    expression.inner = expression.inner | selection
  }

  def selector(_object: Any, member: Any): MetaObject = selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

}
