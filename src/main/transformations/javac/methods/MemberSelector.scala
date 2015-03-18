package transformations.javac.methods

import core.particles.grammars.GrammarCatalogue
import core.particles._
import transformations.javac.classes._
import transformations.javac.expressions.ExpressionSkeleton

object MemberSelector extends ParticleWithGrammar with WithState {

  def getSelectorObject[T <: MetaLike](selector: T) = selector(SelectorObject).asInstanceOf[T]

  def getSelectorMember(selector: MetaObject) = selector(SelectorMember).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression <~ ".") ~ identifier ^^ parseMap(SelectorKey, SelectorObject, SelectorMember)
    grammars.create(SelectGrammar, selection)
  }

  object SelectGrammar

  object SelectorKey

  object SelectorObject

  object SelectorMember

  def selector(_object: Any, member: Any): MetaObject = selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

  def selector(_object: MetaObject, member: String): MetaObject = {
    new MetaObject(SelectorKey,
      SelectorObject -> _object,
      SelectorMember -> member)
  }


  def getClassOrObjectReference(selector: Path, compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = getSelectorObject(selector)
    getReferenceKind(compiler, obj).asInstanceOf[ClassOrObjectReference]
  }

  def getReferenceKind(classCompiler: ClassCompiler, expression: Path): ReferenceKind = {
    val getReferenceKindOption = MemberSelector.getReferenceKindRegistry(classCompiler.state).get(expression.clazz)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(classCompiler, expression)
    })(implementation => implementation(expression))
  }

  def getReferenceKindFromExpressionType(classCompiler: ClassCompiler, expression: Path): ClassOrObjectReference = {
    val classInfo: ClassInfo = classCompiler.findClass(ExpressionSkeleton.getType(classCompiler.state)(expression))
    new ClassOrObjectReference(classInfo, false)
  }

  def getReferenceKindRegistry(state: CompilationState) = getState(state).referenceKindRegistry
  class State {
    val referenceKindRegistry = new ClassRegistry[Path => ReferenceKind]()
  }

  override def createState = new State()

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
