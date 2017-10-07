package transformations.javac.methods

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.node._
import core.particles.path.Path
import transformations.javac.classes._
import transformations.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import transformations.javac.expressions.ExpressionSkeleton

object MemberSelector extends DeltaWithGrammar with WithState {

  def getSelectorObject[T <: NodeLike](selector: T) = selector(SelectorObject).asInstanceOf[T]

  def getSelectorMember(selector: Node) = selector(SelectorMember).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression.as(SelectorObject) ~< ".") ~ identifier.as(SelectorMember) asNode(SelectorKey)
    grammars.create(SelectGrammar, selection)
  }

  object SelectGrammar

  object SelectorKey extends NodeClass

  object SelectorObject  extends NodeField

  object SelectorMember extends NodeField

  def selector(_object: Any, member: Any): Node = selector(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def selector(_object: Node, member: String): Node = {
    new Node(SelectorKey,
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
    val classInfo: ClassSignature = classCompiler.findClass(ExpressionSkeleton.getType(classCompiler.state)(expression))
    new ClassOrObjectReference(classInfo, false)
  }

  def getReferenceKindRegistry(state: Language) = getState(state).referenceKindRegistry
  class State {
    val referenceKindRegistry = new ClassRegistry[Path => ReferenceKind]()
  }

  override def createState = new State()

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
