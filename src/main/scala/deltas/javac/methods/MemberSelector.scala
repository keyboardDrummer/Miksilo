package deltas.javac.methods

import core.deltas.grammars.LanguageGrammars
import core.deltas._
import core.deltas.node._
import core.deltas.path.Path
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ExpressionSkeleton

object MemberSelector extends DeltaWithGrammar with WithLanguageRegistry {

  def getSelectorObject[T <: NodeLike](selector: T) = selector(Target).asInstanceOf[T]

  def getSelectorMember(selector: Node) = selector(Member).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression.as(Target) ~< ".") ~ identifier.as(Member) asNode Clazz
    create(SelectGrammar, selection)
  }

  object SelectGrammar extends GrammarKey

  object Clazz extends NodeClass

  object Target  extends NodeField

  object Member extends NodeField

  def selector(_object: Any, member: Any): Node = selector(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def selector(_object: Node, member: String): Node = {
    new Node(Clazz,
      Target -> _object,
      Member -> member)
  }

  def getClassOrObjectReference(selector: Path, compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = getSelectorObject(selector)
    getReferenceKind(compiler, obj).asInstanceOf[ClassOrObjectReference]
  }

  def getReferenceKind(classCompiler: ClassCompiler, expression: Path): ReferenceKind = {
    val getReferenceKindOption = MemberSelector.getReferenceKindRegistry(classCompiler.compilation).get(expression.clazz)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(classCompiler, expression)
    })(implementation => implementation(classCompiler.compilation, expression))
  }

  def getReferenceKindFromExpressionType(classCompiler: ClassCompiler, expression: Path): ClassOrObjectReference = {
    val classInfo: ClassSignature = classCompiler.findClass(ExpressionSkeleton.getType(classCompiler.compilation)(expression))
    ClassOrObjectReference(classInfo, wasClass = false)
  }

  def getReferenceKindRegistry(state: Language) = getRegistry(state).referenceKindRegistry
  class Registry {
    val referenceKindRegistry = new ClassRegistry[(Compilation, Path) => ReferenceKind]()
  }

  override def createRegistry = new Registry()

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
