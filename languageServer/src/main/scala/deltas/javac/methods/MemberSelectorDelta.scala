package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.expressions.ExpressionDelta
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ByteCodeExpressionSkeleton

object MemberSelectorDelta extends DeltaWithGrammar {

  implicit class MemberSelector[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def member: String = node(Member).asInstanceOf[String]
    def target: T = node(Target).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val selection = (expression.as(Target) ~< ".") ~ identifier.as(Member) asNode Shape
    create(SelectGrammar, selection)
  }

  object SelectGrammar extends GrammarKey

  object Shape extends NodeShape

  object Target  extends NodeField

  object Member extends NodeField

  def selector(_object: Any, member: Any): Node = selector(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def selector(_object: Node, member: String): Node = {
    new Node(Shape,
      Target -> _object,
      Member -> member)
  }

  def getClassOrObjectReference(selector: MemberSelector[NodePath], compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = selector.target
    getReferenceKind(compiler, obj).asInstanceOf[ClassOrObjectReference]
  }

  def getReferenceKind(classCompiler: ClassCompiler, expression: NodePath): ReferenceKind = {
    val getReferenceKindOption = MemberSelectorDelta.referenceKindRegistry.get(classCompiler.compilation).get(expression.shape)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(classCompiler, expression)
    })(implementation => implementation(classCompiler.compilation, expression))
  }

  def getReferenceKindFromExpressionType(classCompiler: ClassCompiler, expression: NodePath): ClassOrObjectReference = {
    val classInfo: ClassSignature = classCompiler.findClass(ByteCodeExpressionSkeleton.getType(classCompiler.compilation)(expression))
    ClassOrObjectReference(classInfo, wasClass = false)
  }

  val referenceKindRegistry = new ShapeProperty[(Compilation, NodePath) => ReferenceKind]

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
