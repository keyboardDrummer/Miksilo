package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.expression.ExpressionDelta
import deltas.javac.classes._
import deltas.javac.classes.skeleton.ClassSignature

object MemberSelectorDelta extends DeltaWithGrammar {

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  implicit class MemberSelector[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def member: String = node.getValue(Member).asInstanceOf[String]
    def target: T = node(Target).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    (expression.as(Target) ~< ".") ~ identifier.as(Member) asLabelledNode Shape
  }

  object Shape extends NodeShape

  object Target  extends NodeField

  object Member extends NodeField

  def neww(_object: Any, member: Any): Node = neww(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def neww(_object: Node, member: String): Node = {
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
    val classInfo: ClassSignature = classCompiler.findClass(ExpressionDelta.getType(classCompiler.compilation)(expression))
    ClassOrObjectReference(classInfo, wasClass = false)
  }

  val referenceKindRegistry = new ShapeProperty[(Compilation, NodePath) => ReferenceKind]

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
