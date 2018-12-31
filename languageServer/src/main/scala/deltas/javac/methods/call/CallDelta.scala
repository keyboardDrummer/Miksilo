package deltas.javac.methods.call

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, Type}
import core.smarts.{ConstraintBuilder, ResolvesToType}
import deltas.expressions.ExpressionDelta
import deltas.javac.methods.MemberSelectorDelta

object CallDelta extends DeltaWithGrammar {

  override def description: String = "Introduces function calls of the form <callee>(<argument list>)"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val calleeGrammar = create(CallDelta.Callee, find(MemberSelectorDelta.Shape))
    val callArguments = create(CallDelta.CallArgumentsGrammar, expression.manySeparated(",").inParenthesis)
    val parseCall = calleeGrammar.as(CallDelta.Callee) ~ callArguments.as(CallDelta.Arguments) asNode CallDelta.Shape
    core.addAlternative(parseCall)
  }

  object Shape extends NodeShape

  object Callee extends NodeField

  object Arguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

  implicit class Call[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def callee: T = node(Callee).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(Arguments).asInstanceOf[Seq[T]])
  }

  def call(callee: Any, arguments: Any): Node =
    neww(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def neww(callee: Node, arguments: Seq[Node] = Seq()): Node = {
    new Node(CallDelta.Shape, CallDelta.Callee -> callee, CallDelta.Arguments -> arguments)
  }

  def callConstraints(compilation: Compilation, builder: ConstraintBuilder, callArguments: Seq[NodePath], parentScope: Scope,
                      methodReference: Reference, returnType: Type): Unit = {
    val callTypes = callArguments.map(argument => ExpressionDelta.getType(compilation, builder, argument, parentScope))
    val functionType = FunctionType.curry(callTypes, returnType)
    builder.add(new ResolvesToType(methodReference, builder.declarationVariable(), functionType))
  }

  override def dependencies = Set(MemberSelectorDelta, ExpressionDelta)
}
