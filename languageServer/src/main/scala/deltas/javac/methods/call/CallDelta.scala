package deltas.javac.methods.call

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{FieldPath, NodePath}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, Type}
import core.smarts.{ConstraintBuilder, ResolvesToType}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.constants.{ClassInfoConstant, MethodRefConstant, NameAndTypeConstant, Utf8ConstantDelta}
import deltas.bytecode.extraConstants.TypeConstant
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.{MemberSelectorDelta, MethodDelta}

object CallDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Introduces function calls of the form <callee>(<argument list>)"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val calleeGrammar = create(CallDelta.Callee, find(MemberSelectorDelta.Shape)) // TODO don't hardcode MemberSelectorDelta here.
    val callArguments = create(CallDelta.CallArgumentsGrammar, expression.manySeparated(",").inParenthesis)
    val parseCall = calleeGrammar.as(CallDelta.Callee) ~ callArguments.as(CallDelta.Arguments) asLabelledNode CallDelta.Shape
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
                      methodReference: Reference, returnType: Type): Type = {
    val callTypes = callArguments.map(argument => ExpressionDelta.getType(compilation, builder, argument, parentScope))
    val functionType = FunctionType.curry(callTypes, returnType)
    builder.add(new ResolvesToType(methodReference, builder.declarationVariable(), functionType))
    functionType
  }

  override def dependencies = Set(MemberSelectorDelta, ExpressionDelta)

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, returnType: Type, parentScope: Scope): Unit = {
    val call: Call[NodePath] = path
    val calleeReference = ReferenceExpressionSkeleton.getReference(compilation, builder, call.callee, parentScope)
    val functionType = CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, calleeReference, returnType)
    ExpressionDelta.nodeType(call.callee) = functionType
  }

  override def shape = Shape

  def getMethodRefIndex(className: QualifiedClassName, methodName: String, methodType: Node) = {
    val classRef = ClassInfoConstant.classRef(className)
    val nameAndTypeIndex = getMethodNameAndTypeIndex(methodName, methodType)
    MethodRefConstant.methodRef(classRef, nameAndTypeIndex)
  }

  def getMethodNameAndTypeIndex(methodName: String, methodType: Node) = {
    val methodNameIndex = getNameIndex(methodName)
    NameAndTypeConstant.nameAndType(methodNameIndex, TypeConstant.constructor(methodType))
  }

  def getNameIndex(methodName: String) = {
    Utf8ConstantDelta.create(methodName)
  }

  def getMethodRefIndexFromCallee(compilation: Compilation, callee: NodePath) = {
    val method: Method[NodePath] = getMethodFromCallee(compilation, callee)
    getMethodRefIndexFromMethod(method)
  }

  def getMethodRefIndexFromMethod(method: Method[NodePath]): Node = {
    val methodType = MethodDelta.getMethodType(method)
    val constructorClass: JavaClass[NodePath] = method.ancestors.find(a => a.shape == JavaClassSkeleton.Shape || a.shape == ByteCodeSkeleton.Shape).get
    getMethodRefIndex(JavaClassSkeleton.getQualifiedClassName(constructorClass), method.name, methodType)
  }

  def getMethodFromCallee(compilation: Compilation, callee: NodePath) = {
    val scopeGraph = compilation.proofs.scopeGraph
    val constructorReference = ReferenceExpressionSkeleton.references(callee)
    val constructorDeclaration = compilation.proofs.declarations(constructorReference)
    val method: Method[NodePath] = constructorDeclaration.origin.get.asInstanceOf[FieldPath].parent
    method
  }
}
