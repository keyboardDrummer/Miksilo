package miksilo.modularLanguages.deltas.method.call

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{FieldPath, NodePath}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.objects.Reference
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{FunctionType, Type}
import miksilo.languageServer.core.smarts.{ConstraintBuilder, ResolvesToType}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.constants.{ClassInfoConstant, MethodRefConstant, NameAndTypeConstant, Utf8ConstantDelta}
import miksilo.modularLanguages.deltas.bytecode.extraConstants.TypeConstant
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{JavaClassDelta, QualifiedClassName}
import miksilo.modularLanguages.deltas.javac.methods.call.ReferenceExpressionSkeleton
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.MethodDelta.Method

object CallDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Introduces function calls of the form <callee>(<argument list>)"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val calleeGrammar = create(CallDelta.Callee)
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

  override def dependencies = Set(ExpressionDelta)

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, returnType: Type, parentScope: Scope): Unit = {
    val call: Call[NodePath] = path
    val calleeReference = ReferenceExpressionSkeleton.getReference(compilation, builder, call.callee, parentScope)
    val functionType = CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, calleeReference, returnType)
    ExpressionDelta.constraintType(call.callee) = functionType
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
    val constructorClass: JavaClass[NodePath] = method.ancestors.find(a => a.shape == ClassDelta.Shape || a.shape == ByteCodeSkeleton.Shape).get
    getMethodRefIndex(JavaClassDelta.getQualifiedClassName(constructorClass), method.name, methodType)
  }

  def getMethodFromCallee(compilation: Compilation, callee: NodePath) = {
    val callReference = ReferenceExpressionSkeleton.references(callee)
    val callDeclaration = compilation.proofs.references(callReference)
    val method: Method[NodePath] = callDeclaration.origin.get.asInstanceOf[FieldPath].parent
    method
  }
}
