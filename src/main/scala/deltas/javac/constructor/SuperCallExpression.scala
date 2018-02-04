package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.{NodePath, Path}
import core.deltas.{Compilation, Contract}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.VoidTypeDelta
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import deltas.javac.statements.StatementSkeleton

object SuperCallExpression extends ExpressionInstance {

  override def description: String = "Enables calling a super constructor."

  override val key = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(SuperCall, CallDelta.CallArguments -> arguments)

  override def getType(expression: NodePath, compilation: Compilation): Node = VoidTypeDelta.voidType

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformSuperCall(classCompiler.currentClass, call, compilation)
  }

  def transformSuperCall(program: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    transformToByteCode(call, compilation, program.parent.get)
  }

  def transformToByteCode(call: NodePath, compilation: Compilation, className: String): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val callArguments = CallDelta.getCallArguments(call)
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation)(argument))
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(MethodQuery(qualifiedName, constructorName, callTypes))
    val argumentInstructions = callArguments.flatMap(argument => StatementSkeleton.getToInstructions(compilation)(argument))
    Seq(LoadAddressDelta.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallDelta.CallArguments) asNode SuperCall
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(superCallGrammar)
  }

  object SuperCall extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val clazz: JavaClass[NodePath] = call.findAncestorShape(JavaClassSkeleton.Shape)
    val parentName = clazz.parent.get //TODO shouldn't really be an option at this point.
    val superClass = builder.resolve(parentName, clazz.node(ClassParent).asInstanceOf[Path], parentScope)
    val superScope = builder.resolveScopeDeclaration(superClass)
    CallDelta.callConstraints(compilation, builder, call, superScope, constructorName, VoidTypeDelta.constraintType)
  }
}
