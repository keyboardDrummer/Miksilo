package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.objects.Reference
import core.smarts.scopes.ReferenceInScope
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import core.smarts.{ConstraintBuilder, SolveConstraintsDelta}
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MethodDelta.Method
import deltas.javac.methods.call.CallDelta.Call
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}

object SuperCallExpression extends DeltaWithGrammar with ExpressionInstance with ConvertsToByteCodeDelta {

  override def description: String = "Enables calling a super constructor."

  override val shape = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(SuperCall, CallDelta.Arguments -> arguments)

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)
    transformSuperCall(classCompiler.currentClass, call, compilation)
  }

  def transformSuperCall(program: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    transformToByteCode(call, compilation, program.parent.get)
  }

  def transformToByteCode(path: NodePath, compilation: Compilation, className: String): Seq[Node] = {
    val call: Call[NodePath] = path
    val callArguments = call.arguments

    val method: Method[NodePath] = SolveConstraintsDelta.getDeclarationOfReference(call.node)

    val methodRefIndex = CallDelta.getMethodRefIndexFromMethod(method)
    val argumentInstructions = callArguments.flatMap(argument => ToByteCodeSkeleton.getToInstructions(compilation)(argument))
    Seq(LoadAddressDelta.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallDelta.Arguments) asNode SuperCall
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(superCallGrammar)
  }

  object SuperCall extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val clazz: JavaClass[NodePath] = call.findAncestorShape(JavaClassDelta.Shape)
    val parentName = clazz.parent.get
    val superClass = builder.resolve(parentName, call.getSourceElement(ClassParent), parentScope)
    val superScope = builder.getDeclaredScope(superClass)

    val superReference = new Reference(constructorName, Some(call))
    builder.add(ReferenceInScope(superReference, superScope))
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, superReference, VoidTypeDelta.constraintType)
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType)
  }
}
