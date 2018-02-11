package deltas.javac.constructor

import core.deltas._
import core.language.exceptions.BadInputException
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.VoidTypeDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.methods.AccessibilityFieldsDelta.PublicVisibility
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import deltas.javac.methods.MethodDelta._
import deltas.javac.methods.call.CallStaticOrInstanceDelta
import deltas.javac.statements.BlockDelta

object ConstructorDelta extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(MethodDelta, CallStaticOrInstanceDelta, InvokeSpecialDelta, LoadAddressDelta, SuperCallExpression)

  case class BadConstructorNameException(javaClass: Node, constructor: Node) extends BadInputException

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: JavaClass[Node] = program
    val className = clazz.name
    for (constructor <- getConstructors(program)) {
      val constructorClassName = constructor(ConstructorClassNameKey).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw BadConstructorNameException(program, constructor.node)

      constructor.shape = MethodDelta.Shape
      constructor(MethodDelta.Name) = SuperCallExpression.constructorName
      constructor(MethodDelta.ReturnType) = VoidTypeDelta.voidType
      constructor(MethodDelta.TypeParameters) = Seq.empty
      constructor(AccessibilityFieldsDelta.Static) = false
      constructor.data.remove(ConstructorClassNameKey)
    }
  }

  def getConstructors[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] = {
    NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == ConstructorKey))
  }

  def constructor(className: String, _parameters: Seq[Node], _body: Seq[Node],
                  visibility: AccessibilityFieldsDelta.Visibility = PublicVisibility) = new Node(ConstructorKey,
    Parameters -> _parameters, Body -> _body, AccessibilityFieldsDelta.VisibilityField -> visibility,
    ConstructorClassNameKey -> className)


  object ConstructorKey extends NodeShape

  object ConstructorClassNameKey extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    val visibilityModifier = find(AccessibilityFieldsDelta.VisibilityField)
    val parseParameters = find(MethodDelta.ParametersGrammar) as Parameters
    val block = find(BlockDelta.Grammar) as Body
    val constructorGrammar = visibilityModifier ~~ identifier.as(ConstructorClassNameKey) ~ parseParameters % block asNode ConstructorKey
    memberGrammar.addOption(constructorGrammar)
  }

  override def description: String = "Introduces constructors."
}
