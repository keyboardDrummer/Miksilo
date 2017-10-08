package transformations.javac.constructor

import core.particles._
import core.particles.exceptions.BadInputException
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.coreInstructions.InvokeSpecialDelta
import transformations.bytecode.coreInstructions.objects.LoadAddressDelta
import transformations.bytecode.types.VoidTypeC
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.methods.MethodDelta
import transformations.javac.methods.MethodDelta._
import transformations.javac.methods.call.CallStaticOrInstanceC
import transformations.javac.statements.BlockC

object ConstructorC extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(MethodDelta, CallStaticOrInstanceC, InvokeSpecialDelta, LoadAddressDelta, SuperCallExpression)

  case class BadConstructorNameException(clazz: Node, constructor: Node) extends BadInputException

  override def transform(clazz: Node, state: Compilation): Unit = {
    val className = clazz.name
    for (constructor <- getConstructors(clazz)) {
      val constructorClassName = constructor(ConstructorClassNameKey).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw new BadConstructorNameException(clazz, constructor)

      constructor.clazz = MethodDelta.MethodKey
      constructor(MethodDelta.MethodNameKey) = SuperCallExpression.constructorName
      constructor(MethodDelta.ReturnTypeKey) = VoidTypeC.voidType
      constructor(MethodDelta.TypeParameters) = Seq.empty
      constructor(StaticKey) = false
      constructor.data.remove(ConstructorClassNameKey)
    }
  }

  def getConstructors(clazz: Node): Seq[Node] = {
    clazz.members.filter(member => member.clazz == ConstructorKey)
  }

  def constructor(className: String, _parameters: Seq[Node], _body: Seq[Node],
                  visibility: Visibility = PublicVisibility) = new Node(ConstructorKey,
    MethodParametersKey -> _parameters, MethodBodyKey -> _body, VisibilityKey -> visibility,
    ConstructorClassNameKey -> className)


  object ConstructorKey extends NodeClass

  object ConstructorClassNameKey extends NodeField

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    val visibilityModifier = grammars.find(MethodDelta.VisibilityGrammar) as VisibilityKey
    val parseParameters = grammars.find(MethodDelta.ParametersGrammar) as MethodParametersKey
    val block = grammars.find(BlockC.BlockGrammar) as MethodBodyKey
    val constructorGrammar = visibilityModifier ~~ identifier.as(ConstructorClassNameKey) ~ parseParameters % block asNode ConstructorKey
    memberGrammar.addOption(constructorGrammar)
  }

  override def description: String = "Introduces constructors."
}
