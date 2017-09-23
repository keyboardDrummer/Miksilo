package transformations.javac.constructor

import core.particles._
import core.particles.exceptions.BadInputException
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.coreInstructions.InvokeSpecialDelta$
import transformations.bytecode.coreInstructions.objects.LoadAddressDelta$
import transformations.bytecode.types.VoidTypeC
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.methods.MethodC
import transformations.javac.methods.MethodC._
import transformations.javac.methods.call.CallStaticOrInstanceC
import transformations.javac.statements.BlockC

object ConstructorC extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(MethodC, CallStaticOrInstanceC, InvokeSpecialDelta$, LoadAddressDelta$, SuperCallExpression)

  case class BadConstructorNameException(clazz: Node, constructor: Node) extends BadInputException

  override def transform(clazz: Node, state: CompilationState): Unit = {
    val className = clazz.name
    for (constructor <- getConstructors(clazz)) {
      val constructorClassName = constructor(ConstructorClassNameKey).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw new BadConstructorNameException(clazz, constructor)

      constructor.clazz = MethodC.MethodKey
      constructor(MethodC.MethodNameKey) = SuperCallExpression.constructorName
      constructor(MethodC.ReturnTypeKey) = VoidTypeC.voidType
      constructor(MethodC.TypeParameters) = Seq.empty
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


  object ConstructorKey extends Key

  object ConstructorClassNameKey extends Key

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    val visibilityModifier = grammars.find(MethodC.VisibilityGrammar)
    val parseParameters = grammars.find(MethodC.ParametersGrammar)
    val block = grammars.find(BlockC.BlockGrammar)
    val constructorGrammar = (visibilityModifier ~~ identifier ~ parseParameters % block).
      asNode(ConstructorKey, VisibilityKey, ConstructorClassNameKey, MethodParametersKey, MethodBodyKey)
    memberGrammar.addOption(constructorGrammar)
  }

  override def description: String = "Introduces constructors."
}
