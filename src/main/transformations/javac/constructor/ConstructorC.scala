package transformations.javac.constructor

import core.exceptions.BadInputException
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.{GrammarTransformation, ParticleWithPhase}
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.InvokeSpecialC
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.classes._
import transformations.javac.methods.MethodC._
import transformations.javac.methods.{CallC, MethodC}
import transformations.javac.statements.BlockC
import transformations.types.VoidTypeC

object ConstructorC extends GrammarTransformation with ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(MethodC, CallC, InvokeSpecialC, LoadAddressC, SuperCallExpression)

  case class BadConstructorNameException(clazz: MetaObject, constructor: MetaObject) extends BadInputException

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    val className = ClassC.getClassName(clazz)
    for (constructor <- getConstructors(clazz)) {
      val constructorClassName = constructor(ConstructorClassNameKey).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw new BadConstructorNameException(clazz, constructor)

      constructor.clazz = MethodC.MethodKey
      constructor(MethodC.MethodNameKey) = SuperCallExpression.constructorName
      constructor(MethodC.ReturnTypeKey) = VoidTypeC.voidType
      constructor(StaticKey) = false
      constructor.data.remove(ConstructorClassNameKey)
    }
  }

  def getConstructors(clazz: MetaObject): Seq[MetaObject] = {
    ClassC.getMembers(clazz).filter(member => member.clazz == ConstructorKey)
  }

  def constructor(className: String, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
                  visibility: Visibility = PublicVisibility) = new MetaObject(ConstructorKey,
    MethodParametersKey -> _parameters, MethodBodyKey -> _body, VisibilityKey -> visibility,
    ConstructorClassNameKey -> className)


  object ConstructorKey

  object ConstructorClassNameKey

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(ClassC.ClassMemberGrammar)
    val visibilityModifier = grammars.find(MethodC.VisibilityGrammar)
    val parseParameters = grammars.find(MethodC.ParametersGrammar)
    val block = grammars.find(BlockC.BlockGrammar)
    val constructorGrammar = visibilityModifier ~~ identifier ~ parseParameters % block ^^
      parseMap(ConstructorKey, VisibilityKey, ConstructorClassNameKey, MethodParametersKey, MethodBodyKey)
    memberGrammar.addOption(constructorGrammar)
  }
}
