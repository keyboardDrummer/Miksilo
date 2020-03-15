package miksilo.modularLanguages.deltas.javac.constructor

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import core.language.exceptions.BadInputException
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.types.VoidTypeDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import miksilo.modularLanguages.deltas.javac.methods.AccessibilityFieldsDelta.PublicVisibility
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta.{Body, Method, Parameters}

object ConstructorDelta extends DeltaWithGrammar with DeltaWithPhase {

  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(MethodDelta)

  case class BadConstructorNameException(javaClass: Node, constructor: Node) extends BadInputException

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: JavaClass[Node] = program
    val className = clazz.name
    for (constructor <- getConstructors(program)) {
      val constructorClassName = constructor(ClassName).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw BadConstructorNameException(program, constructor.node)

      constructor.shape = MethodDelta.Shape
      constructor(Name) = constructorName
      constructor(MethodDelta.ReturnType) = VoidTypeDelta.voidType
      constructor(MethodDelta.TypeParameters) = Seq.empty
      constructor(AccessibilityFieldsDelta.Static) = false
      constructor.data.remove(ClassName)
    }
  }

  def getConstructors[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] = {
    NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == Shape))
  }

  def constructor(className: String, _parameters: Seq[Node], _body: Node,
                  visibility: AccessibilityFieldsDelta.Visibility = PublicVisibility) = new Node(Shape,
    Parameters -> _parameters, Body -> _body, AccessibilityFieldsDelta.VisibilityField -> visibility,
    ClassName -> className)


  object Shape extends NodeShape

  object ClassName extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val visibilityModifier = find(AccessibilityFieldsDelta.VisibilityField)
    val parseParameters = find(MethodDelta.Parameters) as Parameters
    val block = find(BlockDelta.BlockGrammar).as(Body)
    val constructorGrammar = visibilityModifier ~~ identifier.as(ClassName) ~ parseParameters % block asNode Shape
    find(MethodDelta.Shape).addAlternative(constructorGrammar)
  }

  override def description: String = "Introduces constructors."
}
