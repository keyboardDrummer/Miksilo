package deltas.solidity

import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, DeltaWithPhase}
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.HasNameDelta.Name
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.constructor.ConstructorDelta
import deltas.javac.constructor.ConstructorDelta.ClassName
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import deltas.statement.BlockDelta

object SolidityConstructorDelta extends DeltaWithGrammar with DeltaWithPhase { // TODO try to re-use other constructor delta's.

  override def transformProgram(program: Node, state: Compilation): Unit = {
    program.visitShape(ConstructorDelta.Shape, constructor => {
      constructor.shape = MethodDelta.Shape
      constructor(Name) = ConstructorDelta.constructorName
      constructor(SolidityFunctionDelta.ReturnValues) = Seq.empty
      constructor(MethodDelta.TypeParameters) = Seq.empty
      constructor(AccessibilityFieldsDelta.Static) = false
      constructor.data.remove(ClassName)
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(MethodDelta.Parameters)

    val modifiers = find(SolidityFunctionDelta.Modifiers)
    val blockGrammar: BiGrammar = find(BlockDelta.BlockGrammar)
    val body = blockGrammar.as(MethodDelta.Body)
    val grammar = "constructor" ~ parameterList.as(MethodDelta.Parameters) ~ modifiers ~~ body asNode ConstructorDelta.Shape
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity constructors"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}

