package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, DeltaWithPhase}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.javac.constructor.{ConstructorDelta, SuperCallExpression}
import deltas.javac.constructor.ConstructorDelta.ClassName
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import deltas.statement.BlockDelta
import deltas.HasNameDelta.Name

object SolidityConstructorDelta extends DeltaWithGrammar with DeltaWithPhase { // TODO try to re-use other constructor delta's.

  object Shape extends NodeShape

  override def transformProgram(program: Node, state: Compilation): Unit = {
    program.visitShape(Shape, constructor => {
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
    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = blockGrammar.as(MethodDelta.Body)
    val grammar = "constructor" ~ parameterList.as(MethodDelta.Parameters) ~ modifiers ~~ body asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity constructors"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}

