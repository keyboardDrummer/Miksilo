package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{DeltaWithGrammar, DeltaWithPhase}
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.constructor.ConstructorDelta
import miksilo.modularLanguages.deltas.classes.constructor.ConstructorDelta.ClassName
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta}
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta

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
    find(ClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity constructors"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}

