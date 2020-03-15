package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{GrammarKey, NodeShape}
import miksilo.modularLanguages.core.deltas.{DeltaWithGrammar, HasShape}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.attributes.{CodeAttributeDelta, InstructionArgumentsKey}

object ConstantPoolIndexGrammar extends GrammarKey
trait InstructionWithGrammar extends DeltaWithGrammar with HasShape
{
  val shape: NodeShape

  override def dependencies = Set(CodeAttributeDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val instructionGrammar = grammars.find(CodeAttributeDelta.InstructionGrammar)
    instructionGrammar.addAlternative(grammars.create(shape, getGrammarForThisInstruction(grammars)))
  }

  def argumentsGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val constantPoolIndex: BiGrammar = find(ConstantPoolIndexGrammar)
    constantPoolIndex.many.as(InstructionArgumentsKey)
  }

  def grammarName: String

  def getGrammarForThisInstruction(grammars: LanguageGrammars): BiGrammar = {
    val arguments = argumentsGrammar(grammars)
    import grammars._
    (grammarName ~~> arguments).asNode(shape)
  }
}
