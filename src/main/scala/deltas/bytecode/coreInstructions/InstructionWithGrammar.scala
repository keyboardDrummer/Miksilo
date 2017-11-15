package deltas.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.grammars.LanguageGrammars
import core.particles.node.{GrammarKey, NodeClass}
import core.particles.{DeltaWithGrammar, Language}
import deltas.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}

object ConstantPoolIndexGrammar extends GrammarKey
trait InstructionWithGrammar extends DeltaWithGrammar
{
  val key: NodeClass

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(grammars.create(key, getGrammarForThisInstruction(grammars)))
  }

  def argumentsGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val constantPoolIndex: BiGrammar = find(ConstantPoolIndexGrammar)
    (constantPoolIndex | integer).manySeparated(" ").as(InstructionArgumentsKey)
  }

  def grammarName: String

  def getGrammarForThisInstruction(grammars: LanguageGrammars): BiGrammar = {
    val arguments = argumentsGrammar(grammars)
    import grammars._
    (grammarName ~~> arguments).asNode(key)
  }
}
