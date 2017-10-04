package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.NodeClass
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}

object ConstantPoolIndexGrammar
trait InstructionWithGrammar extends DeltaWithGrammar
{
  val key: NodeClass

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(grammars.create(key, getGrammarForThisInstruction(grammars)))
  }

  def argumentsGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val constantPoolIndex: BiGrammar = grammars.find(ConstantPoolIndexGrammar)
    (constantPoolIndex | integer).manySeparated(" ").as(InstructionArgumentsKey)
  }

  def grammarName: String

  def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
    val arguments = argumentsGrammar(grammars)
    (grammarName ~~> arguments).asNode(key)
  }
}
