package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.{CompilationState, DeltaWithGrammar}
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.Key
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}

object ConstantPoolIndexGrammar
trait InstructionWithGrammar extends DeltaWithGrammar
{
  val key: Key

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(grammars.create(KeyGrammar(key), getGrammarForThisInstruction(grammars)))
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
