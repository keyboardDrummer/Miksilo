package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.ParticleWithGrammar
import core.particles.grammars.{KeyGrammar, GrammarCatalogue}
import core.particles.node.Key
import transformations.bytecode.attributes.{InstructionArgumentsKey, CodeAttribute}

trait InstructionWithGrammar extends ParticleWithGrammar
{
  val key: Key

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(getGrammarForThisInstruction(grammars))
  }

  def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
    grammars.create(KeyGrammar(key), name ~> integer.manySeparated(",").inParenthesis ^^ parseMap(key, InstructionArgumentsKey))
  }
}
