package transformations.bytecode.coreInstructions

import core.bigrammar.BiGrammar
import core.particles.DeltaWithGrammar
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.Key
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}

object ConstantPoolIndexGrammar
object InstructionArgumentsGrammar
trait InstructionWithGrammar extends DeltaWithGrammar
{
  val key: Key

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(grammars.create(KeyGrammar(key), getGrammarForThisInstruction(grammars)))
  }

  def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
    val constantPoolIndex: BiGrammar = grammars.findOrCreate(ConstantPoolIndexGrammar, number)
    val arguments = grammars.findOrCreate(InstructionArgumentsGrammar,
      constantPoolIndex.manySeparated(",").inParenthesis.as(InstructionArgumentsKey))
    (name ~> arguments).asNode(key)
  }
}
