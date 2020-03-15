package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{Node, NodeField}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.constants.FieldRefConstant
import miksilo.modularLanguages.deltas.bytecode.constants.FieldRefConstant.FieldRefWrapper
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object GetStaticDelta extends InstructionInstance {

  object FieldRef extends NodeField

  def getStatic(fieldRefIndex: Any): Node = shape.create(FieldRef -> fieldRefIndex)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = instruction(FieldRef).asInstanceOf[Int]
    hexToBytes("b2") ++ shortToBytes(arguments)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(getReturnType(instruction)))

  def getReturnType(getStatic: Node): Node = {
    val fieldRef: FieldRefWrapper[Node] = getStatic(FieldRef).asInstanceOf[Node]
    fieldRef.nameAndType._type.value
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(FieldRef -> FieldRefConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(FieldRef)
  }

  override def getInstructionSize(compilation: Compilation): Int = 3

  override def description: String = "Defines the getStatic instruction, which retrieves a value from a static field."

  override def grammarName = "getstatic"
}
