package transformations.bytecode.attributes

import core.grammarDocument.{ManyVertical, BiGrammar}
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject}
import transformations.bytecode.ByteCodeSkeleton

object InstructionArgumentsKey

trait Instruction {


  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put(InstructionArgumentsKey, arguments)
  }

  def getInstructionArguments(instruction: MetaObject) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]

  def setInstructionArguments(instruction: MetaObject, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }
}

object CodeAttribute extends GrammarTransformation with Instruction {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def codeAttribute(nameIndex: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[MetaObject],
                    exceptionTable: Seq[MetaObject],
                    attributes: Seq[MetaObject]) = {
    new MetaObject(CodeKey) {
      data.put(ByteCodeSkeleton.AttributeNameKey, nameIndex)
      data.put(CodeMaxStackKey, maxStack)
      data.put(CodeMaxLocalsKey, maxLocals)
      data.put(CodeInstructionsKey, instructions)
      data.put(CodeExceptionTableKey, exceptionTable)
      data.put(CodeAttributesKey, attributes)
    }
  }

  def getCodeAnnotations(clazz: MetaObject): Seq[MetaObject] = {
    ByteCodeSkeleton.getMethods(clazz)
      .flatMap(methodInfo => ByteCodeSkeleton.getMethodAttributes(methodInfo))
      .flatMap(annotation => if (annotation.clazz == CodeKey) Some(annotation) else None)
  }

  def getCodeMaxStack(code: MetaObject) = code(CodeMaxStackKey).asInstanceOf[Int]

  def getCodeMaxLocals(code: MetaObject) = code(CodeMaxLocalsKey).asInstanceOf[Int]

  def getCodeExceptionTable(code: MetaObject) = code(CodeExceptionTableKey).asInstanceOf[Seq[MetaObject]]

  def getCodeAttributes(code: MetaObject) = code(CodeAttributesKey).asInstanceOf[Seq[MetaObject]]

  def getCodeInstructions(code: MetaObject) = code(CodeInstructionsKey).asInstanceOf[Seq[MetaObject]]

  object CodeAttributeId

  object CodeKey

  object CodeMaxStackKey

  object CodeMaxLocalsKey

  object CodeInstructionsKey

  object CodeExceptionTableKey

  object CodeAttributesKey

  object InstructionGrammar

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val codeAttributeConstantGrammar = "Code" ~> produce(CodeAttributeId)
    val constantPoolItemContent = grammars.find(ByteCodeSkeleton.ConstantPoolItemContentGrammar)
    constantPoolItemContent.addOption(codeAttributeConstantGrammar)

    val instructionGrammar = grammars.create(InstructionGrammar)

    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    val parseInstruction: BiGrammar = instructionGrammar
    val codeAttributeGrammar = Seq("nameIndex:" ~> number, "maxStack:" ~> number, "maxLocal:" ~> number).reduce((l,r) => (l <~ ",") ~~ r) %
      new ManyVertical(parseInstruction) %
      ("attributes:" %> new ManyVertical(attributeGrammar).indent()) ^^
      parseMap(CodeKey, ByteCodeSkeleton.AttributeNameKey, CodeMaxStackKey, CodeMaxLocalsKey, CodeInstructionsKey, CodeAttributesKey)

    attributeGrammar.addOption(codeAttributeGrammar)
  }
}
