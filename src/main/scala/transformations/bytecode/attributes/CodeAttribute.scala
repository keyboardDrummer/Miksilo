package transformations.bytecode.attributes

import core.bigrammar.{BiGrammar, Choice, ManyVertical}
import core.document.Empty
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, PathRoot}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.Utf8Constant
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionSignature}
import transformations.bytecode.readJar.ClassFileParser
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object InstructionArgumentsKey extends NodeField

object CodeAttribute extends ByteCodeAttribute with WithState {

  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new Node(_type, InstructionArgumentsKey -> arguments)

  def getInstructionArguments(instruction: Node) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]

  def setInstructionArguments(instruction: Node, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }

  def getInstructionSizeRegistry(state: Language) = getState(state).getInstructionSizeRegistry

  def getInstructionSignatureRegistry(state: Language) = getState(state).getInstructionSignatureRegistry

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def codeAttribute(nameIndex: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[Node],
                    exceptionTable: Seq[Node],
                    attributes: Seq[Node]) = {
    new Node(CodeKey,
      AttributeNameKey -> nameIndex,
      CodeMaxStackKey -> maxStack,
      CodeMaxLocalsKey -> maxLocals,
      CodeInstructionsKey -> instructions,
      CodeExceptionTableKey -> exceptionTable,
      CodeAttributesKey -> attributes)
  }

  trait InstructionSignatureProvider
  {
    def getSignature(instruction: Node, programTypeState: ProgramTypeState, state: Compilation): InstructionSignature
  }

  trait InstructionSideEffectProvider
  {
    def getVariableUpdates(instruction: Node, typeState: ProgramTypeState): Map[Int, Node]
  }

  case class JumpBehavior(movesToNext: Boolean, hasJumpInFirstArgument: Boolean)

  def createState = new State()
  class State {
    val getInstructionSignatureRegistry = new ClassRegistry[InstructionSignatureProvider]
    val getInstructionSizeRegistry = new ClassRegistry[Int]
    val jumpBehaviorRegistry = new ClassRegistry[JumpBehavior]
    val localUpdates = new ClassRegistry[InstructionSideEffectProvider]
  }

  val constantEntry = Utf8Constant.create("Code")

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(CodeKey) = attribute => getCodeAttributeBytes(attribute, state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(
      AttributeNameKey -> Utf8Constant.key))
  }

  def getCodeAttributeBytes(attribute: Node, state: Language): Seq[Byte] = {

    def getInstructionByteCode(instruction: Node): Seq[Byte] = {
      ByteCodeSkeleton.getState(state).getBytes(instruction.clazz)(instruction)
    }

    val exceptionTable = CodeAttribute.getCodeExceptionTable(attribute)
    shortToBytes(CodeAttribute.getCodeMaxStack(attribute)) ++
      shortToBytes(CodeAttribute.getCodeMaxLocals(attribute)) ++
      prefixWithIntLength(() => CodeAttribute.getCodeInstructions(attribute).flatMap(getInstructionByteCode)) ++
      shortToBytes(exceptionTable.length) ++
      exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
      getAttributesByteCode(state, CodeAttribute.getCodeAttributes(attribute))
  }


  def getCodeAnnotations[T <: NodeLike](clazz: T): Seq[T] = {
    ByteCodeSkeleton.getMethods(clazz)
      .flatMap(methodInfo => ByteCodeMethodInfo.getMethodAttributes(methodInfo))
      .flatMap(annotation => if (annotation.clazz == CodeKey) Some(annotation) else None)
  }

  def getCodeMaxStack(code: Node) = code(CodeMaxStackKey).asInstanceOf[Int]

  def getCodeMaxLocals(code: Node) = code(CodeMaxLocalsKey).asInstanceOf[Int]

  def getCodeExceptionTable(code: Node) = code(CodeExceptionTableKey).asInstanceOf[Seq[Node]]

  def getCodeAttributes(code: Node) = code(CodeAttributesKey).asInstanceOf[Seq[Node]]

  def getCodeInstructions(code: Node) = code(CodeInstructionsKey).asInstanceOf[Seq[Node]]


  object CodeKey extends NodeClass

  object CodeMaxStackKey extends NodeField

  object CodeMaxLocalsKey extends NodeField

  object CodeInstructionsKey extends NodeField

  object CodeExceptionTableKey extends NodeField

  object CodeAttributesKey extends NodeField

  object InstructionGrammar

  override def key: Key = CodeKey

  object MaxStackGrammar extends Key
  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val attributesGrammar = grammars.find(ByteCodeSkeleton.AttributesGrammar).as(CodeAttributesKey)
    val instructionGrammar: BiGrammar = grammars.create(InstructionGrammar)
    val maxStackGrammar = grammars.create(MaxStackGrammar, ("stack:" ~> integer <~ ", ").as(CodeMaxStackKey))
    val maxLocalGrammar = "locals:" ~> integer.as(CodeMaxLocalsKey)
    val nameGrammar = "name:" ~~> grammars.find(ConstantPoolIndexGrammar).as(AttributeNameKey)
    val instructionsGrammar = new ManyVertical(instructionGrammar).indent().as(CodeInstructionsKey)
    val exceptionTableGrammar = "Exceptions:" %> produce(Seq.empty[Any])
    val body = (nameGrammar ~ "," ~~ maxStackGrammar ~ maxLocalGrammar %
      instructionsGrammar %
      attributesGrammar %
      exceptionTableGrammar.as(CodeExceptionTableKey)).indent()
    val codeGrammar: BiGrammar = ("Code:" %> body).asNode(CodeKey)
    grammars.create(CodeKey, codeGrammar)
  }

  override def constantPoolKey: String = "Code"

  override def description: String = "Adds a new bytecode attribute named code. Its main content is a list of instructions."

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
