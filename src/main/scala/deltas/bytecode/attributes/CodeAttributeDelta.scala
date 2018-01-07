package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.coreInstructions.InstructionDelta.Instruction
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import deltas.bytecode.readJar.ClassFileParser
import deltas.bytecode.simpleBytecode.ProgramTypeState

object InstructionArgumentsKey extends NodeField

object CodeAttributeDelta extends ByteCodeAttribute with WithLanguageRegistry {

  implicit class CodeAttribute[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def maxStack: Int = node(MaxStack).asInstanceOf[Int]
    def maxStack_=(value: Int): Unit = node(MaxStack) = value

    def instructions: Seq[Instruction[T]] = NodeWrapper.wrapList(node(Instructions).asInstanceOf[Seq[T]])
    def instructions_=(value: Seq[T]): Unit = node(Instructions) = value

    def maxLocals: Int = node(CodeMaxLocalsKey).asInstanceOf[Int]

    def exceptionTable: Seq[Node] = node(CodeExceptionTableKey).asInstanceOf[Seq[Node]]
    def attributes: Seq[Node] = node(CodeAttributesKey).asInstanceOf[Seq[Node]]
    def attributes_=(value: Seq[Node]): Unit = node(CodeAttributesKey) = value
  }

  def instruction(_type: NodeShape, arguments: Seq[Any] = Seq()) = new Node(_type, InstructionArgumentsKey -> arguments)

  def getInstructionArguments(instruction: Node): Seq[Int] = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]

  def setInstructionArguments(instruction: Node, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def codeAttribute(nameIndex: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[Node],
                    exceptionTable: Seq[Node],
                    attributes: Seq[Node]): CodeAttribute[Node] = {
    new Node(CodeKey,
      AttributeNameKey -> nameIndex,
      MaxStack -> maxStack,
      CodeMaxLocalsKey -> maxLocals,
      Instructions -> instructions,
      CodeExceptionTableKey -> exceptionTable,
      CodeAttributesKey -> attributes)
  }

  trait InstructionSignatureProvider
  {
    def getSignature(instruction: Node, programTypeState: ProgramTypeState, language: Language): InstructionSignature
  }

  trait InstructionSideEffectProvider
  {
    def getVariableUpdates(instruction: Node, typeState: ProgramTypeState): Map[Int, Node]
  }

  case class JumpBehavior(movesToNext: Boolean, hasJumpInFirstArgument: Boolean)

  def createRegistry = new Registry()
  class Registry {
    val instructions = new ShapeRegistry[InstructionDelta] //TODO make this registry obsolete by storing all the data in the NodeShape of the instruction.
  }

  val constantEntry: Node = Utf8ConstantDelta.create("Code")

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes(CodeKey) = attribute => getCodeAttributeBytes(attribute, state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(
      AttributeNameKey -> Utf8ConstantDelta.key))
  }

  def getCodeAttributeBytes(code: CodeAttribute[Node], state: Language): Seq[Byte] = {

    def getInstructionByteCode(instruction: Instruction[Node]): Seq[Byte] = {
      ByteCodeSkeleton.getRegistry(state).getBytes(instruction.shape)(instruction)
    }

    val exceptionTable = code.exceptionTable
    shortToBytes(code.maxStack) ++
      shortToBytes(code.maxLocals) ++
      prefixWithIntLength(() => code.instructions.flatMap(getInstructionByteCode)) ++
      shortToBytes(exceptionTable.length) ++
      exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
      getAttributesByteCode(state, code.attributes)
  }


  def getCodeAnnotations[T <: NodeLike](shape: ClassFile[T]): Seq[CodeAttribute[T]] = {
    shape.methods
      .flatMap(methodInfo => methodInfo.attributes)
      .flatMap(annotation => if (annotation.shape == CodeKey) Some(new CodeAttribute(annotation)) else None)
  }

  object CodeKey extends NodeShape

  object MaxStack extends NodeField

  object CodeMaxLocalsKey extends NodeField

  object Instructions extends NodeField

  object CodeExceptionTableKey extends NodeField

  object CodeAttributesKey extends NodeField

  object InstructionGrammar extends GrammarKey

  override def key: NodeShape = CodeKey

  object MaxStackGrammar extends GrammarKey
  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val attributesGrammar = find(ByteCodeSkeleton.AttributesGrammar).as(CodeAttributesKey)
    val instructionGrammar: BiGrammar = create(InstructionGrammar)
    val maxStackGrammar = create(MaxStackGrammar, "stack" ~ ":" ~> integer.as(MaxStack) ~< ", ")
    val maxLocalGrammar = "locals" ~ ":" ~> integer.as(CodeMaxLocalsKey)
    val nameGrammar = "name" ~ ":" ~~> find(ConstantPoolIndexGrammar).as(AttributeNameKey)
    val instructionsGrammar = instructionGrammar.manyVertical.indent().as(Instructions)
    val exceptionTableGrammar = "Exceptions" ~ ":" %> value(Seq.empty[Any])
    val body = (nameGrammar ~ ("," ~~ maxStackGrammar ~ maxLocalGrammar) %
      instructionsGrammar %
      attributesGrammar %
      exceptionTableGrammar.as(CodeExceptionTableKey)).indent()
    val codeGrammar: BiGrammar = (Keyword("Code", reserved = false) ~ ":" %> body).asNode(CodeKey)
    create(CodeKey, codeGrammar)
  }

  override def constantPoolKey: String = "Code"

  override def description: String = "Adds a new bytecode attribute named code. Its main content is a list of instructions."

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
