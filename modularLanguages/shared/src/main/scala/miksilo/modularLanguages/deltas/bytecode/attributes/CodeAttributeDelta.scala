package miksilo.modularLanguages.deltas.bytecode.attributes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.{ClassFile, HasBytes}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.constants.Utf8ConstantDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionInstance.Instruction
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object InstructionArgumentsKey extends NodeField

object CodeAttributeDelta extends ByteCodeAttribute with HasBytes with HasShape {

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

  def setInstructionArguments(instruction: Node, arguments: Seq[Any]): Unit = {
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

  val instructions = new ShapeProperty[InstructionInstance]

  val constantEntry: Node = Utf8ConstantDelta.create("Code")

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.hasBytes.add(language, CodeKey, this)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(
      AttributeNameKey -> Utf8ConstantDelta.shape))
  }

  def getBytes(compilation: Compilation, node: Node): Seq[Byte] = {
    val code: CodeAttribute[Node] = node

    def getInstructionByteCode(instruction: Instruction[Node]): Seq[Byte] = {
      ByteCodeSkeleton.getBytes(compilation, instruction)
    }

    val exceptionTable = code.exceptionTable
    shortToBytes(code.maxStack) ++
      shortToBytes(code.maxLocals) ++
      prefixWithIntLength(() => code.instructions.flatMap(getInstructionByteCode)) ++
      shortToBytes(exceptionTable.length) ++
      exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
      getAttributesByteCode(compilation, code.attributes)
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

  override def shape: NodeShape = CodeKey

  object MaxStackGrammar extends GrammarKey
  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val attributesGrammar = find(ByteCodeSkeleton.AttributesGrammar).as(CodeAttributesKey)
    val instructionGrammar: BiGrammar = create(InstructionGrammar)
    val maxStackGrammar = create(MaxStackGrammar, "stack" ~ ":" ~> integer.as(MaxStack) ~< "," ~ printSpace)
    val maxLocalGrammar = "locals" ~ ":" ~> integer.as(CodeMaxLocalsKey)
    val nameGrammar = "name" ~ ":" ~~> find(ConstantPoolIndexGrammar).as(AttributeNameKey)
    val instructionsGrammar = instructionGrammar.manyVertical.indent().as(Instructions)
    val exceptionTableGrammar = "Exceptions" ~ ":" %> value(Seq.empty[Any])
    val body = (nameGrammar ~ ("," ~~ maxStackGrammar ~ maxLocalGrammar) %
      instructionsGrammar %
      attributesGrammar %
      exceptionTableGrammar.as(CodeExceptionTableKey)).indent()
    val codeGrammar: BiGrammar = (stringToGrammar("Code", reserved = false) ~ ":" %> body).asNode(CodeKey)
    create(CodeKey, codeGrammar)
  }

  override def constantPoolKey: String = "Code"

  override def description: String = "Adds a new bytecode attribute named code. Its main content is a list of instructions."

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
