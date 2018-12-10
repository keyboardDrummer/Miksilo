package deltas.bytecode.readJar

import core.language.node.Node
import core.parsers.bytes.{ByteParserWriter, ByteReader}
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.constants._
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton, PrintByteCode}
import deltas.javac.classes.ConstantPool

object ClassFileParser extends ByteParserWriter {

  def parse(bytes: Array[Byte]): ParseResult[Node] = {
    classFileParser.parseNaively(ByteReader(bytes), new EmptyParseState(()))
  }

  lazy val classFileParser: Parser[Node] = {
    for {
      _ <- elems(PrintByteCode.cafeBabeBytes)
      versionCode <- ParseInteger
      constantPool <- constantPoolParser
      accessFlags <- accessFlagParser
      thisIndex <- ParseShort
      superIndex <- ParseShort
      interfaces <- sizedSequenceParser(interfaceParser)
      fields <- sizedSequenceParser(fieldParser)
      methods <- sizedSequenceParser(methodParser)
      attributes <- attributesParser
    } yield ByteCodeSkeleton.neww(thisIndex, superIndex, constantPool, methods, interfaces, fields, attributes)
  }

  def attributesParser: ClassFileParser.Parser[Seq[Node]] = {
    sizedSequenceParser(attributeParser)
  }

  def constantPoolParser: Parser[ConstantPool] = for {
    constantPoolItemCount <- ParseShort.map(s => s - 1)
    constants <- constantsParser(constantPoolItemCount)
  } yield new ConstantPool(constants)

  def constantsParser(constantCount: Int): Parser[List[Any]] = constantCount match {
    case 0 => succeed(List.empty)
    case _ => for {
      constantResult <- constantParser
      result <- constantsParser(constantCount - constantResult.entriesConsumed).map(rest => constantResult.constant :: rest)
    } yield result
  }

  def attributeParser: Parser[Node] = for {
    nameIndex <- ParseShort
    length <- ParseInteger
    bytes <- ParseByte.repN(length)
  } yield UnParsedAttribute.construct(nameIndex, bytes)

  def methodParser: Parser[Node] = for {
    accessFlags <- accessFlagParser
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
    attributes <- attributesParser
  } yield ByteCodeMethodInfo.methodInfo(nameIndex, descriptorIndex, attributes)

  def sizedSequenceParser[T](inner: Parser[T]): Parser[Seq[T]] = for {
    amount <- ParseShort
    items <- inner.repN(amount)
  } yield items

  def fieldParser: Parser[Node] = for {
    accessFlags <- accessFlagParser
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
    attributes <- attributesParser
  } yield ByteCodeFieldInfo.field(nameIndex, descriptorIndex, attributes)

  def interfaceParser: Parser[Int] = ParseShort.map(s => s.toInt)

  def accessFlagParser: Parser[Short] = ParseShort

  def classReferenceParser: Parser[Node] = ParseShort.map(s => ClassInfoConstant.classRef(s))

  def fieldReferenceParser: Parser[Node] = for {
    classRefIndex <- ParseShort
    nameAndTypeIndex <- ParseShort
  } yield FieldRefConstant.fieldRef(classRefIndex, nameAndTypeIndex)

  def methodReferenceParser: Parser[Node] = for {
    classRefIndex <- ParseShort
    nameAndTypeIndex <- ParseShort
  } yield MethodRefConstant.methodRef(classRefIndex, nameAndTypeIndex)


  def interfaceMethodReference: Parser[Node] = for {
    classRefIndex <- ParseShort
    nameAndTypeIndex <- ParseShort
  } yield InterfaceMethodRefConstant.methodRef(classRefIndex, nameAndTypeIndex)

  def utf8Parser: Parser[Node] = ParseUtf8

  def nameAndTypeParser: Parser[Node] = for {
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
  } yield NameAndTypeConstant.nameAndType(nameIndex, descriptorIndex)

  def stringParser = ParseShort.map(index => StringConstant.construct(index))

  def integerParser = ParseInteger.map(integer => IntegerInfoConstant.construct(integer))
  def longParser = ParseLong.map(long => LongInfoConstant.construct(long))
  def doubleParser = ParseDouble.map(double => DoubleInfoConstant.construct(double))

  case class ConstantParseResult(constant: Any, entriesConsumed: Int)
  def consumeOne(parser: Parser[Any]) = parser.map(constant => ConstantParseResult(constant, 1))
  def consumeTwo(parser: Parser[Any]) = parser.map(constant => ConstantParseResult(constant, 2))

  def methodHandleParser: Parser[Any] = for {
    referenceKind <- ParseByte
    referenceIndex <- ParseShort
  } yield MethodHandleConstant.construct(referenceKind, referenceIndex)

  def methodTypeParser = for {
    descriptorIndex <- ParseShort
  } yield MethodTypeConstant.construct(descriptorIndex)

  def invokeDynamicParser = for {
    bootstrapMethodIndex <- ParseShort
    nameAndTypeIndex <- ParseShort
  } yield InvokeDynamicConstant.construct(bootstrapMethodIndex, nameAndTypeIndex)

  def constantParser: Parser[ConstantParseResult] = ParseByte.flatMap {
    case 1 => consumeOne(utf8Parser)
    case 3 => consumeOne(integerParser)
    case 4 => consumeOne(ParseFloat)
    case 5 => consumeTwo(longParser)
    case 6 => consumeTwo(doubleParser)
    case 7 => consumeOne(classReferenceParser)
    case 8 => consumeOne(stringParser)
    case 9 => consumeOne(fieldReferenceParser)
    case 10 => consumeOne(methodReferenceParser)
    case 11 => consumeOne(interfaceMethodReference)
    case 12 => consumeOne(nameAndTypeParser)
    case 15 => consumeOne(methodHandleParser)
    case 16 => consumeOne(methodTypeParser)
    case 18 => consumeOne(invokeDynamicParser)
    case _ => fail("There is no constant starting here.")
  }
}
