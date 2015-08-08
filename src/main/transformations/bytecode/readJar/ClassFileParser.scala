package transformations.bytecode.readJar

import core.particles.node.Node
import transformations.bytecode.attributes.UnParsedAttribute
import transformations.bytecode.constants._
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeFieldInfo, PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool

object ClassFileParser extends ByteParsers {

  def classFileParser: Parser[Node] = {
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
    } yield ByteCodeSkeleton.clazz(thisIndex, superIndex, constantPool, methods, interfaces, fields, attributes)
  }

  def attributesParser: ClassFileParser.Parser[Seq[Node]] = {
    sizedSequenceParser(attributeParser)
  }

  def constantPoolParser: Parser[ConstantPool] = for {
    constantPoolItemCount <- ParseShort.map(s => s - 1)
    constants <- constantsParser(constantPoolItemCount)
  } yield new ConstantPool(constants)

  def constantsParser(constantCount: Int): Parser[List[Any]] = constantCount match {
    case 0 => success(List.empty)
    case _ => for {
      ConstantParseResult(constant, entriesConsumed) <- constantParser
      result <- constantsParser(constantCount - entriesConsumed).map(rest => constant :: rest)
    } yield result
  }

  def attributeParser: Parser[Node] = for {
    nameIndex <- ParseShort
    length <- ParseInteger
    bytes <- repN(length, ParseByte)
  } yield UnParsedAttribute.construct(nameIndex, bytes)

  def methodParser: Parser[Node] = for {
    accessFlags <- accessFlagParser
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
    attributes <- attributesParser
  } yield ByteCodeMethodInfo.methodInfo(nameIndex, descriptorIndex, attributes)

  def sizedSequenceParser[T](inner: Parser[T]): Parser[Seq[T]] = for {
    amount <- ParseShort
    items <- repN(amount, inner)
  } yield items

  def fieldParser: Parser[Node] = for {
    accessFlags <- accessFlagParser
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
    attributes <- attributesParser
  } yield ByteCodeFieldInfo.field(nameIndex, descriptorIndex, attributes)

  def interfaceParser: Parser[Int] = ParseShort.map(s => s.toInt)

  def accessFlagParser: Parser[Short] = ParseShort

  def classReferenceParser: Parser[Node] = ParseShort.map(s => ClassRefConstant.classRef(s))

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

  def utf8Parser: Parser[String] = parseUtf8

  def nameAndTypeParser: Parser[Node] = for {
    nameIndex <- ParseShort
    descriptorIndex <- ParseShort
  } yield NameAndType.nameAndType(nameIndex, descriptorIndex)

  def stringParser = ParseShort.map(index => StringConstant.construct(index))

  def integerParser = ParseInteger.map(integer => IntegerConstant.construct(integer))
  def longParser = ParseLong.map(long => LongConstantEntryC.construct(long))

  case class ConstantParseResult(constant: Any, entriesConsumed: Int)
  def consumeOne(parser: Parser[Any]) = parser.map(constant => new ConstantParseResult(constant, 1))
  def consumeTwo(parser: Parser[Any]) = parser.map(constant => new ConstantParseResult(constant, 2))
  def constantParser: Parser[ConstantParseResult] = ParseByte.into {
    case 1 => consumeOne(utf8Parser)
    case 3 => consumeOne(integerParser)
    case 4 => consumeOne(ParseFloat)
    case 5 => consumeTwo(longParser)
    case 6 => consumeTwo(failure("can't parse double"))
    case 7 => consumeOne(classReferenceParser)
    case 8 => consumeOne(stringParser)
    case 9 => consumeOne(fieldReferenceParser)
    case 10 => consumeOne(methodReferenceParser)
    case 11 => consumeOne(interfaceMethodReference)
    case 12 => consumeOne(nameAndTypeParser)
    case 15 => ???
    case 16 => ???
    case 18 => ???
    case _ => failure("There is no constant starting here.")
  }
}
