package transformations.bytecode

import core.transformation.MetaObject
import org.junit.{Assert, Test}
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}
import transformations.javac.{ConstructorC, TestUtils}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestPrintByteCodeFibonacciWithMain {

  val className = "Fibonacci"
  val fibonacciMethodName = "fibonacci"

  @Test
  def testPrintByteCode() = {
    val expectedHex = "cafe babe 0000 0033 0020 0a00 0600 1209\n0013 0014 0a00 0500 150a 0016 0017 0700\n1807 0019 0100 063c 696e 6974 3e01 0003\n2829 5601 0004 436f 6465 0100 0f4c 696e\n654e 756d 6265 7254 6162 6c65 0100 046d\n6169 6e01 0016 285b 4c6a 6176 612f 6c61\n6e67 2f53 7472 696e 673b 2956 0100 0966\n6962 6f6e 6163 6369 0100 0428 4929 4901\n000d 5374 6163 6b4d 6170 5461 626c 6501\n000a 536f 7572 6365 4669 6c65 0100 0e46\n6962 6f6e 6163 6369 2e6a 6176 610c 0007\n0008 0700 1a0c 001b 001c 0c00 0d00 0e07\n001d 0c00 1e00 1f01 0009 4669 626f 6e61\n6363 6901 0010 6a61 7661 2f6c 616e 672f\n4f62 6a65 6374 0100 106a 6176 612f 6c61\n6e67 2f53 7973 7465 6d01 0003 6f75 7401\n0015 4c6a 6176 612f 696f 2f50 7269 6e74\n5374 7265 616d 3b01 0013 6a61 7661 2f69\n6f2f 5072 696e 7453 7472 6561 6d01 0005\n7072 696e 7401 0004 2849 2956 0020 0005\n0006 0000 0000 0003 0000 0007 0008 0001\n0009 0000 001d 0001 0001 0000 0005 2ab7\n0001 b100 0000 0100 0a00 0000 0600 0100\n0000 0100 0900 0b00 0c00 0100 0900 0000\n2700 0200 0100 0000 0bb2 0002 08b8 0003\nb600 04b1 0000 0001 000a 0000 000a 0002\n0000 0004 000a 0005 0009 000d 000e 0001\n0009 0000 003a 0003 0001 0000 0017 1a05\na200 0704 a700 101a 0464 b800 031a 0564\nb800 0360 ac00 0000 0200 0a00 0000 0600\n0100 0000 0800 0f00 0000 0500 0209 4c01\n0001 0010 0000 0002 0011"
    Assert.assertEquals(expectedHex, PrintByteCode.print(getByteCode))
  }

  def getByteCode: MetaObject = {
    val classAttributes = Seq(ByteCode.sourceFile(16, 17))
    ByteCode.clazz(5, 6, getConstantPool, Seq[MetaObject](getConstructorByteCode, getMainByteCode, getFibonacciMethod), attributes = classAttributes)
  }

  @Test
  def runCompiledFibonacci() {
    val code = getByteCode
    val expectedResult = 8
    TestUtils.runByteCode(className, code, expectedResult)
  }

  def getExpectedUnoptimizedFibonacciWithoutMainByteCode: MetaObject = {
    val constantPool: mutable.Buffer[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethod
    val nativeClass = ByteCode.clazz(3, 4, constantPool, Seq(getConstructorByteCode, getMainByteCode, method))
    nativeClass
  }

  def getFibonacciMethod: MetaObject = {
    val instructions = Seq(
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.ifIntegerCompareGreater(7),
      ByteCode.integerConstant(1),
      ByteCode.goTo(16),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(1),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(3),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(3),
      ByteCode.addInteger,
      ByteCode.integerReturn
    )
    val lineNumberTable = ByteCode.lineNumberTable(10, Seq(new LineNumberRef(8, 0)))
    val stackMapTable = ByteCode.stackMapTable(15, Seq(ByteCode.sameFrame(9),
      ByteCode.sameFrameLocals1StackItem(12, JavaTypes.intType)))
    val method = ByteCode.methodInfo(13, 14, Seq(ByteCode.codeAttribute(9, 3, 1, instructions, Seq(), Seq(lineNumberTable, stackMapTable))),
      Set(ByteCode.PublicAccess, ByteCode.StaticAccess))
    method
  }

  def getConstantPool: mutable.Buffer[Any] = {
    val constantPool = ArrayBuffer[Any](ByteCode.methodRef(6, 18),
      ByteCode.fieldRef(19, 20),
      ByteCode.methodRef(5, 21),
      ByteCode.methodRef(22, 23),
      ByteCode.classRef(24),
      ByteCode.classRef(25),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.voidType, Seq()),
      ByteCode.CodeAttributeId,
      ByteCode.LineNumberTableId,
      "main",
      ByteCode.methodDescriptor(JavaTypes.voidType, Seq(
        JavaTypes.arrayType(JavaTypes.objectType(new QualifiedClassName(Seq("java", "lang", "String")))))),
      fibonacciMethodName,
      ByteCode.methodDescriptor(JavaTypes.intType, Seq(JavaTypes.intType)),
      ByteCode.StackMapTableId,
      ByteCode.SourceFileId,
      "Fibonacci.java",
      ByteCode.nameAndType(7, 8),
      ByteCode.classRef(26),
      ByteCode.nameAndType(27, 28),
      ByteCode.nameAndType(13, 14),
      ByteCode.classRef(29),
      ByteCode.nameAndType(30, 31),
      "Fibonacci",
      new QualifiedClassName(Seq("java", "lang", "Object")),
      new QualifiedClassName(Seq("java", "lang", "System")),
      "out",
      JavaTypes.objectType(new QualifiedClassName(Seq("java", "io", "PrintStream"))),
      "java/io/PrintStream",
      "print",
      ByteCode.methodDescriptor(JavaTypes.voidType, Seq(JavaTypes.intType))
    )
    constantPool
  }

  def getConstructorByteCode: MetaObject = {
    val instructions = Seq(ByteCode.addressLoad(0),
      ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val lineNumberTable = ByteCode.lineNumberTable(10, Seq(new LineNumberRef(1, 0)))
    val codeAttribute = Seq(ByteCode.codeAttribute(9, 1, 1, instructions, Seq(), Seq(lineNumberTable)))
    ByteCode.methodInfo(7, 8, codeAttribute, Set())
  }

  def getMainByteCode: MetaObject = {
    val instructions = Seq(ByteCode.getStatic(2),
      ByteCode.integerConstant(5),
      ByteCode.invokeStatic(3),
      ByteCode.invokeVirtual(4),
      ByteCode.voidReturn)
    val lineNumberTable = ByteCode.lineNumberTable(10, Seq(new LineNumberRef(4, 0), new LineNumberRef(5, 10)))
    ByteCode.methodInfo(11, 12, Seq(ByteCode.codeAttribute(9, 2, 1, instructions, Seq(), Seq(lineNumberTable))),
      Set(ByteCode.PublicAccess, ByteCode.StaticAccess))
  }
}
