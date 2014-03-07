package languages.javac.testing

import org.junit.Test
import languages.javac.{LiteralC, ConstructorC, JavaCompiler}
import languages.bytecode._
import languages.javac.base.JavaTypes
import transformation.MetaObject
import languages.javac.base.JavaClassModel._
import scala.reflect.io.{Path, File, Directory}
import languages.javac.base.JavaMethodModel._
import languages.javac.base.JavaTypes._
import languages.javac.base.JavaBaseModel._
import languages.javac.base.QualifiedClassName
import languages.bytecode.LineNumberRef
import languages.bytecode.SameFrame

class TestFibonacciWithMainCompilation {
  val className = "Fibonacci"
  val defaultPackage = Seq("languages","bytecode","testing")
  val other = new TestFibonacciCompilation()

  @Test
  def runCompiledFibonacci() {
    val fibonacci = getJavaFibonacciWithMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val bytes = PrintByteCode.print(byteCode)
    val tempDirectory = Directory.makeTemp()
    val file = File.apply(tempDirectory / Path(className).addExtension("class"))
    val writer = file.bufferedWriter()
    writer.append(bytes)
    writer.close()
    val processBuilder = new ProcessBuilder("java",file.path)
    processBuilder.inheritIO()
    processBuilder.start()
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(objectType(new QualifiedClassName(Seq("java","lang","String"))))))
    val fibCall = call(variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(call(selector(selector(selector(selector(variable("java"),"lang"),"System"),"out"),"print"), Seq(fibCall)))
    method("main", VoidType, parameters, body, static = true, PublicVisibility)
  }

  @Test
  def compileAndPrintFibonacciWithMain() {
    val fibonacci = getJavaFibonacciWithMain
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    PrintByteCode.print(compiledCode)
  }

  def getJavaFibonacciWithMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getMainMethodJava, other.getFibonacciMethodJava))
  }

  @Test
  def compileAndValidateFibonacciWithMain() {
    val fibonacciJava = getJavaFibonacciWithMain
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacciJava)
    val expectedCode = getExpectedUnoptimizedFibonacciWithMainByteCode
    TestUtils.testMethodEquivalence(expectedCode, compiledCode)
    TestUtils.compareConstantPools(expectedCode, compiledCode)
  }

  val methodName = "fibonacci"
  def getExpectedUnoptimizedFibonacciWithMainByteCode: MetaObject = {
    val constantPool: Seq[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethodByteCode
    val nativeClass = ByteCode.clazz(3, 4, constantPool, Seq(other.getConstructorByteCode, getMainByteCode, method))
    nativeClass
  }

  def getMainByteCode = {
    val instructions = Seq(ByteCode.getStatic(2),
      ByteCode.integerConstant(5),
      ByteCode.invokeStatic(3),
      ByteCode.invokeVirtual(4),
      ByteCode.voidReturn)
    ByteCode.methodInfo(11, 12, Seq(ByteCode.codeAttribute(9, 2, 1, instructions, Seq(), Seq())),
      Set(ByteCode.PublicAccess, ByteCode.StaticAccess))
  }

  def getFibonacciMethodByteCode: MetaObject = {
    val instructions = Seq(
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.ifIntegerCompareGreater(3),
      ByteCode.integerConstant(1),
      ByteCode.goTo(16),
      ByteCode.integerConstant(0),
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
    val method = ByteCode.methodInfo(13, 14, Seq(ByteCode.codeAttribute(9, 3, 1, instructions, Seq(), Seq())),
      Set(ByteCode.PublicAccess, ByteCode.StaticAccess))
    method
  }

  def getConstantPool: Seq[Any] = {
    val constantPool = Seq(ByteCode.methodRef(6, 18),
      ByteCode.fieldRef(19,20),
      ByteCode.methodRef(5, 21),
      ByteCode.methodRef(22,23),
      ByteCode.classRef(24),
      ByteCode.classRef(25),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      ByteCode.CodeAttributeId,
      "main",
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq(
        JavaTypes.arrayType(JavaTypes.objectType(new QualifiedClassName(Seq("java","lang","String")))))),
      methodName,
      ByteCode.methodDescriptor(JavaTypes.IntegerType, Seq(JavaTypes.IntegerType)),
      ByteCode.nameAndType(7, 8),
      ByteCode.classRef(26),
      ByteCode.nameAndType(27, 28),
      ByteCode.nameAndType(13,14),
      ByteCode.classRef(29),
      ByteCode.nameAndType(30,31),
      new QualifiedClassName(defaultPackage ++ Seq(className)),
      new QualifiedClassName(Seq("java", "lang", "Object")),
      new QualifiedClassName(Seq("java", "lang", "System")),
      "out",
      JavaTypes.objectType(new QualifiedClassName(Seq("java","io","PrintStream"))),
      new QualifiedClassName(Seq("java","io","PrintStream")),
      "print",
      ByteCode.methodDescriptor(JavaTypes.VoidType,Seq(JavaTypes.IntegerType))
    )
    constantPool
  }
}
