package transformations.bytecode.readJar

import java.io
import java.io.InputStream

import application.compilerCockpit.PrettyPrint
import core.bigrammar.TestGrammarUtils
import core.particles.node.Node
import core.particles.{CompilerFromParticles, ParticlesToParserConverter}
import org.junit.{Assert, Ignore, Test}
import org.scalatest.FunSuite
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import transformations.javac.JavaCompiler
import transformations.javac.types.TypeAbstraction
import util.TestUtils

import scala.reflect.io.{File, Path}

class TestClassFileDecompiler extends FunSuite {


  test("TypeVariableSimilarToBooleanSignature") {
    val signature = "<B:Ljava/lang/Object;V:Ljava/lang/Object;>(Ljava/lang/Class<TB;>;Ljava/lang/String;Ljava/lang/String;)Lcom/sun/xml/internal/bind/api/RawAccessor<TB;TV;>;"
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildState

    val manager = new ParticlesToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeAbstraction.AbstractMethodTypeGrammar), signature).asInstanceOf[Node]
  }

  test("TypeVariable") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;"
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildState

    val manager = new ParticlesToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }


  test("TypeVariable2") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildState

    val manager = new ParticlesToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }


  test("TypeVariable3") {
    val signature = "(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildState

    val manager = new ParticlesToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }

  ignore("decompileRuntimeJar") {
    val currentDir = new File(new io.File("."))
    val testResources = currentDir / Path("resources") / "rtUnzipped"
    val allCassFiles = testResources.toDirectory.deepFiles
    val compiler: CompilerFromParticles = new CompilerFromParticles(/*Seq(new PrettyPrint()) ++*/ ClassFileSignatureDecompiler.getDecompiler)
    var counter = 0
    val start = 17453
    for(file <- allCassFiles) {
      if (counter >= start && file.extension.contains("class")) {
        val inputStream = file.inputStream()
        Console.println(s"starting: ${file.name}")
        compiler.parseAndTransform(inputStream)
        Console.println(s"progress: $counter / 19.626")
      }
      counter += 1
    }
  }


  test("ObjectClassUnParsedAttributes") {
    val inputStream = getInputStream("Object.class")
    val compiler: CompilerFromParticles = new CompilerFromParticles(Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles)
    val state = compiler.parseAndTransform(inputStream)

    val expected = TestUtils.getTestFile("DecodedObjectClassPrettyPrint.txt").slurp()
    assertResult(expected)(state.output)
  }


  test("ObjectClassParsedAttributes") {
    val compiler = new CompilerFromParticles(Seq(ParseKnownAttributes) ++ Seq(new PrettyPrint()) ++
      ClassFileSignatureDecompiler.onlySignatureAttribute)
    val state = compiler.parseAndTransform(getInputStream("Object.class"))

    val expected = TestUtils.getTestFile("DecodedWithAttributesObjectClassPrettyPrint.txt").slurp()
    assertResult(expected)(state.output)
  }


  test("ObjectClassSignatureDeCompilation") {
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(getInputStream("Object.class"))
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = TestUtils.getTestFile("DecompiledObjectClassFileSignature.txt").slurp()
    assertResult(expected)(outputState.output)
  }


  test("StringClassSignatureDeCompilation") {
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(getInputStream("String.class"))
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = TestUtils.getTestFile("DecompiledStringClassFileSignature.txt").slurp()
    assertResult(expected)(outputState.output)
  }


  test("SystemClassSignatureDeCompilation") {
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(getInputStream("System.class"))
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = TestUtils.getTestFile("DecompiledSystemClassFileSignature.txt").slurp()
    assertResult(expected)(outputState.output)
  }


  test("PrintStreamClassSignatureDeCompilation") {
    val compiler = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(getInputStream("PrintStream.class"))
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = TestUtils.getTestFile("DecompiledPrintStreamClassFileSignature.txt").slurp()
    assertResult(expected)(outputState.output)
  }

  def getInputStream(fileName: Path): InputStream = {
    val file = TestUtils.getTestFile(fileName)
    file.inputStream()
  }

  test("ParseByteCodeType3") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("([BII)V", None, ByteCodeTypeGrammar)
  }


  test("ParseByteCodeType2") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("[B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType1") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType0") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("I", None, ByteCodeTypeGrammar)
  }
}
