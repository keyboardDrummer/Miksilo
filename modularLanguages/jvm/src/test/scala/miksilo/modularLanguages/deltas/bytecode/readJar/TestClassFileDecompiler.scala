package miksilo.modularLanguages.deltas.bytecode.readJar

import java.io

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.TestLanguageGrammarUtils
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.modularLanguages.deltas.javac.types.TypeAbstraction
import miksilo.editorParser.SourceUtils
import miksilo.languageServer.util.StreamUtils
import miksilo.modularLanguages.util.TestLanguageBuilder
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.{File, Path}

class TestClassFileDecompiler extends AnyFunSuite {

  private val testingLanguage = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler("", StreamUtils.stringToStream("")))
  test("TypeVariableSimilarToBooleanSignature") {
    val signature = "<B:Ljava/lang/Object;V:Ljava/lang/Object;>(Ljava/lang/Class<TB;>;Ljava/lang/String;Ljava/lang/String;)Lcom/sun/xml/internal/bind/api/RawAccessor<TB;TV;>;"
    val compiler = testingLanguage
    val language = compiler.language

    val grammar = LanguageGrammars.grammars.get(language).find(TypeAbstraction.AbstractMethodTypeGrammar)
    val parser = toParser(grammar)
    val parseResult = parser.parse(signature)
    parseResult.get.asInstanceOf[Node]
  }

  test("TypeVariable") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;"
    val compiler = testingLanguage
    val language = compiler.language

    val parser = toParser(LanguageGrammars.grammars.get(language).find(TypeSkeleton.ByteCodeTypeGrammar))
    val result = parser.parse(signature).get.asInstanceOf[Node]
  }

  test("TypeVariable2") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = testingLanguage
    val language = compiler.language

    val parser = toParser(LanguageGrammars.grammars.get(language).find(TypeSkeleton.ByteCodeTypeGrammar))
    val result = parser.parse(signature).get.asInstanceOf[Node]
  }

  test("TypeVariable3") {
    val signature = "(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = testingLanguage

    val parser = toParser(LanguageGrammars.grammars.get(compiler.language).find(TypeSkeleton.ByteCodeTypeGrammar))
    val result = parser.parse(signature).get.asInstanceOf[Node]
  }

  ignore("decompileRuntimeJar") {
    val currentDir = new File(new io.File("."))
    val testResources = currentDir / Path("resources") / "rtUnzipped"
    val allCassFiles = testResources.toDirectory.deepFiles
    var counter = 0
    val start = 17453
    for(file <- allCassFiles) {
      if (counter >= start && file.extension.contains("class")) {
        val inputStream = file.inputStream()
        val compiler = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler("", inputStream))
        Console.println(s"starting: ${file.name}")
        compiler.compile()
        Console.println(s"progress: $counter / 19.626")
      }
      counter += 1
    }
  }

  test("ObjectClassUnParsedAttributes") {
    val inputStream = SourceUtils.getResourceFile("Object.class")
    val compiler = TestLanguageBuilder.build(Seq(new DecodeFileStream("", inputStream), PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles)
    val state = compiler.compile()

    val expected = SourceUtils.getResourceFileContents("DecodedObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassParsedAttributes") {
    val inputStream = SourceUtils.getResourceFile("Object.class")
    val compiler = TestLanguageBuilder.build(Seq(new DecodeFileStream("", inputStream), ParseKnownAttributes) ++ Seq(PrettyPrint()) ++
      ClassFileSignatureDecompiler.onlySignatureAttribute)
    val state = compiler.compile()

    val expected = SourceUtils.getResourceFileContents("DecodedWithAttributesObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassSignatureDeCompilation") {
    val inputStream = SourceUtils.getResourceFile("Object.class")
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler("", inputStream))
    val compilation = compiler.compile()
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(compilation.program).output

    val expected = SourceUtils.getResourceFileContents("DecompiledObjectClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("StringClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler("", SourceUtils.getResourceFile("String2.class")))
    val compilation = compiler.compile()
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(compilation.program).output

    val expected = SourceUtils.getResourceFileContents("DecompiledStringClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("SystemClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler("",
      SourceUtils.getResourceFile("System.class")))
    val state = compiler.compile()
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(state.program).output

    val expected = SourceUtils.getResourceFileContents("DecompiledSystemClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("PrintStreamClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler("",
      SourceUtils.getResourceFile("PrintStream.class")))
    val state = compiler.compile()
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(state.program).output

    val expected = SourceUtils.getResourceFileContents("DecompiledPrintStreamClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("ParseByteCodeType3") {
    val grammarUtils = new TestLanguageGrammarUtils(testingLanguage.deltas)
    grammarUtils.compareInputWithPrint("([BII)V", None, ByteCodeTypeGrammar)
  }


  test("ParseByteCodeType2") {
    val grammarUtils = new TestLanguageGrammarUtils(testingLanguage.deltas)
    grammarUtils.compareInputWithPrint("[B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType1") {
    val grammarUtils = new TestLanguageGrammarUtils(testingLanguage.deltas)
    grammarUtils.compareInputWithPrint("B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType0") {
    val grammarUtils = new TestLanguageGrammarUtils(testingLanguage.deltas)
    grammarUtils.compareInputWithPrint("I", None, ByteCodeTypeGrammar)
  }
}
