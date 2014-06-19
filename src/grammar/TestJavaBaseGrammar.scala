package grammar

import org.junit.{Assert, Test}
import transformation.{MetaObject, TransformationManager}
import languages.javac.base._
import scala.reflect.io.{Path, File}
import scala.util.parsing.input.StreamReader
import languages.javac.base.model.{JavaClassModel, JavaImport}

class TestJavaBaseGrammar {

  @Test
  def testBasicClass {
    val input = "package bla; class Help {}"
    val parser = TransformationManager.buildParser(Seq(JavaBaseParse))
    val result = parser(input).get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacci {
    val currentDir = Path(".")
    val inputFile = Path("src") / "languages" / "javac" / "testing" / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()
    val parser = TransformationManager.buildParser(Seq(JavaBaseParse))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)
    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }
}
