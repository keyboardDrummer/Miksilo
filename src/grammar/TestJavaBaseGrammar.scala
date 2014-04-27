package grammar

import org.junit.{Assert, Test}
import transformation.{MetaObject, TransformationManager}
import languages.javac.base.{JavaImport, JavaClassModel, JavaBaseModel, JavaBase}

class TestJavaBaseGrammar {

  @Test
  def testBasicClass {
    val input = "package bla; class Help {}"
    val parser = TransformationManager.buildParser(Seq(JavaBase))
    val result = parser(input).get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }
}
