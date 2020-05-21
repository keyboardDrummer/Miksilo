package miksilo.modularLanguages.core.parsers

import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import miksilo.modularLanguages.deltas.yaml.ModularYamlLanguage
import miksilo.editorParser.{SourceUtils, TestUtils}
import miksilo.editorParser.parsers.PerformanceTest
import miksilo.editorParser.parsers.editorParsers.UntilBestAndXStepsStopFunction
import miksilo.modularLanguages.util.TestLanguageBuilder
import org.scalatest.funsuite.AnyFunSuite

class ModularGrammarPerformanceTest extends AnyFunSuite {

  val json = TestLanguageBuilder.buildWithParser(ModularJsonLanguage.deltas, UntilBestAndXStepsStopFunction())

  val modularGrammarSlowdown = 1.4
  val manyRepetitionsTargetTime = PerformanceTest.manyRepetitionsTargetTime * modularGrammarSlowdown
  val manySourcesCount = 10
  val manySourcesTargetTime = PerformanceTest.manySourcesTargetTime * modularGrammarSlowdown

  test("Correct JSON small file bigrammar performance") {
    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")
    TestUtils.runPerformanceTest(manyRepetitionsTargetTime, 100, () => {
      val result = json.compileString(source)
      assert(result.program.childElements.size == 6)
      assert(result.diagnostics.isEmpty)
    })
  }

  test("Correct JSON large file bigrammar performance") {
    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")
    val tenTimesSource = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    TestUtils.runPerformanceTest(manySourcesTargetTime, 10, () => {
      val result = json.compileString(tenTimesSource)
      assert(result.program.childElements.size == manySourcesCount)
      assert(result.diagnostics.isEmpty)
    })
  }

  test("regression") {
    val program = """{"A":"B","Parameters" : {
                    |    "BrokenParameter",
                    |  }
                    |}""".stripMargin
    val compilation = json.compileString(program)
    assert(compilation.diagnostics.nonEmpty)
  }

  val smallErrorsTargetTime = PerformanceTest.smallErrorsTargetTime * modularGrammarSlowdown // We only allow the small errors to make the parsing 5ms slower
  test("JSON with small errors bigrammar performance") {
    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallErrorsTargetTime, 300, () => {
      val result = json.compileString(source)
      assert(result.diagnostics.size == 2)
      assert(result.program.childElements.size == 6)
    })
  }

  test("regression 2") {
    val program = """{"A":"B",
                    |"
                    |}""".stripMargin
    val compilation = json.compileString(program)
    assert(compilation.diagnostics.nonEmpty)
  }

  test("regression 3") {
    val program = """{"A":"B","C",}""".stripMargin
    val compilation = json.compileString(program)
    assert(compilation.diagnostics.nonEmpty)
  }

  val yaml = TestLanguageBuilder.buildWithParser(ModularYamlLanguage.deltasWithoutParser, UntilBestAndXStepsStopFunction())
  test("YAML with small errors bigrammar performance") {
    val source = SourceUtils.getResourceFileContents("yaml/AutoScalingMultiAZWithNotifications_edited.yaml")
    TestUtils.runPerformanceTest(smallErrorsTargetTime * 2, 300, () => {
      val result = yaml.compileString(source)
      assert(result.diagnostics.size == 2)
      assert(result.program.childElements.size == 7)
    })
  }
}
