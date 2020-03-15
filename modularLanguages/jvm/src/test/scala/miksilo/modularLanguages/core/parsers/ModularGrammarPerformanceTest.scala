package miksilo.modularLanguages.core.parsers

import _root_.core.TestUtils
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction
import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import miksilo.modularLanguages.deltas.yaml.ModularYamlLanguage
import miksilo.editorParser.SourceUtils
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

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

  val smallErrorsTargetTime = PerformanceTest.smallErrorsTargetTime * modularGrammarSlowdown // We only allow the small errors to make the parsing 5ms slower
  test("JSON with small errors bigrammar performance") {
    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallErrorsTargetTime, 300, () => {
      val result = json.compileString(source)
      assert(result.diagnostics.size == 2)
      assert(result.program.childElements.size == 6)
    })
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
