package util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.{Timer, TimerTask}

import core.deltas._
import core.deltas.exceptions.DeltaDependencyViolation
import core.deltas.node.Node

import scala.collection.mutable
import scala.reflect.io.{Directory, File}

object CompilerBuilder {
  val statistics = new Statistics()

  var compilers : Map[Seq[Delta], TestingCompiler] = Map.empty
  def build(deltas: Seq[Delta], description: String = "testing"): TestingCompiler = {
    val result = compilers.getOrElse(deltas, new TestingCompiler(deltas, description))
    compilers += (deltas -> result)
    result
  }

  def profile[T](description: String, action: => T): T = statistics.profile(description, action)

  val timer = new Timer()
  timer.scheduleAtFixedRate(new MyThread(), 5000, 5000)

  class MyThread extends TimerTask {
    override def run(): Unit = {
      System.out.println("\nProfiling global results:")
      statistics.printAll()
    }
  }
}

class TestingCompiler(val deltas: Seq[Delta], compilerName: String) {
  val statistics = new Statistics(CompilerBuilder.statistics)

  validateDependencies(deltas)
  lazy val language: Language = statistics.profile("build language", buildLanguage)

  def parseAndTransform(input: File): Compilation = {
    val state: Compilation = parseAndTransform(input.inputStream())
    state
  }

  def compile(input: File, outputDirectory: Directory): Compilation = {
    val state: Compilation = parseAndTransform(input.inputStream())

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(language)
    state.program = program
    runPhases(state)
    state
  }

  def spliceBeforeTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    deltas.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ deltas.filter(t => !implicitsSet.contains(t))
  }

  def replace(marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  private def runPhases(state: Compilation) = {
    statistics.profile("running phases", {
      for(phase <- language.compilerPhases)
        statistics.profile("run " + phase.description, phase.action(state))
    })
  }

  def parse(input: InputStream): Node = {
    val state = new Compilation(language)
    justParse(input, state)
    state.program
  }

  private def justParse(input: InputStream, state: Compilation): Unit = {
    statistics.profile("parse", state.program = language.parse(input))
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val state = new Compilation(language)
    justParse(input, state)
    runPhases(state)
    state
  }

  def buildLanguage: Language = {
    val result = new Language()
    for(particle <- deltas.reverse)
    {
      statistics.profile("inject " + particle.name, particle.inject(result))
    }
    result
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(transformations: Seq[Delta]): Unit = {
    var available = Set.empty[Contract]
    for (transformation <- transformations.reverse) {
      transformation.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
          throw DeltaDependencyViolation(dependency, transformation)
      )
      available += transformation
    }
  }
}

class Statistics(parent: Statistics = null) {

  def profile[T](description: String, action: => T): T = {
    val start = System.nanoTime()
    val result = action
    val end = System.nanoTime()
    val timing = (end - start)/1000000.0
    if (parent != null)
      parent.add(description, timing)
    add(description, timing)
    result
  }

  val timingsPerKey: mutable.Map[Any, mutable.ArrayBuffer[Double]] = mutable.Map.empty

  def add(key: Any, timing: Double): Double = {
    val existing = timingsPerKey.getOrElseUpdate(key, mutable.ArrayBuffer.empty)
    existing += timing
    val average = existing.sum / existing.length
    average
  }

  def printAll(): Unit = {

    for(timingsForKey <- timingsPerKey.toSeq.sortBy(p => -1 * p._2.sum)) {
      val timings = timingsForKey._2
      val average = "%05.1f".format(timings.sum / timings.length)
      val total = timings.sum
      val totalString = "%06.0f".format(total)
      if (total > 10)
        System.out.println(s"${totalString}ms total, ${average}ms average, for ${timingsForKey._1}")
    }
  }
}