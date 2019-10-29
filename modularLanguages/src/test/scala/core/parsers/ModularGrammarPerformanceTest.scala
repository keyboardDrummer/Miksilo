package core.parsers

import deltas.json.JsonLanguage
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}

class ModularGrammarPerformanceTest extends FunSuite {

  val asapJson = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas)

  test("test whether correct inputs always return a ready in one go") {
    val input = """{
                  |  "AWSTemplateFormatVersion" : "2010-09-09",
                  |
                  |  "Description" : "AWS CloudFormation Sample Template AutoScalingMultiAZWithNotifications: Create a multi-az, load balanced and Auto Scaled sample web site running on an Apache Web Serever. The application is configured to span all Availability Zones in the region and is Auto-Scaled based on the CPU utilization of the web servers. Notifications will be sent to the operator email address on scaling events. The instances are load balanced with a simple health check against the default web page. **WARNING** This template creates one or more Amazon EC2 instances and an Application Load Balancer. You will be billed for the AWS resources used if you create a stack from this template.",
                  |
                  |  "Parameters" : {
                  |
                  |    "VpcId" : {
                  |      "Type" : "AWS::EC2::VPC::Id",
                  |      "Description" : "VpcId of your existing Virtual Private Cloud (VPC)",
                  |      "ConstraintDescription" : "must be the VPC Id of an existing Virtual Private Cloud."
                  |    },
                  |  }
                  |}""".stripMargin
    val result = asapJson.compileString(input)
  }

  test("Errorless JSON performance BiGrammar") {
    val source = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json").
      replaceAll("\\s", "")

    val manySourcesCount = 10
    val maxAttempts = 10
    val tenTimesSource = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    var manyRepetitionsTime = Long.MaxValue
    var manySourcesTime = Long.MaxValue

    def average = (manyRepetitionsTime + manySourcesTime) / (2.0 * manySourcesCount)

    val timeA = System.currentTimeMillis()
    var success = false
    var repetition = 0
    while(!success && repetition < maxAttempts) {
      for(_ <- 1.to(manySourcesCount)) {
        val result = asapJson.compileString(source).diagnostics
        assert(result.isEmpty)
      }

      val timeB = System.currentTimeMillis()
      val result = asapJson.compileString(tenTimesSource).diagnostics
      assert(result.isEmpty)

      val timeC = System.currentTimeMillis()

      manyRepetitionsTime = Math.min(manyRepetitionsTime, timeB - timeA)
      manySourcesTime = Math.min(manySourcesTime, timeC - timeB)
      if (average < 400) {
        success = true
      } else {
        System.out.println(s"current average:$average")
      }
      repetition += 1
    }
    System.out.println(s"manyRepetitions:$manyRepetitionsTime")
    System.out.println(s"manySources:$manySourcesTime")
    System.out.println(s"average:$average")
    assert(success)
  }

  test("Edited") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    val timeA = System.currentTimeMillis()

    val repetitions = 5
    for(_ <- 1.to(repetitions)) {
      val result = asapJson.compileString(program).diagnostics
      assert(result.nonEmpty)
    }
    val timeB = System.currentTimeMillis()
    val elapsedTime = timeB - timeA
    val average = elapsedTime / repetitions
    System.out.println(s"edited average: $average")
    assert(average < 1000)
  }
}
