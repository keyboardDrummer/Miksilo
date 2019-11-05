import sbt.Keys.{homepage, scmInfo}

import scala.sys.process._

lazy val miksilo = project
  .in(file("."))
  .aggregate(
    editorParser,
    LSPProtocol,
    languageServer,
    modularLanguages,
    playground
  )

lazy val commonSettings = Seq(

  version := "1.0",
  resolvers += "dhpcs at bintray" at "https://dl.bintray.com/dhpcs/maven",
  logLevel := Level.Info,
  logBuffered in Test := false,
  scalaVersion := "2.12.4",
  scalacOptions += "-deprecation",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  scalacOptions += "-language:postfixOps",

  libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",

  // https://mvnrepository.com/artifact/com.typesafe.scala-logging/scala-logging
)

lazy val assemblySettings = Seq(

  assemblyJarName in assembly := name.value + ".jar",
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val editorParser = (project in file("editorParser")).
  settings(commonSettings: _*)

lazy val LSPProtocol = (project in file("LSPProtocol")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies += "com.dhpcs" %% "scala-json-rpc" % "2.0.1",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7",
  ).dependsOn(editorParser)

lazy val languageServer = (project in file("languageServer")).
  settings(commonSettings: _*).
  settings(
    assemblySettings,

    organization := "com.github.keyboardDrummer",
    homepage := Some(url("http://keyboarddrummer.github.io/Miksilo/")),
    scmInfo := Some(ScmInfo(url("https://github.com/keyboardDrummer/Miksilo"),
      "git@github.com:keyboardDrummer/Miksilo.git")),
    developers := List(Developer("keyboardDrummer",
      "Remy Willems",
      "rgv.willems@gmail.com",
      url("https://github.com/keyboardDrummer"))),
    licenses += ("MIT", url("https://github.com/keyboardDrummer/Miksilo/blob/master/LICENSE")),
    publishMavenStyle := true,

    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7",

    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),

  ).dependsOn(editorParser % "compile->compile;test->test", LSPProtocol)

lazy val modularLanguages = (project in file("modularLanguages")).
  settings(commonSettings: _*).
  settings(
    name := "modularLanguages",
    assemblySettings,
    mainClass in Compile := Some("deltas.Program"),
    vscode := {
      val assemblyFile: String = assembly.value.getAbsolutePath
      val extensionDirectory: File = file("./vscode-extension").getAbsoluteFile
      val tsc = Process("tsc", file("./vscode-extension"))
      val vscode = Process(Seq("code", s"--extensionDevelopmentPath=$extensionDirectory"),
        None,
        "MIKSILO" -> assemblyFile)

      tsc.#&&(vscode).run
    },

    // byteCode parser
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",

    //import com.google.common.primitives.{Ints, Longs}
    libraryDependencies += "com.google.guava" % "guava" % "18.0",

    // https://mvnrepository.com/artifact/com.typesafe.play/play-json
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9",

  ).dependsOn(languageServer,
    editorParser % "compile->compile;test->test" /* for bigrammar testing utils*/ )

lazy val playground = (project in file("playground")).
  settings(commonSettings: _*).
  settings(
    assemblySettings,
    mainClass in Compile := Some("application.Program"),
    libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.12" % "2.0.0",
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "2.5.8",
    libraryDependencies += "org.bidib.org.oxbow" % "swingbits" % "1.2.2",
    libraryDependencies += "org.swinglabs" % "swingx" % "1.6.1",
    libraryDependencies += "jgraph" % "jgraph" % "5.13.0.0",
    libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",
  ).dependsOn(modularLanguages)

lazy val vscode = taskKey[Unit]("Run VS Code with Miksilo")
