import java.nio.file.{Files, Paths, StandardCopyOption}

import sbt.Keys.{homepage, scmInfo}

import scala.sys.process._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

import scala.reflect.io.File

lazy val miksilo = project
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    skip in publish := true)
  .aggregate(
    editorParser.jvm,
    editorParser.js,
    LSPProtocol.jvm,
    LSPProtocol.js,
    languageServer.jvm,
    languageServer.js,
    modularLanguages.jvm,
    modularLanguages.js,
    playground
  )

lazy val jvm = project
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    skip in publish := true)
  .aggregate(
    editorParser.jvm,
    LSPProtocol.jvm,
    languageServer.jvm,
    modularLanguages.jvm,
    playground
  )

lazy val commonSettings = Seq(

  version := "0.1.9",
  resolvers := Seq("Repo" at "https://oss.sonatype.org/content/repositories/releases/"),
  logLevel := Level.Info,
  logBuffered in Test := false,
  scalaVersion := "2.13.1",
  crossScalaVersions := List("2.13.1"),
  scalacOptions += "-deprecation",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  scalacOptions += "-language:postfixOps",

  publishConfiguration := publishConfiguration.value.withOverwrite(true),
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

  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),

  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test"
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val editorParser = crossProject(JVMPlatform, JSPlatform).
  crossType(CrossType.Full).
  in(file("editorParser")).
  settings(commonSettings: _*).
  jvmSettings(
    assemblySettings,
  ).
  jvmSettings(
    // Only used for SourceUtils, should get rid of it.
    // https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.1"
  )

lazy val LSPProtocol = crossProject(JVMPlatform, JSPlatform).
  crossType(CrossType.Full).
  in(file("LSPProtocol")).
  settings(commonSettings: _*).
  jvmSettings(
    assemblySettings,
  ).
  settings(
    libraryDependencies += "com.malliina" %%% "play-json" % "2.8.1"
  ).
  dependsOn(editorParser)

lazy val languageServer = crossProject(JVMPlatform, JSPlatform).
  crossType(CrossType.Full).
  in(file("languageServer")).
  settings(commonSettings: _*).
  settings(
    mainClass in Compile := Some("miksilo.languageServer.Program")).
  jvmSettings(
    assemblySettings,
    vscode := {
      val assemblyFile: String = assembly.value.getAbsolutePath
      languageServerCommonTask(assemblyFile)
    }
  ).
  jsSettings(

    scalaJSUseMainModuleInitializer := true,
    vscode := {
      val assemblyFile: String = (fastOptJS in Compile).value.data.getAbsolutePath
      languageServerCommonTask(assemblyFile)
    }).
  dependsOn(editorParser % "compile->compile;test->test", LSPProtocol)

def languageServerCommonTask(assemblyFile: String): Unit = {
  val extensionPath = Paths.get(".", "vscode-extension")
  val outPath = extensionPath.resolve("out")
  outPath.toFile.mkdir()
  outPath.resolve("LanguageServer.jar").toFile.delete()
  val extension = assemblyFile.split("\\.").last
  Files.copy(Paths.get(assemblyFile),
    outPath.resolve(s"LanguageServer.$extension"), StandardCopyOption.REPLACE_EXISTING)
  val yarn = Process(enableForWindows(Seq("yarn", "compile")), file("./vscode-extension"))
  val extensionDirectory = Paths.get(".", "vscode-extension").toAbsolutePath
  val vscode = Process(enableForWindows(Seq("code", s"--extensionDevelopmentPath=$extensionDirectory")))
  yarn.#&&(vscode).run
}

def enableForWindows(parts: Seq[String]): Seq[String] = {
  val os = sys.props("os.name").toLowerCase
  os match {
    case x if x contains "windows" => Seq("cmd", "/C") ++ parts
    case _ => parts
  }
}

lazy val modularLanguages = crossProject(JVMPlatform, JSPlatform).
  crossType(CrossType.Full).
  in(file("modularLanguages")).
  settings(commonSettings: _*).
  jvmSettings(
    assemblySettings,
    vscode := {
      val assemblyFile: String = assembly.value.getAbsolutePath
      languageServerCommonTask(assemblyFile)
    },

  ).
  jsSettings(
    scalaJSUseMainModuleInitializer := true,

    vscode := {
      val assemblyFile: String = (fastOptJS in Compile).value.data.getAbsolutePath
      languageServerCommonTask(assemblyFile)
    }).
  settings(
    name := "modularLanguages",
    assemblySettings,
    mainClass in Compile := Some("miksilo.modularLanguages.deltas.Program"),

    // byteCode parser
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",

  ).dependsOn(languageServer,
  editorParser % "compile->compile;test->test" /* for bigrammar testing utils*/ )

lazy val playground = (project in file("playground")).
  settings(commonSettings: _*).
  settings(
    skip in publish := true,

    assemblySettings,
    mainClass in Compile := Some("miksilo.playground.application.Program"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.0.8",
    libraryDependencies += "org.bidib.org.oxbow" % "swingbits" % "1.2.2",
    libraryDependencies += "org.swinglabs" % "swingx" % "1.6.1",
    libraryDependencies += "jgraph" % "jgraph" % "5.13.0.0",
    libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",
  ).dependsOn(modularLanguages.jvm)

lazy val vscode = taskKey[Unit]("Run VS Code with Miksilo")
