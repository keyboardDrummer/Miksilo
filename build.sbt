
lazy val miksilo = project
  .in(file("."))
  .aggregate(
    languageServer,
    playground,
  )


lazy val commonSettings = Seq(

  version := "1.0",
  resolvers += "dhpcs at bintray" at "https://dl.bintray.com/dhpcs/maven",
  logLevel := Level.Info,
  logBuffered in Test := false,
  scalaVersion := "2.12.3",
  scalacOptions += "-deprecation",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  scalacOptions += "-language:postfixOps",

  libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

  libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.3",

  libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6",

  libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.12" % "2.0.0",

  libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",

  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",

  libraryDependencies += "com.google.guava" % "guava" % "18.0",

  libraryDependencies += "io.github.shogowada" %% "scala-json-rpc" % "0.9.3",

  // https://mvnrepository.com/artifact/com.typesafe.play/play-json
  libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9",

  // https://mvnrepository.com/artifact/com.typesafe.scala-logging/scala-logging
  libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",

  libraryDependencies += "com.github.dragos" %% "languageserver" % "0.2.1",
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val languageServer = (project in file("languageServer")).
  settings(commonSettings: _*).
  settings(
    name := "MiksiloLspServer",
    assemblySettings,
    mainClass in Compile := Some("languageServer.lsp.Program"),
  )

lazy val playground = (project in file("playground")).
  settings(commonSettings: _*).
  settings(
    name := "MiksiloPlayground",
    assemblySettings,
    mainClass in Compile := Some("application.Program"),
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "2.5.8",
    libraryDependencies += "org.bidib.org.oxbow" % "swingbits" % "1.2.2",
    libraryDependencies += "org.swinglabs" % "swingx" % "1.6.1",
    libraryDependencies += "jgraph" % "jgraph" % "5.13.0.0",
    libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5",
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1",
  ).dependsOn(languageServer)