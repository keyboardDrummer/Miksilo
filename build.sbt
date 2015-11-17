name := "ParticleCompilerSbt"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "2.5.8"

libraryDependencies += "org.swinglabs" % "swingx" % "1.6.1"

libraryDependencies += "jgraph" % "jgraph" % "5.13.0.0"

libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1"

libraryDependencies += "junit" % "junit" % "4.12"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"

libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

libraryDependencies += "com.google.guava" % "guava" % "18.0"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.10" % "2.1.3"

unmanagedResourceDirectories in Compile += baseDirectory.value / "testResources"
//resourceDirectory in Test := baseDirectory.value / "testResources"

    