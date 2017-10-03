name := "ParticleCompilerSbt"

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-language:implicitConversions"
scalacOptions += "-language:postfixOps"

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "2.5.8"

libraryDependencies += "org.swinglabs" % "swingx" % "1.6.1"

libraryDependencies += "jgraph" % "jgraph" % "5.13.0.0"

libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

libraryDependencies += "com.google.guava" % "guava" % "18.0"

    