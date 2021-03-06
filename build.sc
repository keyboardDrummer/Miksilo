import coursier.Repository
import mill._
import scalalib._
import coursier.maven.MavenRepository
import mill.define.Task

import scala.sys.process.Process
import mill.scalajslib._
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}

trait MiksiloPublishModule extends PublishModule {

  def publishVersion = "0.1.10"

  def pomSettings = PomSettings(
    description = "Libraries for modular programming language construction",
    organization = "com.github.keyboardDrummer",
    url = "https://github.com/keyboardDrummer/Miksilo",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("keyboardDrummer", "Miksilo"),
    developers = Seq(
      Developer("keyboardDrummer", "Remy Willems", "https://github.com/keyboardDrummer")
    )
  )
}

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.13.1"

  override def repositoriesTask: Task[Seq[Repository]] = super.repositoriesTask.map(r => {
    r ++ Seq(
      // Not sure if this is used
      MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
      // Not sure if this is used
      MavenRepository("https://dl.bintray.com/dhpcs/maven"),
      MavenRepository("https://repo1.maven.org/maven2/")
    )
  })
}

trait CommonMainModule extends CommonModule with MavenModule with MiksiloPublishModule {

  override def sources = T.sources(
    millSourcePath / "src" / "main" / "java",
    millSourcePath / "src" / "main" / "scala",
    millSourcePath / os.up / "shared" / "src" / "main" / "java",
    millSourcePath / os.up / "shared" / "src" / "main" / "scala"
  )

  trait MyModuleTests extends ScalaModuleTests with MavenModuleTests {
    override def sources = T.sources(
        millSourcePath / "src" / "test" / "scala",
        millSourcePath / "src" / "test" / "java",
        millSourcePath / os.up / "shared" / "src" / "test" / "java",
        millSourcePath / os.up / "shared" / "src" / "test" / "scala"
      )

    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.1.1")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
  trait Tests extends MyModuleTests
}

trait CommonScalaJSModule extends ScalaJSModule with CommonMainModule {
  def scalaJSVersion = "1.5.0"

  trait Tests extends super.Tests with TestScalaJSModule {
    def scalaJSVersion = "1.5.0"
    override def scalaVersion = "2.13.1"

    override def sources = {
      T.sources(
        millSourcePath / "src" / "test" / "scala",
        millSourcePath / "src" / "test" / "java",
      )
    }
  }
}

trait CommonJVMModule extends CommonMainModule {
  trait Tests extends super.Tests {
    override def scalaVersion = "2.13.1"
  }
}

object editorParser extends Module {
  object js extends CommonScalaJSModule {
//    object test extends Tests
  }
  object jvm extends CommonJVMModule {
    override def ivyDeps = Agg(ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}")
    object test extends Tests {
      override def moduleDeps = super.moduleDeps
    }
  }

}

object LSPProtocol extends Module {
  object js extends CommonScalaJSModule {
    override def ivyDeps = Agg(ivy"com.malliina::play-json::2.8.1")
    override def moduleDeps = Seq(editorParser.js)
//    object test extends Tests
  }
  object jvm extends CommonJVMModule {
    override def moduleDeps = Seq(editorParser.jvm)
    override def ivyDeps = Agg(ivy"com.malliina::play-json:2.8.1")
    object test extends Tests {
    }
  }

}

object languageServer extends Module {
  object js extends CommonScalaJSModule {
    override def moduleDeps = Seq(LSPProtocol.js)
//    object test extends Tests
  }
  object jvm extends CommonJVMModule {
    override def moduleDeps = Seq(LSPProtocol.jvm)
    object test extends Tests {
      override def moduleDeps = super.moduleDeps ++ Seq(
        /* For TestUtils */ editorParser.jvm.test,
        /* For SourceUtils */ editorParser.jvm)
    }
  }

}

object modularLanguages extends Module {
  object js extends CommonScalaJSModule with DefinesLanguageServerModule {
    override def ivyDeps = Agg(ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2")
    override def moduleDeps = super.moduleDeps ++ Seq(languageServer.js)
//    object test extends Tests
  }
  object jvm extends CommonJVMModule with DefinesLanguageServerModule {
    override def ivyDeps = Agg(ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2")
    override def moduleDeps = Seq(languageServer.jvm)
    object test extends Tests {
      override def moduleDeps = super.moduleDeps ++ Seq(editorParser.jvm.test)
    }
  }

}

object playground extends CommonMainModule {

  override def moduleDeps = Seq(modularLanguages.jvm)
  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-swing:3.0.0",
    ivy"com.fifesoft:rsyntaxtextarea:3.0.8",
    ivy"org.bidib.org.oxbow:swingbits:1.2.2",
    ivy"org.swinglabs:swingx:1.6.1",
    ivy"jgraph:jgraph:5.13.0.0",
    ivy"org.tinyjee.jgraphx:jgraphx:2.3.0.5",
    ivy"org.jgrapht:jgrapht-core:0.9.1",
    ivy"org.apache.commons:commons-math3:3.5"
  )
}

trait DefinesLanguageServerModule extends ScalaModule {

  def extensionPath = os.pwd / "vscode-extension"

  def vscodePrePublish() = T.command {
    val assemblyPath: PathRef = assembly()
    val outPath = extensionPath  / "out"

    os.copy(assemblyPath.path, outPath / "LanguageServer.jar", replaceExisting = true, createFolders = true)
  }

  def vscode() = T.command {

    vscodePrePublish()()
    val vscode = Process(Seq("code", s"--extensionDevelopmentPath=$extensionPath"), None)
    vscode.!
  }
}

