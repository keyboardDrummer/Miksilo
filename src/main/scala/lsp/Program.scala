package lsp

import com.typesafe.scalalogging.LazyLogging
import deltas.javac.JavaLanguage

import scala.util.Try

object Program extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info(s"Starting server in ${System.getenv("PWD")}")

    val server = Try {
      new MiksiloLanguageServer(JavaLanguage.getJava, new StreamConnection(System.in, System.out))
    }
    server.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
  }
}

