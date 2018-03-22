package lsp

import ch.qos.logback.classic.Level
import com.typesafe.scalalogging.LazyLogging
import deltas.javac.JavaLanguage
import org.slf4j.LoggerFactory

import scala.util.Try

object Program extends LazyLogging {
  def main(args: Array[String]): Unit = {
    LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger].setLevel(Level.INFO)
    logger.debug(s"Starting server in ${System.getenv("PWD")}")

    val connection = new JsonRpcConnection(System.in, System.out)
    val server = Try {
      new MiksiloLanguageServer(JavaLanguage.getJava, connection)
    }
    server.recover{case e => logger.error(e.getMessage); e.printStackTrace() }
    connection.start()
  }
}

