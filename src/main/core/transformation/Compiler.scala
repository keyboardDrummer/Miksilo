package core.transformation

import scala.reflect.io.{Directory, File}

trait Compiler {
  def compile(input: File, outputDirectory: Directory)
}
