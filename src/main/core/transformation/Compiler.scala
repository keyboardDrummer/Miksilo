package core.transformation

import java.io.OutputStream

import transformations.bytecode.PrintByteCode

import scala.reflect.io.{Directory, File, Path}

trait Compiler {
  def parse(input: String): MetaObject

  def transform(program: MetaObject): MetaObject

  def compile(input: File, outputDirectory: Directory) {
    val inputStream = File(input).slurp()
    outputDirectory.createDirectory()
    val byteCodeFile = File.apply(outputDirectory / Path(input.name).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    compile(inputStream, writer)
    writer.close()
  }

  def compile(input: String, output: OutputStream) {
    val inputAST = parse(input)
    val code = transform(inputAST)
    val bytes = PrintByteCode.getBytes(code).toArray
    output.write(bytes)
    output.close()
  }
}
