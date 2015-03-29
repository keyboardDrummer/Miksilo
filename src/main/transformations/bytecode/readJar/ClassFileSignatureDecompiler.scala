package transformations.bytecode.readJar

import transformations.javac.JavaCompiler

object ClassFileSignatureDecompiler {

  def getDecompiler = JavaCompiler.byteCodeTransformations
}
