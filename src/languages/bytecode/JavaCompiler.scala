package languages.bytecode

import transformation.TransformationManager

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(Seq(
    AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase, ByteCodeGoTo))
}
