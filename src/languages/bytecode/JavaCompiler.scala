package languages.bytecode

import transformation.TransformationManager
import languages.java.base.JavaBase

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(Seq(
    LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase, ByteCodeGoTo))
}
