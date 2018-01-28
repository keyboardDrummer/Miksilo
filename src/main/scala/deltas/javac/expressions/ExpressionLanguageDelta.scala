package deltas.javac.expressions

import core.deltas.path.NodePathRoot
import core.deltas.{Compilation, Delta}
import core.language.Language
import core.nabl.ConstraintBuilder
import deltas.bytecode.types.IntTypeDelta

//TODO. This delta is WIP and currently just used for testing.
object ExpressionLanguageDelta extends Delta {
  override def description: String = "Takes an expression as a program and prints the result of that expression"

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation: Compilation, builder: ConstraintBuilder) => {
      val _type = ExpressionSkeleton.getType(compilation, builder, NodePathRoot(compilation.program), builder.newScope())
      builder.typesAreEqual(_type, IntTypeDelta.constraintType) //TODO generalize to not only ints.
    }
  }
}
