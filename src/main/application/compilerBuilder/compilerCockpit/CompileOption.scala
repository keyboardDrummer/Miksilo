package application.compilerBuilder.compilerCockpit

import core.transformation.sillyCodePieces.Injector

object Compile extends CompileOption {
  override def toString = "Compile"
}

trait CompileOption extends Injector
