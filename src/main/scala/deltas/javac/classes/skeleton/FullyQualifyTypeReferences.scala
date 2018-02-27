package deltas.javac.classes.skeleton

import core.deltas.DeltaWithPhase
import core.deltas.path.PathRoot
import core.language.Compilation
import core.language.node.{FieldLocation, Node}
import deltas.bytecode.types.{QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta}

object FullyQualifyTypeReferences extends DeltaWithPhase {
  override def description: String = "Replaces unqualified type references with qualified ones."

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(UnqualifiedObjectTypeDelta.Shape, _type => {
      val declaration = compilation.proofs.resolveLocation(_type).get.asInstanceOf[FieldLocation].node
      val clazz: JavaClassSkeleton.JavaClass[Node] = declaration
      val parts = clazz._package ++ Seq(clazz.name)
      _type.replaceWith(QualifiedObjectTypeDelta.neww(QualifiedClassName(parts)))
    })
  }
}
