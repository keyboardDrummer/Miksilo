package deltas.javac.classes.skeleton

import core.deltas.DeltaWithPhase
import core.deltas.path.{NodePath, PathRoot}
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.types.{QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta}

object FullyQualifyTypeReferences extends DeltaWithPhase {
  override def description: String = "Replaces unqualified type references with qualified ones."

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(UnqualifiedObjectTypeDelta.Shape, _type => {
      val reference = compilation.proofs.scopeGraph.findReference(_type).get
      val declaration = compilation.proofs.resolutions(reference).origin.asInstanceOf[NodePath]
      val clazz: JavaClassSkeleton.JavaClass[NodePath] = declaration
      val parts = clazz._package ++ Seq(clazz.name)
      _type.replaceWith(QualifiedObjectTypeDelta.neww(QualifiedClassName(parts)))
    })
  }
}
