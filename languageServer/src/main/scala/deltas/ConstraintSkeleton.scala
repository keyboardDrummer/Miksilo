package deltas

import core.deltas.ShapeProperty
import deltas.javac.classes.skeleton.{HasConstraints, HasDeclaration}

object ConstraintSkeleton {

  val hasDeclarations: ShapeProperty[HasDeclaration] = new ShapeProperty[HasDeclaration]
  val hasConstraints: ShapeProperty[HasConstraints] = new ShapeProperty[HasConstraints]

}
