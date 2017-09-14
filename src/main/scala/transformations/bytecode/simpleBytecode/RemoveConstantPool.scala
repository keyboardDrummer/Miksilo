package transformations.bytecode.simpleBytecode

import core.bigrammar.{Choice, GrammarReference, RootGrammar}
import core.grammar.StringLiteral
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node
import core.particles.path.{OriginWithParent, PathRoot}
import core.particles.{CompilationState, DeltaWithGrammar, DeltaWithPhase}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton.{ClassConstantPool, ConstantPoolGrammar, ConstantPoolItemContentGrammar}
import transformations.bytecode.constants.FieldDescriptorConstant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.types.TypeSkeleton
import transformations.javac.classes.ConstantPool
import transformations.javac.types.MethodTypeC

object RemoveConstantPool extends DeltaWithPhase with DeltaWithGrammar {
  override def transform(program: Node, state: CompilationState): Unit = {
    val pool = new ConstantPool()
    program(ByteCodeSkeleton.ClassConstantPool) = pool
    val constantTypes = ByteCodeSkeleton.getState(state).constantTypes
    PathRoot(program).visit(afterChildren = node => if (constantTypes.contains(node.clazz)) {
          val index = pool.store(node.current)
          node.asInstanceOf[OriginWithParent].replaceWith(index)
        }, beforeChildren = node => {
          if (node.clazz == FieldDescriptorConstant.key) { //TODO replace this method with something based on constantReferences
            val index = pool.store(node.current)
            node.asInstanceOf[OriginWithParent].replaceWith(index)
            false
          }
          else if (TypeSkeleton.getState(state).instances.contains(node.clazz)) {
              val index = pool.store(node.current)
              node.asInstanceOf[OriginWithParent].replaceWith(index)
            false
          }
          else
            true
        })
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val constantEntry = grammars.find(ConstantPoolItemContentGrammar)
    for(path <- grammars.findPathsToKey(ConstantPoolIndexGrammar))
      path.set(constantEntry | number)

    val constantPoolGrammar = grammars.findPathsToKey(ConstantPoolGrammar).head
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}