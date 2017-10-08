package transformations.bytecode.simpleBytecode

import core.bigrammar.{GrammarReference, RootGrammar}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{Node, NodeClass, NodeField, NodeWrapper}
import core.particles.path.PathRoot
import core.particles.{Compilation, DeltaWithGrammar, DeltaWithPhase, Language}
import transformations.bytecode.ByteCodeMethodInfo.MethodDescriptor
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolGrammar
import transformations.bytecode.constants.FieldRefConstant.ClassInfo
import transformations.bytecode.constants.MethodRefConstant.{ClassRef, MethodRefKey}
import transformations.bytecode.constants.NameAndTypeConstant.Type
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.extraConstants.TypeConstant.TypeConstantWrapper
import transformations.bytecode.extraConstants.{QualifiedClassNameConstantDelta, TypeConstant}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool

object RemoveConstantPool extends DeltaWithPhase with DeltaWithGrammar {
  override def transform(program: Node, state: Compilation): Unit = {
    val pool = new ConstantPool()
    program(ByteCodeSkeleton.ClassConstantPool) = pool
    val constantReferences = ByteCodeSkeleton.getRegistry(state).constantReferences

    PathRoot(program).visit(afterChildren = node => constantReferences.get(node.clazz).foreach(reference => {
      for (entry <- reference) {
        node.current.get(entry._1).foreach(fieldValue => {
          val index = pool.store(fieldValue)
          node.current.data.put(entry._1, index)
        })
      }
    }))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val constantReferences = ByteCodeSkeleton.getRegistry(state).constantReferences

    val constantPoolIndexGrammar = grammars.find(ConstantPoolIndexGrammar)
    for(containerEntry <- constantReferences) {
      val key: Any = containerEntry._1
      val constantFields: Map[NodeField, NodeClass] = containerEntry._2
      val keyGrammar = new RootGrammar(grammars.find(key))
      for(field <- constantFields) {
        val asGrammar = keyGrammar.findAs(field._1)
        val constantRef = asGrammar.findGrammar(constantPoolIndexGrammar).get.asInstanceOf[GrammarReference]
        constantRef.set(grammars.find(field._2))
      }
    }

    grammars.find(Utf8ConstantDelta.key).inner = Utf8ConstantDelta.getConstantEntryGrammar(grammars)
    grammars.find(TypeConstant.key).inner = TypeConstant.getConstantEntryGrammar(grammars)
    grammars.find(QualifiedClassNameConstantDelta.key).inner = QualifiedClassNameConstantDelta.getConstantEntryGrammar(grammars)
    grammars.find(MethodRefConstant.key).inner = (grammars.find(ClassInfoConstant.key).as(ClassRef) ~< "." ~
      grammars.find(NameAndTypeConstant.key).as(MethodRefConstant.NameAndType)) asNode MethodRefKey
    grammars.find(ClassInfoConstant.key).inner = grammars.find(QualifiedClassNameConstantDelta.key).as(ClassInfoConstant.Name) asNode ClassInfoConstant.Clazz
    grammars.find(FieldRefConstant.key).inner = grammars.find(ClassInfoConstant.key).as(ClassInfo) ~ "." ~
      grammars.find(NameAndTypeConstant.key).as(FieldRefConstant.NameAndType) asNode FieldRefConstant.key
    grammars.find(NameAndTypeConstant.key).inner = grammars.find(Utf8ConstantDelta.key).as(NameAndTypeConstant.Name) ~~
      grammars.find(TypeConstant.key).as(Type) asNode NameAndTypeConstant.Clazz
    grammars.find(QualifiedClassNameConstantDelta.key).inner = QualifiedClassNameConstantDelta.getConstantEntryGrammar(grammars)

    val constantPoolGrammar = grammars.find(ProgramGrammar).findLabelled(ConstantPoolGrammar)
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}