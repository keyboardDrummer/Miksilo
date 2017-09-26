package transformations.bytecode.simpleBytecode

import core.bigrammar.{As, GrammarReference, RootGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.PathRoot
import core.particles.{Compilation, DeltaWithGrammar, DeltaWithPhase, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolGrammar
import transformations.bytecode.constants.ClassInfoConstant.{ClassRefKey, ClassRefName}
import transformations.bytecode.constants.FieldRefConstant.{FieldRefClassIndex, FieldRefNameAndTypeIndex}
import transformations.bytecode.constants.MethodRefConstant.{MethodRefClassName, MethodRefKey, MethodRefMethodName}
import transformations.bytecode.constants.NameAndTypeConstant.{NameAndTypeKey, NameAndTypeName, NameAndTypeType}
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.extraConstants.{QualifiedClassNameConstant, TypeConstant}
import transformations.javac.classes.ConstantPool

object RemoveConstantPool extends DeltaWithPhase with DeltaWithGrammar {
  override def transform(program: Node, state: Compilation): Unit = {
    val pool = new ConstantPool()
    program(ByteCodeSkeleton.ClassConstantPool) = pool
    val constantReferences = ByteCodeSkeleton.getState(state).constantReferences

    PathRoot(program).visit(afterChildren = node => constantReferences.get(node.clazz).foreach(reference => {
      for (entry <- reference) {
        val fieldValue = node.current(entry._1)
        val index = pool.store(fieldValue)
        node.current.data.put(entry._1, index)
      }
    }))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val constantReferences = ByteCodeSkeleton.getState(state).constantReferences

    val constantPoolIndexGrammar = grammars.find(ConstantPoolIndexGrammar)
    for(containerEntry <- constantReferences) {
      val key: Any = containerEntry._1
      val constantFields: Map[NodeField, NodeClass] = containerEntry._2
      val keyGrammar = new RootGrammar(grammars.find(key))
      for(field <- constantFields) {
        val asGrammar = keyGrammar.find(p => p.get match {
          case as: As => as.key == field._1
          case _ => false
        }).get
        val constantRef = asGrammar.findGrammar(constantPoolIndexGrammar).get.asInstanceOf[GrammarReference]
        constantRef.set(grammars.find(field._2))
      }
    }

    grammars.find(Utf8Constant.key).inner = Utf8Constant.getConstantEntryGrammar(grammars)
    grammars.find(TypeConstant.key).inner = TypeConstant.getConstantEntryGrammar(grammars)
    grammars.find(QualifiedClassNameConstant.key).inner = QualifiedClassNameConstant.getConstantEntryGrammar(grammars)
    grammars.find(MethodRefConstant.key).inner = (grammars.find(ClassInfoConstant.key).as(MethodRefClassName) <~ "." ~
      grammars.find(NameAndTypeConstant.key).as(MethodRefMethodName)) asNode MethodRefKey
    grammars.find(ClassInfoConstant.key).inner = grammars.find(QualifiedClassNameConstant.key).as(ClassRefName) asNode ClassRefKey
    grammars.find(FieldRefConstant.key).inner = grammars.find(ClassInfoConstant.key).as(FieldRefClassIndex) ~ "." ~
      grammars.find(NameAndTypeConstant.key).as(FieldRefNameAndTypeIndex) asNode FieldRefConstant.key
    grammars.find(NameAndTypeConstant.key).inner = grammars.find(Utf8Constant.key).as(NameAndTypeName) ~~
      grammars.find(TypeConstant.key).as(NameAndTypeType) asNode NameAndTypeKey
    grammars.find(QualifiedClassNameConstant.key).inner = QualifiedClassNameConstant.getConstantEntryGrammar(grammars)

    val constantPoolGrammar = grammars.findPathToKey(ConstantPoolGrammar)
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}