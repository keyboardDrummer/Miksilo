package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language, Phase}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.constants.ClassInfoConstant
import miksilo.modularLanguages.deltas.classes.ClassDelta.{JavaClass, Members}
import miksilo.modularLanguages.deltas.javac.classes.{ClassCompiler, FieldToByteCode}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassDelta}
import miksilo.modularLanguages.deltas.javac.methods.{MethodDelta, MethodToByteCode}

object JavaClassToByteCodeDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.insertPhaseAfter(
      Phase(this, description, compilation => transformProgram(compilation.program.asInstanceOf[PathRoot].current, compilation)),
      FullyQualifyTypeReferences)
    super.inject(language)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    transformClass(program)

    def transformClass(program: Node): Unit = {
      val javaClass: JavaClass[NodePath] = PathRoot(program)
      JavaStandardLibraryDelta.loadIntoClassPath(compilation)
      javaClass.node.shape = ByteCodeSkeleton.Shape
      val classFile = new ClassFile(javaClass.node)
      val classCompiler: ClassCompiler = ClassCompiler(javaClass.node, compilation)
      JavaClassDelta.state(compilation).classCompiler = classCompiler
      classCompiler.bind()

      val classInfo = classCompiler.currentClassInfo
      classFile.attributes = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      program(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = javaClass.parent.get
      val parentRef = ClassInfoConstant.classRef(classCompiler.fullyQualify(parentName))
      program(ByteCodeSkeleton.ClassParentIndex) = parentRef
      program(ByteCodeSkeleton.ClassInterfaces) = Seq()

      program(ByteCodeSkeleton.ClassFields) = JavaClassDelta.getFields[NodePath](javaClass).map(field =>
        FieldToByteCode.compile(compilation, field))

      program(ByteCodeSkeleton.Methods) = MethodDelta.getMethods[NodePath](javaClass).map(method =>
        MethodToByteCode.compile(compilation, method))

      javaClass.node.data.remove(Members)
    }
  }

  override def description = "Converts a Java class to JVM bytecode"

  override def dependencies = FieldToByteCode.dependencies ++ MethodToByteCode.dependencies
}
