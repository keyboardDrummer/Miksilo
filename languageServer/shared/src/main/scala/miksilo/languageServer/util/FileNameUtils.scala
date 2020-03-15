package miksilo.languageServer.util

object FileNameUtils {
  private val EXTENSION_SEPARATOR = '.'
  private val DIRECTORY_SEPARATOR = '/'

  def removeExtension(filename: String): String = {
    if (filename == null) return null
    val index = indexOfExtension(filename)
    if (index == -1) filename
    else filename.substring(0, index)
  }

  /**
    * Return the file extension from a filename, including the "."
    * <p/>
    * e.g. /path/to/myfile.jpg -> .jpg
    */
  def getExtension(filename: String): String = {
    if (filename == null) return null
    val index = indexOfExtension(filename)
    if (index == -1) filename
    else filename.substring(index)
  }

  def indexOfExtension(filename: String): Int = {
    if (filename == null) return -1
    // Check that no directory separator appears after the
    // EXTENSION_SEPARATOR
    val extensionPos = filename.lastIndexOf(EXTENSION_SEPARATOR)
    val lastDirSeparator = filename.lastIndexOf(DIRECTORY_SEPARATOR)
    if (lastDirSeparator > extensionPos) return -1
    extensionPos
  }
}