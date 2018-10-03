package files

import filesystem.FilesystemException

class File(override val parentPath: String, override val name: String, val content: String) extends DirEntry(parentPath, name) {

  def asDirectory: Directory =
    throw new FilesystemException("A file cannot be converted to a directory.")

  def getType: String = "File"

  def asFile: File = this

  def isFile: Boolean = true

  def isDirectory: Boolean = false

  def setContent(newContent: String): File =
    new File(parentPath, name, newContent)

  def appendContent(newContent: String): File =
    setContent(content + "\n" + newContent)

}

object File {

  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")



}
