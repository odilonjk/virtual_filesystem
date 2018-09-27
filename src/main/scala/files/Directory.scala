package files

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def hasEntry(name: String): Boolean = ???

  def getAllDirsInPath: List[String] =
    path.substring(1).split(Directory.SEPARATOR).toList

  def findDescendant(path: List[String]): Directory = ???

  def addEntry(newEntry: DirEntry): Directory = ???

  def findEntry(head: String): DirEntry = ???

  def replaceEntry(name: String, directory: Directory): Directory = ???

  def asDirectory: Directory = this

}

object Directory {

  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String) =
    new Directory(parentPath, name, List())

}
