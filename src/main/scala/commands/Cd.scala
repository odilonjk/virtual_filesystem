package commands
import files.{DirEntry, Directory}
import filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    val root = state.root
    val workingDirectory = state.workingDirectory
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (workingDirectory.isRoot) workingDirectory.path + dir
      else workingDirectory.path + Directory.SEPARATOR + dir
    val destinationDir = findEntry(root, absolutePath)
    if (destinationDir == null || !destinationDir.isDirectory)
      state.setMessage(dir + ": no such directory")
    else
      State(root, destinationDir.asDirectory)
  }

  def findEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(directory: Directory, path: List[String]):DirEntry = {
      if (path.isEmpty || path.head.isEmpty) directory
      else if (path.tail.isEmpty) directory.findEntry(path.head)
      else {
        val nextDir = directory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }
    @tailrec
    def  collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      }
      else collapseRelativeTokens(path.tail, result :+ path.head)
    }
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val newTokens = collapseRelativeTokens(tokens, List())
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }

}
