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

  def findEntry(root: Directory, absolutePath: String): DirEntry = {
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
    val tokens: List[String] = absolutePath.substring(1).split(Directory.SEPARATOR).toList
    findEntryHelper(root, tokens)
  }

}
