package commands
import files.Directory
import filesystem.State

class Rm(name: String) extends Command {


  def doRm(path: String, state: State): State = {
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (!nextDir.isDirectory) currentDirectory
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if (newNextDir == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head, newNextDir)
        }
      }
    }
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)
    if (newRoot == state.root)
      state.setMessage(path + ": no such file or directory.")
    else
      State(newRoot, newRoot.findDescendant(state.workingDirectory.path.substring(1)))
  }

  override def apply(state: State): State = {
    val workingDirectory = state.workingDirectory
    val absolutePath =
      if (name.startsWith(Directory.ROOT_PATH)) name
      else if (workingDirectory.isRoot) workingDirectory.path + name
      else workingDirectory + Directory.SEPARATOR + name
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("You cannot remove root directory.")
    else
      doRm(absolutePath, state)
  }

}
