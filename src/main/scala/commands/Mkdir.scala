package commands
import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.workingDirectory
    if (wd.hasEntry(name))
      state.setMessage(s"Entry $name already exists!")
    else if (name.contains(Directory.SEPARATOR))
      state.setMessage(name + " must not contain separators!")
    else if (checkIllegal(name))
      state.setMessage(name + ": illegal entry name!")
    else
      doMkdir(name, state)
  }

  def doMkdir(name: String, state: State): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty)
        currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }
    val workingDirectory = state.workingDirectory
    val allDirsInPath = workingDirectory.getAllDirsInPath
    val newDir = Directory.empty(workingDirectory.path, name)
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)
    val newWorkingDirectory = newRoot.findDescendant(allDirsInPath)
    State(newRoot, newWorkingDirectory)
  }

  def checkIllegal(name: String): Boolean = name.contains(".")

}
