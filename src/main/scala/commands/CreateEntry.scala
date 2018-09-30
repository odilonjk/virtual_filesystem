package commands

import files.{DirEntry, Directory}
import filesystem.State

abstract class CreateEntry(val entryName: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.workingDirectory
    if (wd.hasEntry(entryName))
      state.setMessage(s"Entry $entryName already exists!")
    else if (entryName.contains(Directory.SEPARATOR))
      state.setMessage(entryName + " must not contain separators!")
    else if (checkIllegal(entryName))
      state.setMessage(entryName + ": illegal entry name!")
    else
      doCreateEntry(entryName, state)
  }

  def doCreateEntry(name: String, state: State): State = {
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
    val newEntry: DirEntry = createSpecificEntry(state)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    val newWorkingDirectory = newRoot.findDescendant(allDirsInPath)
    State(newRoot, newWorkingDirectory)
  }

  def createSpecificEntry(state: State): DirEntry

  def checkIllegal(name: String): Boolean = name.contains(".")

}
