package commands
import files.{DirEntry, File}
import filesystem.State

class Touch(val name: String) extends CreateEntry(name) {

  override def createSpecificEntry(state: State): DirEntry =
    File.empty(state.workingDirectory.parentPath, name)

}
