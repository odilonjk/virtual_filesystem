package commands
import filesystem.State

class Cat(fileName: String) extends Command {

  override def apply(state: State): State = {
    val workingDirectory = state.workingDirectory
    val dirEntry = workingDirectory.findEntry(fileName)
    if (dirEntry == null || !dirEntry.isFile)
      state.setMessage(fileName + ": no such file")
    else
      state.setMessage(dirEntry.asFile.content)
  }

}
