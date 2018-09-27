package commands
import files.Directory
import filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name))
      state.setMessage(s"Entry $name already exists!")
    else if (name.contains(Directory.SEPARATOR))
      state.setMessage(name + " must not contain separators!")
    else if (checkIllegal(name))
      state.setMessage(name + ": illegal entry name!")
    else
      doMkdir(name, state)
  }

  def doMkdir(name: String, state: State): State = ???

  def checkIllegal(name: String): Boolean = name.contains(".")

}
