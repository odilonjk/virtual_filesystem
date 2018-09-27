package filesystem

import files.Directory

class State(val root: Directory, val workingDirectory: Directory, val output: String) {

  def show: Unit =
    print(State.SHELL_TOKEN)
    println(output)
  def setMessage(message: String): State = State(root, workingDirectory, message)

}

object State {

  val SHELL_TOKEN = "$ "
  def apply(root: Directory, wd: Directory, output: String = "") =
    new State(root, wd, output)

}