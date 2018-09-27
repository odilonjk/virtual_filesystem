package commands

import filesystem.State

trait Command {

  def apply(state: State): State

}

object Command {

  val MKDIR = "mkdir"

  val EMPTY_SPACE = " "

  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state
  }

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State =
      state.setMessage(name + ": incomplete command!")
  }

  def from(input: String): Command = {
    val tokens = input.split(EMPTY_SPACE)
    if (input.isEmpty || tokens.isEmpty) emptyCommand
    else if (MKDIR.equals(tokens(0))) {
      if (tokens.length < 2) incompleteCommand(MKDIR)
      else new Mkdir(tokens(1))
    }
    else new UnknownCommand
  }

}
