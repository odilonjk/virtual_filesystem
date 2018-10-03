package commands

import filesystem.State

trait Command extends (State => State) {
}

object Command {

  val CAT = "cat"

  val CD = "cd"

  val ECHO = "echo"

  val EMPTY_SPACE = " "

  val LS = "ls"

  val MKDIR = "mkdir"

  val PWD = "pwd"

  val TOUCH = "touch"

  val RM = "rm"

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
    else tokens(0) match {
      case MKDIR =>
        if (tokens.length < 2) incompleteCommand(MKDIR)
        else new Mkdir(tokens(1))
      case LS =>
        new Ls
      case PWD =>
        new Pwd
      case TOUCH =>
        new Touch(tokens(1))
      case CD =>
        if (tokens.length < 2) incompleteCommand(CD)
        else new Cd(tokens(1))
      case RM =>
        if (tokens.length < 2) incompleteCommand(RM)
        else new Rm(tokens(1))
      case ECHO =>
        if (tokens.length < 2) incompleteCommand(ECHO)
        else new Echo(tokens.tail)
      case CAT =>
        if (tokens.length < 2) incompleteCommand(CAT)
        else new Cat(tokens(1))
      case _ =>
        new UnknownCommand
    }
  }

}
