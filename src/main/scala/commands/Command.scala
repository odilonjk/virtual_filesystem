package commands

import filesystem.State

trait Command {

  def apply(state: State): State

}

object Command {

  val CD = "cd"

  val EMPTY_SPACE = " "

  val LS = "ls"

  val MKDIR = "mkdir"

  val PWD = "pwd"

  val TOUCH = "touch"

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
    else if (LS.equals(tokens(0))) {
      new Ls
    }
    else if (PWD.equals(tokens(0))) {
      new Pwd
    }
    else if (TOUCH.equals(tokens(0))) {
      new Touch(tokens(1))
    }
    else if (CD.equals(tokens(0))) {
      if (tokens.length < 2) incompleteCommand(MKDIR)
      else new Cd(tokens(1))
    }
    else new UnknownCommand
  }

}
