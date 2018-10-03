package filesystem

import java.util.Scanner

import commands.Command
import files.Directory

object Filesystem extends App {

  val root = Directory.ROOT
  io.Source.stdin.getLines().foldLeft(State(root, root))((currentState, newLine) => {
    currentState.show
    Command.from(newLine).apply(currentState)
  })

}
