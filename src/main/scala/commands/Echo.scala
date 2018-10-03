package commands
import files.Directory
import files.File
import filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {

  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if (currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex + 1, accumulator + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }

  def getRootAfterEcho(currentDir: Directory, path: List[String], content: String, append: Boolean): Directory = {
    if (path.isEmpty) currentDir
    else if(path.tail.isEmpty) {
      val dirEntry = currentDir.findEntry(path.head)
      if (dirEntry == null) currentDir.addEntry(new File(currentDir.path, path.head, content))
      else if(dirEntry.isDirectory) currentDir
      else
        if(append) currentDir.replaceEntry(path.head, dirEntry.asFile.appendContent(content))
        else currentDir.replaceEntry(path.head, dirEntry.asFile.setContent(content))
    }
    else {
      val nextDir = currentDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, content, append)
      if (newNextDir == newNextDir) currentDir
      else currentDir.replaceEntry(path.head, newNextDir)
    }
  }

  def doEcho(state: State, content: String, fileName: String, append: Boolean) = {
    if (fileName.contains(Directory.SEPARATOR))
      state.setMessage("Echo: file name must not contain separators.")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.workingDirectory.getAllDirsInPath :+ fileName, content, append)
      if (newRoot == state.root)
        state.setMessage(fileName + ": no such file")
      else
        State(newRoot, newRoot.findDescendant(state.workingDirectory.getAllDirsInPath))
    }
  }

  override def apply(state: State): State = {
    if(args.isEmpty) state
    else if(args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val fileName = args(args.length - 1)
      val content = createContent(args, args.length - 2)
      if (">>".equals(operator)) doEcho(state, content, fileName, append = true)
      else if (">".equals(operator)) doEcho(state, content, fileName, append = false)
      else state.setMessage(createContent(args, args.length))
    }
  }

}
