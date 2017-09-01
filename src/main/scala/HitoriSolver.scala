import java.io.{File, PrintWriter}

object HitoriSolver {

  def loadPuzzle(filePath:String):Array[Array[Int]] = {
    val file = new File(filePath)
    val lines = scala.io.Source.fromFile(file).mkString.split("\n")
    val size = lines.length

    var puzzle = Array.ofDim[Int](size, size)

    for (line<-lines) {
      var row = Array()
      line.mkString.split("\\s").foreach{ x => row :+ x.toInt}
      puzzle :+ row
    }

    puzzle
  }

  def savePuzzle(filePath:String, puzzle:Array[Array[Int]]):Unit = {
    var outputFile = new PrintWriter(new File(filePath))

    for (row<-puzzle) {
      var line = StringBuilder
      row.foreach(x => line += x + " ")
      outputFile.println(line)
    }

    outputFile.close
  }

  def isSolved(puzzleValues:Array[Array[Int]], puzzleColors:Array[Array[Int]]):Boolean = {
    /*
     PuzzleColors
     -1 - nothing
     0 - black
     1 - white
      */

    // Rows and columns only have unique numbers
    // Find all values in a row/column and


    // No adjacent black fields

    //horizontal
    //puzzleColors
    // vertical
    //puzzleColors.

    // All white fields are connected continuously
    // Flood fill all white fields starting from an arbitrary white, when done, check if there any more whites left

    true
  }

  def printBoard(puzzle:Array[Array[Int]]):Unit = {

  }
  
  def main(args: Array[String]): Unit = {

    /*
    * TODO Get board from file
    * TODO Apply starting techniques
    * TODO Check if solved
    * TODO Continously apply techniques
    * TODO Actually solve it (recursion with backtracking)
    * TODO Save solution
    * */

    val board = List(List(2))(2)

    val puzzle = loadPuzzle(args(0))

    val size = board.length

    /* Solve function
    *
    * */

    // Run after solution found
    savePuzzle(args(1), puzzle)
  }

}