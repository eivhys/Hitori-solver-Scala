import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer

object HitoriSolver {

  def loadPuzzle(filePath:String):Array[Array[Int]] = {
    val file = new File(filePath)
    val lines = scala.io.Source.fromFile(file).mkString.split("\n")
    val size = lines.length

    var puzzle = new ArrayBuffer[Array[Int]]()

    for (line<-lines) {
      var row = new ArrayBuffer[Int]()
      line.mkString.split("\\s").foreach{ x => row += x.toInt}
      puzzle += row.toArray
    }

    puzzle.toArray
  }

  def savePuzzle(filePath:String, puzzle:Array[Array[Int]]):Unit = {
    var outputFile = new PrintWriter(new File(filePath))

    for (row<-puzzle) {
      var line = new StringBuilder()
      row.foreach{x => line ++= wOrB(x)}
      outputFile.println(line)
    }

    outputFile.close
  }

  def getColumn(puzzle:Array[Array[Int]], i:Int = -1):Array[Int] = {
    var rows = new ArrayBuffer[Array[Int]]()

    puzzle.foreach(arr => rows += arr)

    rows.toArray
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

    // Rows

    // Columns


    // No adjacent black fields

    //horizontal
    //puzzleColors
    // vertical
    //puzzleColors.

    // All white fields are connected continuously
    // Flood fill all white fields starting from an arbitrary white, when done, check if there any more whites left

    true
  }

  def floodFill(puzzle:Array[Array[Int]]):Array[Array[Int]] = {
    // Select arbitrary starting point
    // "Fill" current tile by popping of stack
    // Add top, bottom, left, right tiles (only white) to stack for filling next
    // Pop next and repeat
    // Finally, compare filled tiles to puzzle tile count - blacks
  }

  val wOrB = (x:Int) => (if (x==0) "b " else "w ");

  def printBoard(puzzle:Array[Array[Int]]):Unit = {
    for (line<-puzzle){
      line.foreach(x => print(x + wOrB(x)))
      println("")
    }
  }

  def main(args: Array[String]): Unit = {

    /*
    * TODO Get board from file ✓
    * TODO Apply starting techniques ✓
    * TODO Check if solved
    * TODO Continously apply techniques
    * TODO Actually solve it (recursion with backtracking)
    * TODO Save solution ✓
    * */

    val board = Array.ofDim[Int](5, 5)

    val puzzle = loadPuzzle(args(0))

    printBoard(puzzle)

    val size = board.length

    val noColor = -1

    val black = 0

    val white = 1

    def applyStartingTechniques (values:Array[Array[Int]]): Array[Array[Int]] = {

      //Gives cells a color based on their position relative to each other

      //TODO Needs more starting techniques

      val newColors = Array.ofDim[Int](size, size)

      for (x <- 0 until size) {
        for (y <- 0 until size) {
          newColors(x)(y) = noColor
        }
      }

      if (size > 3) {
        for (x <- 0 until size) {
          for (y <- 0 until size - 3) {
            //Adjecent triples
            //Vertical
            if (values(x)(y) == values(x)(y + 1) && values(x)(y) == values(x)(y + 2)) {
              newColors(x)(y) = black
              newColors(x)(y + 1) = white
              newColors(x)(y + 2) = black
            }
            //Horizontal
            if (values(y)(x) == values(y + 1)(x) && values(y)(x) == values(y + 2)(x)) {
              newColors(y)(x) = black
              newColors(y + 1)(x) = white
              newColors(y + 2)(x) = black
            }

            //Square between pair
            //Vertical
            if (values(x)(y) == values(x)(y + 2)) {
              newColors(x)(y + 1) = white
            }
            //Horizontal
            if (values(y)(x) == values(y + 2)(x)) {
              newColors(y + 1)(x) = white
            }
          }
        }

        if (size > 4) {

          //Pair induction
          for (x <- 0 until size) {
            for (y <- 0 until size - 4) {
              if (values(x)(y) == values(x)(y + 2) && values(x)(y) == values(x)(y + 3)) {
                newColors(x)(y) = black
              }
              if (values(x)(y) == values(x)(y + 1) && values(x)(y) == values(x)(y + 3)) {
                newColors(x)(y + 2) = black
              }
              if (values(y)(x) == values(y + 2)(x) && values(y)(x) == values(y + 3)(x)) {
                newColors(y)(x) = black
              }
              if (values(y)(x) == values(y + 1)(x) && values(y)(x) == values(y + 3)(x)) {
                newColors(y + 2)(x) = black
              }
            }
          }

          //White around all blacks
          for (x <- 0 until size) {
            for (y <- 0 until size) {
              if (newColors(x)(y) == black) {
                if (x >= 0 && x < size - 1) {
                  newColors(x + 1)(y) = white
                }
                if (x > 0 && x < size) {
                  newColors(x - 1)(y) = white
                }
                if (y >= 0 && y < size - 1) {
                  newColors(x)(y + 1) = white
                }
                if (y > 0 && y < size) {
                  newColors(x)(y - 1) = white
                }
              }
            }
          }
        }
      }
      newColors
    }

    val startingColors = applyStartingTechniques(board)

    // Run after solution found
    savePuzzle(args(1), puzzle)
  }

}