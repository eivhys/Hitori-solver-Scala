import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection.mutable.ArrayBuffer

object HitoriSolver {

  val noColor = -1

  val black = 0

  val white = 1

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
    val outputFile = new PrintWriter(new File(filePath))

    for (row<-puzzle) {
      var line = new StringBuilder()
      row.foreach{x => line ++= wOrB(x)}
      outputFile.println(line)
    }

    outputFile.close
  }

  /*
  def getColumn(puzzle:Array[Array[Int]], i:Int = -1):Array[Int] = {
    var rows = new ArrayBuffer[Array[Int]]()

    puzzle.foreach(arr => rows += arr)

    rows.toArray
  }*/

  def uniqueNumbers(board:Array[Array[Int]], colors:Array[Array[Int]]): Boolean = {
    for (x <- 0 until board.length) {
      for (y <- 0 until board.length) {
        val e = board(x)(y)
        for (i <- 0 until board.length) {
          val e2 = board(i)(y)
          val e3 = board(x)(i)
          if (e == e2 && e == white && e2 == white) {
            return false
          }
          if (e == e3 && e == white && e3 == white) {
            return false
          }
        }
      }
    }
    true
  }

  def isSolved(puzzleValues:Array[Array[Int]], puzzleColors:Array[Array[Int]]):Boolean = {
    /*
     PuzzleColors
     -1 - nothing
     0 - black
     1 - white
      */

    val horizontalValues = for (row <- puzzleValues) yield row
    val horizontalColors = for (row <- puzzleColors) yield row
    val verticalValues = horizontalValues.transpose
    val verticalColors = horizontalColors.transpose


    // Rows and columns only have unique numbers
    // Find all values in a row/column and

    //val unique = horizontalValues.forall(l => l.distinct)




    // No adjacent black fields
    val adjacentHorizontal = horizontalColors.sliding(2).forall(pair => pair.length != pair.distinct.length)
    val adjacentVertical = verticalColors.sliding(2).forall(pair => pair.length != pair.distinct.length)



    adjacentHorizontal && adjacentVertical && floodFillCheck(puzzleColors)
  }

  val containsOnlyDistinct = (arr:Array[Any]) => arr.length == arr.distinct.length

  /**
    * Recursively checks if a puzzle has interconnected white tiles, black tiles are ignored.
    *
    * @param puzzleColors 2D array of tiles containing -1, 0, 1 color mappings
    * @return True if the puzzle has interconnected white tiles, false otherwise
    */
  def floodFillCheck(puzzleColors:Array[Array[Int]]):Boolean = {
    val size = puzzleColors.length

    // Puzzle is not solved yet
    if (puzzleColors.contains(-1))
      return false

    val checked = for (tile <- puzzleColors.flatten) yield tile == 0

    //println("Pre flood fill checked: " + checked.length)
    //println("Pre flood fill whites: " + checked.count(x => !x))
    //println("Pre flood fill blacks: " + checked.count(x => x))

    // Find the first white tile
    val tile = checked.indexWhere(x => !x)
    val first = if (tile != -1) tile :: List[Int]() else List[Int]()

    @tailrec
    def floodFill(checkedTiles:Array[Boolean], next:List[Int], dir:Int): Array[Boolean] = {
      next match {
        case Nil => checkedTiles
        case head :: tail => {
          val index = head

          checkedTiles(index) = true

          //println("Checked: " + index + " | isChecked: " + checkedTiles(index) + " | dir: " + dir)

          dir match {
            case 0 => {
              val up = index - size
              val next = if (up > 0 && !checkedTiles(up)) up :: tail else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 1 => {
              val right = index + 1
              val next = if (right % size > index % size && right < checkedTiles.length && right < checkedTiles.length && !checkedTiles(right)) right :: tail else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 2 => {
              val down = index + size
              val next = if (down < checkedTiles.length && !checkedTiles(down)) down :: tail else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 3 => {
              val left = index - 1
              val next = if (left % size < index % size && left > 0 && left < checkedTiles.length && !checkedTiles(left)) left :: tail else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case _ => {
              floodFill(checkedTiles, tail, 0)
            }
          }
        }
      }
    }

    !floodFill(checked, first, 0).contains(false)
  }

  // val performs better than def when the method is called multiple times
  val xIndex = (i:Int, size:Int) => i % size
  val yIndex = (i:Int, size:Int) => i / size    // Works because of integer division, alternative: (i - (i % size)) / size
  val index = (x:Int, y:Int, size:Int) => x + (y * size)

  val wOrB = (x:Int) => (if (x == 0) "b " else "w ");

  def printBoard(puzzle:Array[Array[Int]], headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    for (line<-puzzle){
      line.foreach(x => print(x + wOrB(x)))
      println("")
    }
  }

  def printBoardBool(puzzle:Array[Array[Boolean]], headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    for (line<-puzzle){
      line.foreach(x => print(wOrB(if (x) 1 else 0)))
      println("")
    }
  }

  def main(args: Array[String]): Unit = {

    /*
    * TODO Get board from file ✓
    * TODO Apply starting techniques ✓
    * TODO Check if solved ✓
    * TODO Continuously apply techniques
    * TODO Actually solve it (recursion with backtracking)
    * TODO Save solution ✓
    * */

    val board = Array.ofDim[Int](5, 5)

    val puzzle = loadPuzzle(args(0))
    val puzzleColors = Array.tabulate(5, 5)((x, y) => if (x == 3 && y == 0 || x == 0 && y == 4 || x == 4 && y == 1) 0 else 1)

    printBoard(puzzle, "Initial puzzle")

    printBoard(puzzleColors, "Puzzle colors")

    println("Flood fill: " + floodFillCheck(puzzleColors))

    println("Puzzle solved: " + isSolved(puzzle, puzzleColors))

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

          //White around all blacks (all whites needs a friend)
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
        //Corner rule
        val end = size - 1
        //Top left
        if (values(0)(0) == values(1)(0) && values(0)(0) == values(0)(1)) {
          newColors(0)(0) = black
        }
        //Top right
        if (values(end)(0) == values(end)(1) && values(end)(0) == values(end - 1)(0)) {
          newColors(end)(0) = black
        }
        //Bottom left
        if (values(0)(end) == values(1)(end) && values(0)(end) == values(0)(end - 1)) {
          newColors(0)(end) = black
        }
        //Bottom right
        if (values(end)(end) == values(end)(end - 1) && values(end)(end) == values(end - 1)(end)) {
          newColors(end)(end) = black
        }
      }
      newColors
    }

    val startingColors = applyStartingTechniques(board)

    // Run after solution found
    savePuzzle(args(1), puzzle)
  }

}