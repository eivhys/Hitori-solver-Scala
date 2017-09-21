import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object HitoriSolver {

  val noColor = -1
  val black = 0
  val white = 1
  val size = 5

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

  val containsOnlyDistinct = (arr:Array[Int]) => arr.length == arr.distinct.length

  // Checks if an array only contains blacks (0)
  val containsNoBlacksOnly = (arr:Array[Int]) => arr.contains(1)

  // Goes through each row and column (arr) 2 tiles at a time, ensures that a pair never contains blacks only
  val containsNoAdjacent = (arr:Array[Int]) => arr.sliding(2).forall(containsNoBlacksOnly)

  val completePuzzle = (colors:Array[Array[Int]]) => colors.exists(_.contains(-1))

  /**
    * Checks if the puzzle is a valid solution. Requires puzzle as two 2D arrays, one for values and another for blacked
    * out tiles.
    *
    * Ensures that there are:
    * - Unique numbers on each line/columns
    * - No adjacent black tiles
    * - All white tiles are interconnected
    *
    * @param horizontalValues 2D array of puzzle values
    * @param horizontalColors 2D array of puzzle colors
    * @param log Flag for printing debug messages to the console
    * @return True if the puzzle is solved, false otherwise
    */
  def isSolved(horizontalValues:Array[Array[Int]], horizontalColors:Array[Array[Int]], log:Boolean=false):Boolean = {
    if (completePuzzle(horizontalColors)) {
      if (log) println("Incomplete puzzle! Aborting puzzle solved check")
      return false
    }

    val verticalValues = horizontalValues.transpose
    val verticalColors = horizontalColors.transpose

    val filteredHorizontal = for ((vs, cs) <- horizontalValues.zip(horizontalColors)) yield vs.zip(cs).collect{ case(v, 1) => v }
    val filteredVertical = for ((vs, cs) <- verticalValues.zip(verticalColors)) yield vs.zip(cs).collect{ case(v, 1) => v }

    if (log) {
      printArray(horizontalValues, "Horizontal values")
      printArray(horizontalColors, "Horizontal colors")
      printArray(filteredHorizontal, "Horizontal values duplicates filtered")
      printArray(filteredVertical, "Vertical values duplicates filtered")
    }

    // Are all tiles on the same row/column contain only unique numbers unless they are blacked out?
    val unique = filteredHorizontal.forall(containsOnlyDistinct) && filteredVertical.forall(containsOnlyDistinct)

    // Are no black fields adjacent(not diagonally)?
    val adjacent = horizontalColors.forall(containsNoAdjacent) && verticalColors.forall(containsNoAdjacent)

    // Are all white tiles are interconnected?
    val floodFill = floodFillCheck(horizontalColors)

    if (log) {
      println("All tile values unique: " + unique)
      println("No adjacent black tiles: " + adjacent)
      println("All white tiles interconnected: " + floodFill)
    }

    unique && adjacent && floodFill
  }

  /**
    * Recursively check if a puzzle has interconnected white tiles, black tiles are ignored.
    *
    * @param puzzleColors 2D array of tiles containing -1, 0, 1 color mappings
    * @return True if the puzzle has interconnected white tiles, false otherwise
    */
  def floodFillCheck(puzzleColors:Array[Array[Int]]):Boolean = {
    if (completePuzzle(puzzleColors)) {
      println("Incomplete puzzle! Aborting flood fill check")
      return false
    }

    val size = puzzleColors.length
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
              val next =
                if (up > 0 && !checkedTiles(up))
                  up :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 1 => {
              val right = index + 1
              val next =
                if (right % size > index % size &&
                  right < checkedTiles.length &&
                  right < checkedTiles.length &&
                  !checkedTiles(right))
                  right :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 2 => {
              val down = index + size
              val next =
                if (down < checkedTiles.length && !checkedTiles(down))
                  down :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)
            }
            case 3 => {
              val left = index - 1
              val next = if (left % size < index % size &&
                left > 0 &&
                left < checkedTiles.length &&
                !checkedTiles(left))
                left :: tail
              else tail
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

  def printArray(arr:Array[Array[Int]], headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    arr.foreach(a => println(a.mkString(", ")))
  }

  def printBoard(puzzle:Array[Array[Int]], headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    for (line<-puzzle) {
      line.foreach(x => print(x + wOrB(x)))
      println("")
    }
  }

  def printBoardBool(puzzle:Array[Array[Boolean]], headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    for (line<-puzzle) {
      line.foreach(x => print(wOrB(if (x) 1 else 0)))
      println("")
    }
  }

  def applyStartingTechniques (values:Array[Array[Int]]): Array[Array[Int]] = {

    //Gives cells a color based on their position relative to each other

    //TODO Needs more starting techniques

    val newColors = Array.tabulate(size, size)((_,_) => -1)

    if (size > 3) {
      for (x <- 0 until size) {
        for (y <- 0 until size - 3) {
          //Adjacent triples
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

  def functionalStarting(values:Array[Array[Int]]):Array[Array[Int]] = {
    val size = values.length
    var colors = Array.tabulate(size, size)((_,_) => -1)

    val valuesFlipped = values.transpose

    // Adjacent triplets

    colors
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
    //val puzzleColors = Array.tabulate(5, 5)((_,_) => -1)//((x, y) => if (x == 3 && y == 0 || x == 0 && y == 3 || x == 4 && y == 2) 0 else 1)

    val puzzleColors = applyStartingTechniques(puzzle)

    printBoard(puzzle, "Initial puzzle")

    printBoard(puzzleColors, "Puzzle colors")

    //println("Flood fill: " + floodFillCheck(puzzleColors))

    println("Puzzle solved: " + isSolved(puzzle, puzzleColors, true))

    val size = board.length

    val noColor = -1

    val black = 0

    val white = 1


    val startingColors = applyStartingTechniques(board)

    // Run after solution found
    savePuzzle(args(1), puzzle)
  }

}