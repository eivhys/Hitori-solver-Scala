import java.io.{File, PrintWriter}
import java.util.concurrent.Executors

import HitoriSolver.{Puzzle, isSolved}

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Random, Success}


object HitoriSolver {
  type Board = Array[Array[Int]]
  case class Puzzle(values:Board, colors:Board)

  val noColor:Int = -1
  val black:Int = 0
  val white:Int = 1

  /**
    * Loads a puzzle board from the desired location
    *
    * @param filePath Path to file
    * @return Board containg the values of the puzzle
    */
  def loadPuzzle(filePath:String):Board = {
    val file = new File(filePath)
    val lines = scala.io.Source.fromFile(file).mkString.split("\n")

    val puzzle = for {
      line <- lines
    } yield line.mkString.split("\\s+").map(_.toInt)

    puzzle
  }

  /**
    * Saves the puzzle to a .txt file at the specified path. The tile's color, white or black, are represented by
    * w and b respectively. Resulting file contains the n x n board with w or b's separated by spaces.
    *
    * E.G:
    * w b w b w
    * w w w w w
    * b w b w w
    * w b w b w
    * w w w w w
    *
    * @param filePath
    * @param puzzle
    */
  def savePuzzle(filePath:String, puzzle:Board):Unit = {
    val outputFile = new PrintWriter(new File(filePath))

    puzzle.foreach {
      x => outputFile.println(x.map(wOrB).mkString(" "))
    }

    outputFile.close()
  }

  /**
    * Prints the execution time in milliseconds of a code block to the console.
    *
    * @param block Code block which is to be executed
    * @tparam T Result type
    * @return Block result
    */
  def timedFunc[T](block: => T):T = {
    val start = System.currentTimeMillis()
    val res = block
    print("Time elapsed: " + (System.currentTimeMillis() - start) + " ms\n")
    res
  }

  /** Checks if an int array only contains unique elements */
  val containsOnlyDistinct = (arr:Array[Int]) => arr.length == arr.distinct.length

  /** Checks if an array of size 2 only contains blacks (0) */
  val containsNoBlacksOnly = (arr:Array[Int]) => arr.count(_ == 0) != 2

  /** Goes through an array 2 elements at a time, ensures that a pair never contains blacks only */
  val containsNoAdjacent = (arr:Array[Int]) => arr.sliding(2).forall(containsNoBlacksOnly)

  /** Checks if a puzzle has no non-colored tiles */
  val incompletePuzzle = (puzzle:Puzzle) => puzzle.colors.exists(_.contains(-1))

  val xIndex = (i:Int, size:Int) => i % size
  val yIndex = (i:Int, size:Int) => i / size    //Alternative: (i - (i % size)) / size
  val index = (x:Int, y:Int, size:Int) => x + (y * size)

  /** Translate int color to string */
  val wOrB = (x:Int) => if (x == 0) "b" else "w"

  /**
    * Checks if the puzzle is a valid solution. Requires puzzle as two 2D arrays, one for values and another for blacked
    * out tiles.
    *
    * Ensures that there are:
    * - Unique numbers on each line/columns
    * - No adjacent black tiles
    * - All white tiles are interconnected
    *
    * @param puzzle 2D array of puzzle values and colors
    * @param log Flag for printing debug messages to the console
    * @return True if the puzzle is solved, false otherwise
    */
  def isSolved(puzzle:Puzzle, log:Boolean=false):Boolean = {
    if (incompletePuzzle(puzzle)) {
      if (log) println("Incomplete puzzle! Aborting puzzle solved check")
      return false
    }

    validMoves(puzzle, log)
  }

  /**
    * Checks if the puzzle is a valid. Puzzle does not have to be complete to check. Requires puzzle as two 2D arrays,
    * one for values and another for blacked out tiles.
    *
    * Ensures that there are:
    * - Unique numbers on each line/columns
    * - No adjacent black tiles
    * - All white tiles are interconnected
    *
    * @param puzzle 2D array of puzzle values and colors
    * @param log Flag for printing debug messages to the console
    * @return True if the puzzle is valid, false otherwise
    */
  def validMoves(puzzle:Puzzle, log:Boolean=false):Boolean = {
    val horizontalValues = puzzle.values
    val horizontalColors = puzzle.colors
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
    val floodFill = floodFillCheck(puzzle)

    if (log) {
      println("All tile values unique: " + unique)
      println("No adjacent black tiles: " + adjacent)
      println("All white tiles interconnected: " + floodFill)
    }

    unique && adjacent && floodFill
  }

  /**
    * Recursively check if a puzzle has interconnected white tiles, black tiles are ignored, -1 tiles acts as white.
    * Every tile must be checked in order to succeed. Black tiles start out as checked, while white and noColor tiles
    * does not. Each
    *
    * @param puzzle 2D array of tiles containing -1, 0, 1, describing tile color
    * @return True if the puzzle has interconnected white tiles, false otherwise
    */
  def floodFillCheck(puzzle:Puzzle, log:Boolean=false):Boolean = {
    val size = puzzle.colors.length
    val checked = for (tile <- puzzle.colors.flatten) yield tile == 0

    if (log) {
      println("Pre flood fill checked: " + checked.length)
      println("Pre flood fill whites: " + checked.count(x => !x))
      println("Pre flood fill blacks: " + checked.count(x => x))
    }

    // Find the first non-black tile
    val tile = checked.indexWhere(!_)
    val first = if (tile != -1) tile :: List[Int]() else List[Int]()

    @tailrec
    def floodFill(checkedTiles:Array[Boolean], next:List[Int], dir:Int):Array[Boolean] = {
      next match {
        case Nil => checkedTiles  // Next tile stack is empty, return result
        case head :: tail =>      // Pop head of next stack
          val index = head

          checkedTiles(index) = true

          if (log) println("Checked: " + index + " | isChecked: " + checkedTiles(index) + " | dir: " + dir)

          dir match {
            case 0 =>
              val up = index - size
              val next =
                if (up > 0 && !checkedTiles(up))
                  up :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)

            case 1 =>
              val right = index + 1
              val next =
                if (right % size > index % size
                  && right > 0
                  && right < checkedTiles.length
                  && !checkedTiles(right))
                  right :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)

            case 2 =>
              val down = index + size
              val next =
                if (down < checkedTiles.length && !checkedTiles(down))
                  down :: tail
                else tail
              floodFill(checkedTiles, index :: next, dir + 1)

            case 3 =>
              val left = index - 1
              val next = if (left % size < index % size
                && left > 0
                && left < checkedTiles.length
                && !checkedTiles(left))
                left :: tail
              else tail
              floodFill(checkedTiles, index :: next, dir + 1)

            case _ =>
              floodFill(checkedTiles, tail, 0)
          }
      }
    }

    !floodFill(checked, first, 0).contains(false)
  }

  /**
    * Solves a puzzle using recursion.
    *
    * @param values Puzzle values
    * @param log Flag for printing debug messages to the console
    * @return Solved puzzle
    */
  def solve(values:Board, log:Boolean=false):Puzzle = {
    val puzzle = Puzzle(values, applyStartingTechniques(values))

    def recSolve(location:Int = 0): Boolean = {
      val x = location / puzzle.colors.length
      val y = location % puzzle.colors.length
      if (location >= puzzle.colors.flatten.length) {
        true
      } else {
        puzzle.colors(x)(y) = white
        if (validMoves(puzzle) && recSolve(location + 1)) {
          if (log) println("White at" + x + ":" + y)
          true
        } else {
          puzzle.colors(x)(y) = black
          if (validMoves(puzzle) && recSolve(location + 1)) {
            if (log) println("Black at" + x + ":" + y)
            true
          } else {
            puzzle.colors(x)(y) = noColor
            if (log) println("NoColor at" + x + ":" + y)
            false
          }
        }
      }
    }

    recSolve()

    puzzle
  }

  def applyStartingTechniques(values:Board):Board = {
    val size = values.length
    val newColors = Array.tabulate(size, size)((_,_) => noColor)

    if (size > 3) {
      for (x <- 0 until size) {
        for (y <- 0 until size - 2) {
          //Adjacent triples
          if (values(y)(x) == values(y + 1)(x) && values(y)(x) == values(y + 2)(x)) {
            newColors(y)(x) = black
            newColors(y + 1)(x) = white
            newColors(y + 2)(x) = black
          }

          //Vertical
          if (values(x)(y) == values(x)(y + 1) && values(x)(y) == values(x)(y + 2)) {
            newColors(x)(y) = black
            newColors(x)(y + 1) = white
            newColors(x)(y + 2) = black
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
          for (y <- 0 until size - 3) {
            if (values(x)(y) == values(x)(y + 2) && values(x)(y) == values(x)(y + 3)) {
              newColors(x)(y) = black
            }
            if (values(x)(y) == values(x)(y + 1) && values(x)(y) == values(x)(y + 3)) {
              newColors(x)(y + 3) = black
            }
            if (values(y)(x) == values(y + 2)(x) && values(y)(x) == values(y + 3)(x)) {
              newColors(y)(x) = black
            }
            if (values(y)(x) == values(y + 1)(x) && values(y)(x) == values(y + 3)(x)) {
              newColors(y + 3)(x) = black
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

  def printArray(arr:Board, headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    arr.foreach(a => println(a.mkString(" ")))
  }

  def printBoard(puzzle:Board, headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    puzzle.foreach {
      x => println(x.map(wOrB).mkString(""))
    }
  }

  def printPuzzle(puzzle:Puzzle, headline:String=""):Unit = {
    if (!headline.isEmpty)
      println("-- " + headline + " --")

    (puzzle.values, puzzle.colors).zipped.foreach{
      (vs, cs) => (vs, cs).zipped.foreach{
        (v, c) => print(v + wOrB(c) + " ")
      }
        println()
    }
    println()
  }


  def main(args: Array[String]): Unit = {
    val puzzleValues = loadPuzzle(args(0))

    printArray(puzzleValues, "Initial puzzle")

    val solved = timedFunc { solve(puzzleValues) }

    printPuzzle(solved, "Solved puzzle: " + isSolved(solved))

    savePuzzle(args(1), solved.colors)
  }
}

/*
def solve(values:Board, log:Boolean=false):Puzzle = {
  val startColors = applyStartingTechniques(values)
  var result = Puzzle(values, applyRunningTechniques(values, startColors))
  val size = values.length

  if (log) println("Valid starting techniques: " + validMoves(result))

  def findSolution(puzzle:Puzzle):Boolean = {
    if (isSolved(puzzle))
      return true

    if (!validMoves(puzzle))
      return false

    /*
    var prevColors = puzzle.colors
    var colors = applyRunningTechniques(puzzle.values, prevColors)

    // Apply running techniques until there are no new changes
    while (!prevColors.sameElements(colors)) {
      prevColors = colors
      colors = applyRunningTechniques(puzzle.values, prevColors)
    }

    var colors1 = puzzle.colors
    var colors2 = applyRunningTechniques(puzzle.values, colors1)
    var runningDone = false
    while (!runningDone) {
      colors1 = applyRunningTechniques(puzzle.values, colors2)
      colors2 = applyRunningTechniques(puzzle.values, colors1)
      if (colors1.sameElements(colors2)) runningDone = true
    }

    val colors = colors2
    */

    val colors = puzzle.colors

    if (log) printPuzzle(puzzle, "Intermediate solved puzzle")

    val size = puzzle.values.length
    val index = colors.flatten.indexWhere(_ == -1)

    if (index == -1) {
      return false
    }

    colors(yIndex(index, size))(xIndex(index, size)) = 0

    if (!findSolution(Puzzle(puzzle.values, colors)))
      colors(yIndex(index, size))(xIndex(index, size)) = 1

    findSolution(Puzzle(puzzle.values, colors))
  }

  if (!findSolution(result)) {
    val threadPoolCount = 128

    implicit val context = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadPoolCount))

    val f = Future {
      optimisticSolver(result)
    }

    f.onComplete {
      case Success(p) => result = p
      case Failure(error) => println("ThreadPool optimistic solver failed: " + error)
    }

    Await.result(f, Duration.Inf)
  }

  result
}


// Last hope, may the force be with you
def optimisticSolver(puzzle:Puzzle):Puzzle = {
  val r = Random

  while (!isSolved(puzzle)) {
    puzzle.colors.foreach(_.transform(_ => r.nextInt(2)))
  }

  puzzle
}

def applyRunningTechniques (values:Array[Array[Int]], colors:Array[Array[Int]], log:Boolean=false): Array[Array[Int]] = {
  val size = values.length

  def wAroundB(values:Array[Array[Int]], colors:Array[Array[Int]]): Array[Array[Int]] = {
    //White around all blacks (all whites needs a friend)
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        if (colors(x)(y) == black) {
          if (x >= 0 && x < size - 1) {
            colors(x + 1)(y) = white
          }
          if (x > 0 && x < size) {
            colors(x - 1)(y) = white
          }
          if (y >= 0 && y < size - 1) {
            colors(x)(y + 1) = white
          }
          if (y > 0 && y < size) {
            colors(x)(y - 1) = white
          }
        }
      }
    }
    colors
  }

  def sameValueDiffColor(values:Array[Array[Int]], colors:Array[Array[Int]]): Array[Array[Int]] = {
    // 2 equal neighbours with different colors
    for (number <- 1 until size + 1) {
      var numlist = ListBuffer[Tuple2[Int,Int]]()
      for (x <- 0 until size) {
        numlist.clear()
        for (y <- 0 until size) {
          if (values(x)(y) == number) {
            numlist += (Tuple2(x, y))
          }
          if (numlist.length > 2) {
            for (e <- 0 until numlist.length) {
              if (colors(numlist(e)._1)(numlist(e)._2) == white) {
                for (i <- 0 until numlist.length) {
                  if (i != e) {
                    var check = colors
                    check(numlist(i)._1)(numlist(i)._2) = black
                    if (validMoves(Puzzle(values, check))) {
                      colors(numlist(i)._1)(numlist(i)._2) = black
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    for (number <- 1 until size + 1) {
      var numlist = ListBuffer[Tuple2[Int,Int]]()
      for (x <- 0 until size) {
        numlist.clear()
        for (y <- 0 until size) {
          if (values(y)(x) == number) {
            numlist += (Tuple2(y, x))
          }
          if (numlist.length > 2) {
            for (e <- 0 until numlist.length) {
              if (colors(numlist(e)._1)(numlist(e)._2) == white) {
                for (i <- 0 until numlist.length) {
                  if (i != e) {
                    var check = colors
                    check(numlist(i)._1)(numlist(i)._2) = black
                    if (validMoves(Puzzle(values, check))) {
                      colors(numlist(i)._1)(numlist(i)._2) = black
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    colors
  }

  def sameValueDiffColor2(values:Array[Array[Int]], colors:Array[Array[Int]]): Array[Array[Int]] = {
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        val value = values(x)(y)
        var valPosH = -1
        var valPosV = -1
        var listPosH = new ListBuffer[Int]()
        var listPosV = new ListBuffer[Int]()
        var valuesHor = 1
        var valuesVer = 1
        for (zh <- 0 until size) {
          if (values(x)(y) == values(x)(zh) && zh != y) {
            valuesHor = valuesHor + 1
            valPosH = zh
            listPosH += (zh)
          }
        }
        for (zv <- 0 until size) {
          if (values(x)(y) == values(zv)(y) && zv != x) {
            valuesVer = valuesVer + 1
            valPosV = zv
            listPosV += (zv)
          }
        }
        if (valuesHor == 2) {
          if (colors(x)(valPosH) == black) {
            colors(x)(y) = white
          }
          if (colors(x)(valPosH) == white) {
            colors(x)(y) = black
          }
        }
        if (valuesVer == 2) {
          if (colors(valPosV)(y) == black) {
            colors(x)(y) = white
          }
          if (colors(valPosV)(y) == white) {
            colors(x)(y) = black
          }
        }
      }
    }
    colors
  }

  var colorsNew = colors

  var wAroundBNew = wAroundB(values, colors)
  if (validMoves(Puzzle(values, wAroundBNew))) {
    colorsNew = wAroundBNew
  }

  val sameValueDiffColorNew = sameValueDiffColor(values, colorsNew)
  if (validMoves(Puzzle(values, sameValueDiffColorNew))) {
    colorsNew = sameValueDiffColorNew
  }

  val sameValueDiffColor2New = sameValueDiffColor2(values, colorsNew)
  if (validMoves(Puzzle(values, sameValueDiffColor2New ))) {
    colorsNew = sameValueDiffColor2New
  }

  wAroundBNew = wAroundB(values, colors)
  if (validMoves(Puzzle(values, wAroundBNew))) {
    colorsNew = wAroundBNew
  }

  val horizontalValues = values
  val horizontalColors = colors
  val verticalValues = horizontalValues.transpose
  val verticalColors = horizontalColors.transpose

  // Numbers present on line
  val rowContent = for (row <- horizontalValues) yield row.distinct

  // Create array with numbers and indices
  val rowNumIndices = horizontalValues.flatten.zipWithIndex.grouped(size).toArray

  /*
  val temp = for {
    content <- rowContent
  } yield for {
    num <- content
    row <- rowNumIndices
  } yield row.collect{ case(n, i) if n == num => (n, i)}*/

  val content = (1 to size).toArray
  val temp =  for {
    num <- content
    row <- rowNumIndices
  } yield row.filter{ case(n, _) => n == num }.grouped(5)


  //println(temp)

  /*
  println("****************************")
  temp.foreach {
    x => x.foreach {
        z => print(z + " | ")
    }
    println()
  }
  println("****************************")

  val testArr = Array(4, 3, 2, 2, 4, 5, 6, 6, 1, 2)
  val un = testArr.distinct
  // Separate list into smaller distinct lists [1, 1, 1, 2, 3, 3] => [[1,1,1], [2], [3,3]]
  val tes = for (n <- un) yield testArr.filter(_ == n)

  */



  /*
  // Search each row for content, act upon content length
  val test = rowContent.foreach{ rowContent =>
    for {
      (v, i) <- horizontalValues.flatten.zipWithIndex
    } yield v
  }*/

  // foreach row of content
  // match (value, index)
  // case (1, i) => Set to 1
  // case (v, i) if v > 1 => If 1, set all other 0
  // case (_, i) => Nothing

  // 3-black-round-1-white
  for (x <- 1 until size - 1) {
    for (y <- 1 until size - 1) {
      var blacks = 0
      var whiteX = 0
      var whiteY = 0
      if (colorsNew(x + 1)(y) == black) {
        blacks = blacks + 1
      } else {
        whiteX = x + 1
        whiteY = y
      }
      if (colorsNew(x - 1)(y) == black) {
        blacks = blacks + 1
      } else {
        whiteX = x - 1
        whiteY = y
      }
      if (colorsNew(x)(y + 1) == black) {
        blacks = blacks + 1
      } else {
        whiteX = x
        whiteY = y + 1
      }
      if (colorsNew(x)(y - 1) == black) {
        blacks = blacks + 1
      } else {
        whiteX = x
        whiteY = y - 1
      }
      if (blacks >= 3) {
        colorsNew(whiteX)(whiteY) = white
      }
    }
  }
  if (log) println(validMoves(Puzzle(values,colorsNew)))

  colorsNew
}
*/