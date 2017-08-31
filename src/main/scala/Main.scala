/*object hitoriSolver extends App {

  //TODO: get puzzle from .txt file (to 1D array for fastest performance)
  //TODO: apply starting techniques

  //Timer
  val timer = System.currentTimeMillis()

  def puzzlePrint (values:Array[Int], colors:Array[Colors.Value]): Unit = {
    //Prints the board
    print("Hitori puzzle board" + " " + m + "x" + n + ":")
    for (e <- 0 until m*n) {
      if (e%5 == 0) print("\n")
      print(values(e) + colors(e).toString + " ")
    }
    println("")
  }

  def applyStartingTechniques (values:Array[Int]): Array[Colors.Value] = {

    //Gives cells a color based on their position relative to each other

    //TODO Needs more starting techniques

    val newColors:Array[Colors.Value] = new Array[Colors.Value](m * n)
    for (e <- 0 until m*n) {
      newColors(e) = Colors.None
    }
    //Adjacent triples
    //Horizontal
    for (e <- 0 until m) {
      for (i <- 0 until n - 3) {
        if (values(e * m + i) == values(e * m + i + 1) && values(e * m + i) == values(e * m + i + 2)) {
          newColors(e * m + i) = Colors.Black
          newColors(e * m + i + 1) = Colors.White
          newColors(e * m + i + 2) = Colors.Black
        }
      }
    }
    //Vertical
    for (e <- 0 until m - 3) {
      for (i <- 0 until n) {
        if (values(e * n + i) == values((e + 1) * n + i) && values(e * n + i) == values((e + 2) * n + i)) {
          newColors(e * n + i) = Colors.Black
          newColors((e + 1) * n + i) = Colors.White
          newColors((e + 2) * n + i) = Colors.Black
        }
      }
    }

    //Square between pair
    //Horizontal
    for (e <- 0 until m) {
      for (i <- 0 until n - 3) {
        if (values(e * m + i) == values(e * m + i + 2)) {
          newColors(e * m + i + 1) = Colors.White
        }
      }
    }
    //Vertical
    for (e <- 0 until m - 3) {
      for (i <- 0 until n) {
        if (values(e * n + i) == values((e + 2) * n + i)) {
          newColors((e + 1) * n + i) = Colors.White
        }
      }
    }

    //Pair induction

    //White around all blacks

    newColors

  }

  def isSolved(values:Array[Int], colors:Array[Colors.Value]): Boolean = {

    //TODO Gotta make this work somehow, we using recursion bro

    //Sees that there are no non-colored cells, all cells must be black or white
    for (e <- 0 until m*n) {
      if (colors(e) == Colors.None) {
        //println("Contains non-colored cells")
        //return false
      }
    }

    //Checks if there are more than one white cell with a certain value
    for (e <- 0 until m*n) {
      if (colors(e) == Colors.White) {
        for (i <- 0 until m) {
          val x = i + (e / m) * m
          if (values(e) == values(x) && e != x && colors(x) != Colors.Black) {
            println("Two horizontal cells have equal value and are white: " + e + ": " + values(e) + colors(e) + ", " + x + ": " + values(x) + colors(x))
            return false
          }
          val y = i * m + e % m
          if (values(e) == values(y) && e != y && colors(y) != Colors.Black) {
            println("Two vertical cells have equal value and are white: " + e + ": " + values(e) + colors(e) + ", " + y + ": " + values(y) + colors(y))
            return false
          }
        }
      }
    }

    //TODO: Check for continous path between all white cells, all white cells needs a friend <3 Black cells can't have friends :-(

    true

  }

  def potentialMove (values:Array[Int], colors:Array[Colors.Value], index:Int): Array[Colors.Value] = {
    val e = index
    if (!isSolved(values, colors)) {
      if (colors(e) != Colors.Black) {
        for (i <- 0 until m) {
          val x = i + (e / m) * m
          if (values(e) == values(x) && e != x && colors(x) != Colors.Black) {
            //decide between two
            if (shouldSetBlack(values, colors, index)) {
              colors(e) = Colors.Black
              solve(values, colors, index + 1)
            } else  {
              colors(e) = Colors.White
              solve(values, colors, index + 1)
            }
          }
          val y = i * m + e % m
          if (values(e) == values(y) && e != y && colors(y) != Colors.Black) {
            if (shouldSetBlack(values, colors, index)) {
              colors(e) = Colors.Black
              solve(values, colors, index + 1)
            } else  {
              colors(e) = Colors.White
              solve(values, colors, index + 1)
            }
          }
        }
      }
    }
    colors
  }

  def shouldSetBlack(values:Array[Int], colors:Array[Colors.Value], index:Int): Boolean = {
    if (colors(index) == Colors.White) {
      false
    }
    for (e <- 0 until m) {
      for (i <- 0 until n) {
        if (values(index) == values(e + index / m) && values(index) == values(i * n + index % n)) {
          if (colors(index) == Colors.None && colors(e + index / m) == Colors.None && colors(i * n + index % n) == Colors.None) {
            true
          } else false
        } else false
      }
    }
    false
  }

  def solve(values:Array[Int], colors:Array[Colors.Value], index:Int = 0): Array[Colors.Value] = {
    val newColors:Array[Colors.Value] = colors

    //Solves if not solved
    if (!isSolved(values, newColors)) {
      puzzlePrint(values, colors)
      potentialMove(values, colors, index)
    }
    newColors
  }

  object Colors extends Enumeration { //Enum to distinguish cells' colors
    type Color = Value
    val Black, White, None = Value
  }

  val m, n = 5 //Dimensions of array (should be the same value)

  val puzzleBoardValues:Array[Int] = Array(1,2,2,4,4,1,1,3,2,5,5,5,2,1,4,4,5,2,3,4,1,3,4,5,1) //Example puzzle

  val puzzleBoardColors = applyStartingTechniques(puzzleBoardValues)

  puzzlePrint(puzzleBoardValues, puzzleBoardColors)

  if (isSolved(puzzleBoardValues, puzzleBoardColors)) {
    println("Puzzle is already complete!")
    puzzlePrint(puzzleBoardValues, puzzleBoardColors)
  } else {
    val solvedBoard = solve(puzzleBoardValues, puzzleBoardColors)
    println("Puzzle completed in " + ((System.currentTimeMillis() - timer) / 1000) + " seconds!" )
    puzzlePrint(puzzleBoardValues, solvedBoard)
  }

  //TODO Recursive solver function



}*/