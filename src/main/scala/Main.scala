object hitoriSolver extends App {

  //TODO: get puzzle from .txt file (to 1D array for fastest performance)
  //TODO: apply starting techniques
  //TODO: -1 = NO COLOR, 0 = BLACK, 1 = WHITE

  //Timer
  val timer = System.currentTimeMillis()

  val colorNone = -1
  val colorBlack = 0
  val colorWhite = 1

  def toColor(x:Int): String = {
    x match {
      case -1 => "None"
      case 0 => "Black"
      case 1 => "White"
    }
  }

  def puzzlePrint (values:Array[Int], colors:Array[Int]): Unit = {
    //Prints the board
    print("Hitori puzzle board" + " " + m + "x" + n + ":")
    for (e <- 0 until m*n) {
      if (e%5 == 0) print("\n")
      print(values(e) + toColor(colors(e)) + " ")
    }
    println("")
  }

  def applyStartingTechniques (values:Array[Int]): Array[Int] = {

    //Gives cells a color based on their position relative to each other

    //TODO Needs more starting techniques

    val newColors:Array[Int] = new Array[Int](m * n)
    for (e <- 0 until m*n) {
      newColors(e) = colorNone
    }
    //Adjacent triples
    //Horizontal
    for (e <- 0 until m) {
      for (i <- 0 until n - 3) {
        if (values(e * m + i) == values(e * m + i + 1) && values(e * m + i) == values(e * m + i + 2)) {
          newColors(e * m + i) = colorBlack
          newColors(e * m + i + 1) = colorWhite
          newColors(e * m + i + 2) = colorBlack
        }
      }
    }
    //Vertical
    for (e <- 0 until m - 3) {
      for (i <- 0 until n) {
        if (values(e * n + i) == values((e + 1) * n + i) && values(e * n + i) == values((e + 2) * n + i)) {
          newColors(e * n + i) = colorBlack
          newColors((e + 1) * n + i) = colorWhite
          newColors((e + 2) * n + i) = colorBlack
        }
      }
    }

    //Square between pair
    //Horizontal
    for (e <- 0 until m) {
      for (i <- 0 until n - 3) {
        if (values(e * m + i) == values(e * m + i + 2)) {
          newColors(e * m + i + 1) = colorWhite
        }
      }
    }
    //Vertical
    for (e <- 0 until m - 3) {
      for (i <- 0 until n) {
        if (values(e * n + i) == values((e + 2) * n + i)) {
          newColors((e + 1) * n + i) = colorWhite
        }
      }
    }

    //Pair induction

    //White around all blacks

    newColors

  }

  def isSolved(values:Array[Int], colors:Array[Int]): Boolean = {

    //TODO Gotta make this work somehow, we using recursion bro

    //Sees that there are no non-colored cells, all cells must be black or white
    for (e <- 0 until m*n) {
      if (colors(e) == colorNone) {
        //println("Contains non-colored cells")
        //return false
      }
    }

    //Checks if there are more than one white cell with a certain value
    for (e <- 0 until m*n) {
      if (colors(e) == colorWhite) {
        for (i <- 0 until m) {
          val x = i + (e / m) * m
          if (values(e) == values(x) && e != x && colors(x) != colorBlack) {
            println("Two horizontal cells have equal value and are white: " + e + ": " + values(e) + toColor(colors(e)) + ", " + x + ": " + values(x) + toColor(colors(x)))
            return false
          }
          val y = i * m + e % m
          if (values(e) == values(y) && e != y && colors(y) != colorBlack) {
            println("Two vertical cells have equal value and are white: " + e + ": " + values(e) + toColor(colors(e)) + ", " + y + ": " + values(y) + toColor(colors(y)))
            return false
          }
        }
      }
    }

    //TODO: Check for continous path between all white cells, all white cells needs a friend <3 Black cells can't have friends :-(

    true

  }


  def solve(values:Array[Int], colors:Array[Int], index:Int = 0): Array[Int] = {
    val newColors:Array[Int] = colors

    //Solves if not solved
    if (!isSolved(values, newColors)) {
      puzzlePrint(values, colors)
    }
    newColors
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
    puzzlePrint(puzzleBoardValues, solvedBoard)
  }

  println("Puzzle completed in " + ((System.currentTimeMillis() - timer) / 1000) + " seconds!" )
  //TODO Recursive solver function



}