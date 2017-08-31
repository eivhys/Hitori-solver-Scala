object hitoriSolver extends App {

  //TODO: get puzzle from .txt file (to 1D array for fastest performance)
  //TODO: apply starting techniques
  //TODO: -1 = NO COLOR, 0 = BLACK, 1 = WHITE

  //Timer
  val timer = System.currentTimeMillis()

  val colorNone = -1
  val colorBlack = 0
  val colorWhite = 1

  var position = 0

  def oneDtoTwoDX(x:Int): Int = x % m

  def oneDtoTwoDY(y:Int): Int = {
    (y - oneDtoTwoDX(y)) / m
  }

  def twoDtoOneD(x:Int, y:Int): Int = x + (m * y)

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

    if (m > 3) {
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

      if (m > 4) {
        // Pair induction
        for (e <- 0 until m) {
          for (i <- 0 until n - 4) {
            if (values(i + e * m) == values(i + 2 + e * m) && values(i + 2 + e * m) == values(i + 3 + e * m)) {
              newColors(i + e * m) = colorBlack
            }
            if (values(i + e * m) == values(i + 1 + e * m) && values(i + 1 + e * m) == values(i + 3 + e * m)) {
              newColors(i + 3 + e * m) = colorBlack
            }
            if (values(i * m + e) == values(i * m + e + m * 2) && values (i * m + e + m * 2) == values(i * m + e + m * 3)) {
              newColors(i * m + e) = colorBlack
            }
            if (values(i * m + e) == values(i * m + e + m * 1) && values (i * m + e + m * 1) == values(i * m + e + m * 3)) {
              newColors(i * m + e + m * 3) = colorBlack
            }
          }
        }
      }
    }

    //White around all blacks
    for (x <- 0 until m) {
      for (y <- 0 until n) {
        if (newColors(twoDtoOneD(x, y)) == colorBlack) {
          if (x >= 0 && x < m) {
            newColors(twoDtoOneD(x + 1, y)) = colorWhite
          }
          if (x < m && y > 0) {
            newColors(twoDtoOneD(x - 1, y)) = colorWhite
          }
          if (y >= 0 && y < m) {
            newColors(twoDtoOneD(x, y + 1)) = colorWhite
          }
          if (y < m && y > 0) {
            newColors(twoDtoOneD(x, y - 1)) = colorWhite
          }
        }
      }
    }

    newColors

  }

  def isSolved(values:Array[Int], colors:Array[Int]): Boolean = {

    //TODO Gotta make this work somehow, we using recursion bro

    //Sees that there are no non-colored cells, all cells must be black or white
    for (e <- 0 until m * n) {
      if (colors(e) == colorNone) {
        println("Contains non-colored cells")
        false
      }
    }

    //Checks if there are more than one white cell with a certain value
    for (e <- 0 until m*n) {
      if (colors(e) == colorWhite) {
        for (i <- 0 until m) {
          val x = i + (e / m) * m
          if (values(e) == values(x) && e != x && colors(x) == colorWhite) {
            println("Two horizontal cells have equal value and are white: " + e + ": " + values(e) + toColor(colors(e)) + ", " + x + ": " + values(x) + toColor(colors(x)))
            return false
          }
          val y = i * m + e % m
          if (values(e) == values(y) && e != y && colors(y) == colorWhite) {
            println("Two vertical cells have equal value and are white: " + e + ": " + values(e) + toColor(colors(e)) + ", " + y + ": " + values(y) + toColor(colors(y)))
            return false
          }
        }
      }
    }

    //TODO: Check for continous path between all white cells, all white cells needs a friend <3 Black cells can't have friends :-(
    // Fill


    true

  }

  def solve(values:Array[Int], colors:Array[Int], index:Int = 0): Array[Int] = {
    val newColors:Array[Int] = colors

    //Solves if not solved
    if (!isSolved(values, newColors)) {
      //puzzlePrint(values, colors)
    }
    newColors
  }

  val m, n = 5 //Dimensions of array (should be the same value)

  val puzzleBoardValues:Array[Int] = Array(1,2,2,4,4,1,1,3,1,5,5,5,2,1,4,1,5,2,3,4,1,3,4,5,1) //Example puzzle

  val puzzleBoardColors = applyStartingTechniques(puzzleBoardValues)


  if (isSolved(puzzleBoardValues, puzzleBoardColors)) {
    println("Puzzle is already complete!")
    puzzlePrint(puzzleBoardValues, puzzleBoardColors)
  } else {
    val solvedBoard = solve(puzzleBoardValues, puzzleBoardColors)
    puzzlePrint(puzzleBoardValues, solvedBoard)
  }

  puzzlePrint(puzzleBoardValues, puzzleBoardColors)
  println("Puzzle completed in " + ((System.currentTimeMillis() - timer) / 1000) + " seconds!" )
  //TODO Recursive solver function


}