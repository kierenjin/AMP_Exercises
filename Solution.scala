import scala.io.StdIn.readLine

//Class object to hold blocks of "-" spaces to be filled.
class WordBlock(val start: Int, val end: Int, val vertical: Boolean, val colmOrRow: Int) {
  //initialize variables to set block characteristics.
  var isVertical: Boolean = vertical
  var columnOrRow: Int = colmOrRow
  var startIndex: Int = start
  var endIndex: Int = end
}

class WordPuzzle(var matrix: Array[Array[Char]], val ws: Array[String], val dimension: Int) {

  //method to check if any negative "-" spaces remain
  def isNegativeRemain(m: Array[Array[Char]]): Boolean = {
    if (m == null)
      return false

    //iterative over entire 2d array.
    for (i <- 0 until dimension)
      for (j <- 0 until dimension)
        if (m(i)(j) == '-')
          return false
    true
  }

  //Function to iterate every row and column to find continous blocks of '-' to be filled by words in list.
  def findWordBlocks(word: String, matrix: Array[Array[Char]]): Array[WordBlock] = {
    var blocks: Array[WordBlock] = Array()

     /*
      Iterative over every row and column find all continous '-' word block.
      blocks will be stored.
     */

    //1. Iterative over every row to find word blocks.
    for (i <- 0 until dimension)
    {
      //set row object
      val row = matrix(i)

      //set word block start index to -1. will remain -1 if '-' is not found in row or column.
      var start = -1
      var end = dimension //set end index to end of array length.

      for (j <- 0 until dimension) {
        //
        if (row(j) != '+' && start == -1)
          start = j //detected first '-', set start index to j position.
        else if ((row(j) == '+' || j == dimension - 1) && start != -1) {
          if (row(j) == '+')
            end = j //end of word block
          else
            end = j + 1 //end of array (i.e. last element of array is a '-' space
          if (word.length == end - start)
            //Add horizontal wordblock to array.
            blocks = blocks :+ new WordBlock(start, end - 1, false, i)

          //Reset start index to -1 for next row.
          start = -1
        }
      }
    }

    //2. Iterative over every column to find word blocks.
    for (i <- 0 until dimension) {
      var start = -1
      var end = dimension
      for (j <- 0 until dimension) {
        if (matrix(j)(i) != '+' && start == -1)
          start = j
        else if ((matrix(j)(i) == '+' || j == dimension - 1) && start != -1) {
          end = j
          if (matrix(j)(i) == '+')
            end = j
          else
            end = j + 1
          if (word.length == end - start)
            blocks = blocks :+ new WordBlock(start, end - 1, true, i)
          start = -1
        }
      }
    }

    return blocks
  }

  def fillWord(word: String, pos: WordBlock, matrix: Array[Array[Char]]): Array[Array[Char]] = {
    if (pos.vertical) {
      for (j <- pos.start to pos.end)
        if (matrix(j)(pos.columnOrRow) == '-' || word.charAt(j - pos.start) == matrix(j)(pos.columnOrRow))
          matrix(j)(pos.columnOrRow) = word.charAt(j - pos.start)
        else
        // the word cannot be placed in this slot
          return null
    } else {
      for (j <- pos.start to pos.end)
        if (matrix(pos.columnOrRow)(j) == '-' || word.charAt(j - pos.start) == matrix(pos.columnOrRow)(j))
          matrix(pos.columnOrRow)(j) = word.charAt(j - pos.start)
        else
        // the word cannot be placed in this slot
          return null
    }
    matrix
  }

  //Function to check edge cases
  def CheckPuzzleConstraint(matrix: Array[Array[Char]], words: Array[String]): Boolean = {
    if (words.length == 0 || words.length > 10) {
      return false
    }

    //word must contain only letters from a to z.
    for(s <- words) {
      if(!s.matches(".*[a-z].*")) {
        return false;
      }
    }

    return true;
  }

  //Main function to solve word puzzle.
  def fillPuzzle(matrix: Array[Array[Char]], words: Array[String]): Array[Array[Char]] = {

    if (isNegativeRemain(matrix)) {
      this.matrix = matrix
      return matrix
    }

    for (word <- words)
      for (pos <- findWordBlocks(word, matrix)) {
        // Put word in the slot and remove it from the list of words
        var clonedMtx = matrix.map(_.clone)
        clonedMtx = fillWord(word, pos, clonedMtx)

        if (clonedMtx != null) {
          val restOfWords = words.filter(_ != word)
          val mtx = fillPuzzle(clonedMtx, restOfWords)
          if (isNegativeRemain(mtx))
            return mtx
        }
      }
    null
  }

  def PrintPuzzle(): Unit = {
    for (i <- 0 until dimension) {
      for (j <- 0 until dimension)
        print(this.matrix(i)(j))
      println()
    }
  }
}

object Solution {

  def main(args: Array[String]) {

    println("Enter the list of words separated by ';'")
    val line = readLine().trim.toUpperCase
    val words = line.split(";")
    println("Enter the 10 by 10 matrix")
    var matrix = Array.ofDim[Char](10, 10)
    for (i <-0 until 10) {
      val line = readLine().trim
      matrix(i) = line.toCharArray
    }

    val cw: WordPuzzle = new WordPuzzle(matrix, words, 10)
    if(cw.CheckPuzzleConstraint(cw.matrix, words)) {
      cw.fillPuzzle(cw.matrix, words)
      println("___________ Puzzle Solution ___________")
      cw.PrintPuzzle()
    }
    else {
      println("___________ Failed Puzzle Constraints ___________")
    }
  }
}

