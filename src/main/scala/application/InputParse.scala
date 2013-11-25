package main.scala.application

import main.scala.domain.Matrix

/** Helper class used to process the input lines.
  *
  * @constructor create a new instance of the parser and sets the properties: matrix, wrap_mode, and word_list.
  * @param input_lines a list of lines containing the content of the game.
  * @return an instance of InputParse.
  */
class InputParse(input_lines: Iterator[String]) {
  /** Helper used to process the matrix section based on the height and width which is retrieved from the first line.
    *
    * @param lines a list of lines containing the matrix with the first line declaring how many words should be found.
    * @return the discovered matrix and lines left to process.
    */
  def parse_matrix(lines: Iterator[String]): Option[(Matrix, Iterator[String])] = {
    val dimension = lines.next().split(" ", 2)
    val height = Integer.valueOf(dimension(0))
    val width = Integer.valueOf(dimension(1))
    val matrix = new Matrix(height, width, lines.take(height))

    if(matrix.grid.length == height) {
      Some(matrix, lines)
    } else None
  }

  /** Helper used to process the wrap mode if found.
    *
    * @param lines a list of lines which the first line should be the wrap mode.
    * @return the wrap mode and lines left to process.
    */
  def parse_wrap_mode(lines: Iterator[String]): Option[(Boolean, Iterator[String])] = lines.next() match {
    case "NO_WRAP" => Some(false, lines)
    case "WRAP" => Some(true, lines)
    case _ => None
  }

  /** Helper used to process the word list section based on number found on the first line.
    *
    * @param lines a list of lines containing the word list with the first line declaring how many words should be found.
    * @return the word list and lines left to process.
    */
  def parse_word_list(lines: Iterator[String]): Option[(Array[String], Iterator[String])] = {
    val word_count = Integer.valueOf(lines.next())
    val word_list = lines.take(word_count).toArray

    if(word_list.length == word_count) {
      Some(word_list, lines)
    } else None
  }

  lazy val sections = (matrix, wrap_mode, word_list)

  /*
    Constructor

    @algorithm
      1) Parse the matrix.
      2) Get the wrap mode.
      3) Store all the words to be matched in a list.
      4) If everything was found successfully then set matrix, wrap_mode, and word_list variables with
        their relevant values, otherwise the default values set will be an empty matrix, false for wrap_mode,
        and an empty word_list.
   */

  val (matrix, wrap_mode, word_list) = (for(
    (matrix, lines) <- parse_matrix(input_lines);
    (wrap_mode, lines) <- parse_wrap_mode(lines);
    (word_list, lines) <- parse_word_list(lines)
  ) yield (matrix, wrap_mode, word_list)).getOrElse(new Matrix(0, 0, Array("").iterator), false, Array[String]())
}
