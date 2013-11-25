package test.scala.application

import org.scalatest.FlatSpec
import org.scalatest.matchers._
import main.scala.application.InputParse
import scala._
import scala.io.Source

class InputParseSpec extends FlatSpec with ShouldMatchers {
  val new_line_indent = "\n\t\t"

  "Parsing Game1" should "result in a 3x3 matrix, no wrap, and 5 items in the word list" in {
    val test_matrix = Array(
      Array('A', 'B', 'C'),
      Array('D', 'E', 'F'),
      Array('G', 'H', 'I')
    )
    val test_wrap_mode = false
    val test_word_list = Array("FED", "CAB", "GAD", "BID", "HIGH")

    val input_lines = Source.fromFile("src/files/input/game1.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game2" should "result in a 3x3 matrix, wrap, and 5 items in the word list" in {
    val test_matrix = Array(
      Array('A', 'B', 'C'),
      Array('D', 'E', 'F'),
      Array('G', 'H', 'I')
    )
    val test_wrap_mode = true
    val test_word_list = Array("FED", "CAB", "GAD", "BID", "HIGH")

    val input_lines = Source.fromFile("src/files/input/game2.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game3 - invalid wrap value" should "result in a 0x0 matrix, no wrap, and 0 items in the word list" in {
    val test_matrix = Array[Char]()
    val test_wrap_mode = false
    val test_word_list = Array()

    val input_lines = Source.fromFile("src/files/input/game3.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game4" should "result in a 14x14 matrix, no wrap, and 28 items in the word list" in {
    val test_matrix = Array(
        Array('X', 'N', 'F', 'I', 'N', 'I', 'S', 'H', 'L', 'U', 'M', 'B', 'E', 'R'),
        Array('F', 'P', 'L', 'Y', 'W', 'O', 'O', 'D', 'S', 'I', 'Z', 'I', 'N', 'G'),
        Array('G', 'Q', 'S', 'F', 'L', 'G', 'L', 'E', 'I', 'U', 'P', 'K', 'V', 'Z'),
        Array('W', 'R', 'W', 'T', 'W', 'C', 'A', 'N', 'D', 'T', 'R', 'I', 'M', 'D'),
        Array('A', 'A', 'O', 'L', 'O', 'S', 'T', 'I', 'I', 'T', 'O', 'B', 'U', 'R'),
        Array('L', 'T', 'D', 'J', 'O', 'C', 'H', 'A', 'N', 'S', 'I', 'T', 'B', 'I'),
        Array('L', 'T', 'N', 'N', 'A', 'W', 'K', 'R', 'G', 'O', 'S', 'L', 'A', 'T'),
        Array('I', 'A', 'I', 'Q', 'I', 'M', 'A', 'G', 'S', 'P', 'L', 'I', 'T', 'H'),
        Array('N', 'N', 'W', 'M', 'A', 'E', 'B', 'E', 'H', 'A', 'E', 'W', 'T', 'A'),
        Array('G', 'P', 'A', 'L', 'P', 'I', 'H', 'S', 'I', 'R', 'C', 'R', 'E', 'T'),
        Array('F', 'X', 'W', 'T', 'B', 'I', 'T', 'O', 'N', 'Q', 'X', 'O', 'N', 'C'),
        Array('F', 'K', 'A', 'F', 'D', 'A', 'Y', 'L', 'G', 'U', 'E', 'Y', 'R', 'H'),
        Array('Q', 'D', 'A', 'C', 'K', 'Y', 'P', 'C', 'L', 'E', 'L', 'O', 'P', 'K'),
        Array('L', 'M', 'V', 'E', 'N', 'E', 'E', 'R', 'E', 'T', 'T', 'U', 'H', 'S')
    )
    val test_wrap_mode = false
    val test_word_list = Array("BATTEN", "BEAM", "CLOSEGRAINED", "CORK", "EXCELSIOR", "FINISHLUMBER", "JAMBS",
      "LATH", "PARQUET", "PLYWOODSIZING", "POLE", "POST", "RATTAN", "RUSH", "SEASONING", "SHIPLAP", "SHUTTER",
      "SIDINGSHINGLE", "SLAT", "SPLIT", "STAKE", "STOCK", "STUD", "THATCH", "TRIM", "VENEER", "WALLING", "WINDOWS")

    val input_lines = Source.fromFile("src/files/input/game4.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game5" should "result in a 3x4 matrix, no wrap, and 8 items in the word list" in {
    val test_matrix = Array(
      Array('A', 'B', 'C', 'D'),
      Array('E', 'F', 'G', 'H'),
      Array('I', 'J', 'K', 'L')
    )
    val test_wrap_mode = false
    val test_word_list = Array("FED", "CAB", "GAD", "BID", "HIGH", "XYZ", "BE", "BH")

    val input_lines = Source.fromFile("src/files/input/game5.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }


  "Parsing Game6" should "result in a 3x4 matrix, no wrap, and 5 items in the word list" in {
    val test_matrix = Array(
      Array('A', 'B', 'C', 'D'),
      Array('E', 'F', 'G', 'H'),
      Array('I', 'J', 'K', 'L')
    )
    val test_wrap_mode = false
    val test_word_list = Array("DIF", "DGJ", "BCDA", "BKHA", "EI")

    val input_lines = Source.fromFile("src/files/input/game6.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game7" should "result in a 3x4 matrix, wrap, and 5 items in the word list" in {
    val test_matrix = Array(
      Array('A', 'B', 'C', 'D'),
      Array('E', 'F', 'G', 'H'),
      Array('I', 'J', 'K', 'L')
    )
    val test_wrap_mode = true
    val test_word_list = Array("DIF", "DGJ", "BCDA", "BKHA", "EI")

    val input_lines = Source.fromFile("src/files/input/game7.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }

  "Parsing Game8 - incorrect number of words in word list" should "result in a 0x0 matrix, no wrap, and 0 items in the word list" in {
    val test_matrix = Array[Char]()
    val test_wrap_mode = false
    val test_word_list = Array()

    val input_lines = Source.fromFile("src/files/input/game8.input").getLines().withFilter(!_.equals(""))
    val (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections

    for((row, row_idx) <- test_matrix.zipWithIndex) row should equal (test_matrix(row_idx))
    matrix.grid.length should equal (test_matrix.length)

    wrap_mode should equal (test_wrap_mode)
    word_list should equal (test_word_list)
  }
}