package main.scala

import scala.io.Source
import main.scala.application.InputParse
import main.scala.domain.SuperWordSearch

/** Main way to run the game.
  *
  * @constructor for each argument passed in the command line a Super Word Search is executed.
  */
object Main extends BaseScript {
  val game_inputs = load_configuration(args)

  val word_search_games: Array[(String, SuperWordSearch)] = for(
    game_path <- game_inputs;
    input_lines = Source.fromFile(game_path).getLines().withFilter(!_.equals(""));
    (matrix, wrap_mode, word_list) = new InputParse(input_lines).sections;
    super_word_search = new SuperWordSearch(matrix, wrap_mode, word_list)
  ) yield (game_path, super_word_search)

  for((path, game) <- word_search_games) {
    println("#--------------------------------#")
    println(path)
    println("#--------------------------------#")
    game.results.foreach(result => println(result._2))
    println("")
  }
}

/** Helper class to handle getting arguments from the command line, if no arguments are passed in then a
  *   default game file is processed.
  *
  * @constructor create a new instance of BaseScript.
  * @param-implicit args a list of files containing games to be processed.
  * @return an instance of BaseScript.
  */
class BaseScript extends App {
  def load_configuration( args: Array[String] ) = {
    if ( args.length > 0 ) args else Array("src/files/input/game1.input")
  }
}