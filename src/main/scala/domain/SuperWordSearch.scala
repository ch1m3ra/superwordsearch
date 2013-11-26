package main.scala.domain

import scala.collection.mutable

/** Main class for the game.
  *
  * @constructor create a new instance of the word search game.
  * @param matrix stores all the letters for the game.
  * @param wrap_mode determines whether the edges of the matrix limit results.
  * @param word_list is the list of all words to look for.
  * @return an instance of SuperWordSearch.
  */
class SuperWordSearch(matrix: Matrix, wrap_mode: Boolean, word_list: Array[String]) {
  /** Get all vectors for a given word.
    *
    * @param word to query the matrix for.
    * @return a tuple of word and a list of vectors of possible words.
    */
  private def get_location_vectors(word: String) = {
    possible_words(word) match {
      case Some(locations) =>
        (word, matrix.get_neighborhood_vectors(
          locations,
          word.size,
          wrap_mode
        ))
      case None => (word, mutable.ArrayBuffer())
    }
  }

  /*
    Constructor

    @algorithm
      1) For each word get all the locations where the first letter is found in the matrix.
      2) Get all the possible valid vectors radiating in the 8 cardinal directions for each location.
      3) For each word in the word list test all of the vectors against that word to see if they match.
      4) If the word was not found in the matrix then return "NOT FOUND", otherwise return the
        starting and ending locations for the word. If more than one match is found then display an error since
        the instructions stated that would never happen.
   */

//  println(matrix)

  private val possible_words = Map() ++ word_list.map(word =>
    (word, matrix.locations.get(word.head))
  )

  private val location_vectors = word_list.map(word => get_location_vectors(word))

  val results = for((word, coordinate_results) <- location_vectors) yield {
    val results = (for(coordinate_vector <- coordinate_results) yield {
      val word_test = (for((y, x) <- coordinate_vector) yield matrix.grid(y)(x)).mkString("")
      if(word.equals(word_test))
        Some("%s %s".format(coordinate_vector(0), coordinate_vector(coordinate_vector.length - 1)))
      else None
    }).filter(_ != None).map(_.get)

    val result = results.length match {
      case 0 => "NOT FOUND"
      case 1 => results.head
      case _ => "ERROR: MULTIPLE RESULTS"
    }

    (word, result)
  }
}