package main.scala.domain

import scala.Array.canBuildFrom
import scala.collection.mutable

/** Class used to store the letter matrix and to query the letter locations
  *
  * @constructor create a new instance of the matrix
  * @param height of the matrix
  * @param width of the matrix
  * @param lines used to populate the matrix
  * @return an instance of Matrix
  */
class Matrix(height: Int, width: Int, lines: Iterator[String]) {
  private type Coordinate = (Int, Int)

  override def toString: String = grid.map(_.mkString("")).mkString("\n")

  private def increase(left:Int)(right:Int) = left + right
  private def decrease(left:Int)(right:Int) = left - right

  /** Gets all the valid vectors based on the locations given
    *
    * @param coordinates of the locations
    * @param size of the vector to look for
    * @param wrap_mode determines whether vectors wrap past the boundaries
    * @return a list of vectors
    */
  def get_neighborhood_vectors(coordinates: mutable.ArrayBuffer[Coordinate], size: Int, wrap_mode: Boolean = false) = {
    /** Helper function for the filter to drop all vectors that has overlapping coordinates within it.
      *
      * @param vector of coordinates
      * @return true if no overlapping coordinates found, false otherwise
      */
    def check_no_overlap(vector: List[Coordinate]): Boolean = {
      val map = mutable.HashMap[Coordinate, Int]()
      vector.foreach(coordinate => {
        map.get(coordinate) match {
          case Some(x) => return false
          case None => map.put(coordinate, 1)
        }
      })
      true
    }

    wrap_mode match {
      case true => coordinates.map(coordinate => get_neighborhood_vectors_wrap(coordinate, size)
        .filter(vector => check_no_overlap(vector)))
        .flatten
      case false => coordinates.map(coordinate => get_neighborhood_vectors_nowrap(coordinate, size)).flatten
    }
  }

  /** Gets all the valid vectors based on a location given, boundaries limit the results returned.
    *
    * @param coordinate of the location
    * @param size of the vector to look for
    * @return a list of vectors
    */
  def get_neighborhood_vectors_nowrap(coordinate: Coordinate, size: Int) = {
    val (y_origin, x_origin) = coordinate
    val size_ = size - 1

    val possible_vectors = for(
      y_endpoint <- List.range(y_origin - size_, y_origin + size, size_)
        if y_endpoint >= 0
          && y_endpoint <= height - 1;
      x_endpoint <- List.range(x_origin - size_, x_origin + size, size_)
        if x_endpoint >= 0
          && x_endpoint <= width - 1
          && !(y_endpoint, x_endpoint).equals(coordinate)
    ) yield {
      val x_step = if(x_endpoint > x_origin) increase(x_origin) _ else decrease(x_origin) _
      val y_step = if(y_endpoint > y_origin) increase(y_origin) _ else decrease(y_origin) _

      for(offset <- List.range(0, size)) yield {
        val x_offset = if(x_origin != x_endpoint) {
          x_step(offset)
        } else x_origin

        val y_offset = if(y_origin != y_endpoint) {
          y_step(offset)
        } else y_origin

        (y_offset, x_offset)
      }
    }
    possible_vectors
  }

  /** Gets all the valid vectors based on a location given, boundaries are ignored and wrapped instead
    *
    * @param coordinate of the location
    * @param size of the vector to look for
    * @return a list of vectors
    */
  def get_neighborhood_vectors_wrap(coordinate: Coordinate, size: Int) = {
    val (y_origin, x_origin) = coordinate
    val size_ = size - 1

    val possible_vectors = for(
      y_endpoint <- List.range(y_origin - size_, y_origin + size, size_);
      x_endpoint <- List.range(x_origin - size_, x_origin + size, size_)
        if !(y_endpoint, x_endpoint).equals(coordinate)
    ) yield {
      val x_step = if(x_endpoint > x_origin) increase(x_origin) _ else decrease(x_origin) _
      val y_step = if(y_endpoint > y_origin) increase(y_origin) _ else decrease(y_origin) _

      for(offset <- List.range(0, size)) yield {
        val x_offset = if(x_origin != x_endpoint) {
          val x_temp = x_step(offset) % width

          if(x_temp >= 0) x_temp
          else x_temp + width
        } else x_origin

        val y_offset = if(y_origin != y_endpoint) {
          val y_temp = y_step(offset) % height

          if(y_temp >= 0) y_temp
          else y_temp + height
        } else y_origin

        (y_offset, x_offset)
      }
    }
    possible_vectors
  }

  /*
    Constructor

    @algorithm
      1) Create a 2 dimensional array of strings, height and width of the matrix are fixed based on values passed in.
      2) Create a hash map of all the locations each letter resides in the grid for fast lookup, the key is the char,
        and the value is a list of locations.
      3) Iterate over each row, splitting the input string into an array of Char and only taking the predefined
        width in Chars, anything extra is discarded.
      4) If the Char doesn't exist in the hash map then add an entry for it and assign the first location to it,
        otherwise add a new location to its list.
   */

  val grid = new Array[Array[Char]](height)
  val locations = new mutable.HashMap[Char, mutable.ArrayBuffer[Coordinate]]

  for((row, row_index) <- grid.zipWithIndex) {
    grid(row_index) = lines.next().toArray.take(width)

    for((col, col_index) <- grid(row_index).zipWithIndex) {
      locations.get(col) match {
        case None => locations(col) = new mutable.ArrayBuffer[Coordinate]()
        case _ => ()
      }

      locations(col) += ((row_index, col_index))
    }
  }
}