package test.scala.domain

import scala.collection.mutable
import org.scalatest.FlatSpec
import org.scalatest.matchers._
import main.scala.domain.Matrix
import scala._

class MatrixSpec extends FlatSpec with ShouldMatchers {
  val new_line_indent = "\n\t\t"

  {
    val row_one = Array('A', 'B', 'C')
    val row_two = Array('D', 'E', 'F')
    val row_three = Array('G', 'A', 'I')
    val matrix = new Matrix(3, 3, Array(
        row_one.mkString(""),
        row_two.mkString(""),
        row_three.mkString("")
    ).iterator)

    "A 3x3 Matrix" should "parse all the input lines and break them into a list of characters." in {
      matrix.grid(0) should equal (row_one)
      matrix.grid(1) should equal (row_two)
      matrix.grid(2) should equal (row_three)
    }

    it should "be able to return all locations for the letter A" in {
      val locations = mutable.ArrayBuffer((0,0), (2, 1))
      matrix.locations.get('A').get should equal (locations)
    }

    it should "be able to return all locations for the letter C" in {
      val locations = mutable.ArrayBuffer((0,2))
      matrix.locations.get('C').get should equal (locations)
    }

    it should "be able to return all locations for the letter G" in {
      val locations = mutable.ArrayBuffer((2,0))
      matrix.locations.get('G').get should equal (locations)
    }

    it should "be able to return all locations for the letter I" in {
      val locations = mutable.ArrayBuffer((2,2))
      matrix.locations.get('I').get should equal (locations)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2)),

        List((2,1), (1,1), (0,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (0,1), (0,0)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (2,1), (2,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (2,2), (1,1)),
        List((0,0), (2,0), (1,0)),
        List((0,0), (2,1), (1,2)),
        List((0,0), (0,2), (0,1)),
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,2), (2,1)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2)),

        List((2,1), (1,0), (0,2)),
        List((2,1), (1,1), (0,1)),
        List((2,1), (1,2), (0,0)),
        List((2,1), (2,0), (2,2)),
        List((2,1), (2,2), (2,0)),
        List((2,1), (0,0), (1,2)),
        List((2,1), (0,1), (1,1)),
        List((2,1), (0,2), (1,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (2,1), (1,0)),
        List((0,2), (2,2), (1,2)),
        List((0,2), (2,0), (1,1)),
        List((0,2), (0,1), (0,0)),
        List((0,2), (0,0), (0,1)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2)),
        List((0,2), (1,0), (2,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((2,0), (1,2), (0,1)),
        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,2), (2,1)),
        List((2,0), (2,1), (2,2)),
        List((2,0), (0,2), (1,1)),
        List((2,0), (0,0), (1,0)),
        List((2,0), (0,1), (1,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (1,0), (0,1)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (2,0), (2,1)),
        List((2,2), (0,1), (1,0)),
        List((2,2), (0,2), (1,2)),
        List((2,2), (0,0), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find no neighborhood vectors for all A locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find no neighborhood vectors for all C locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find no neighborhood vectors for all G locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find no neighborhood vectors for all I locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find no neighborhood vectors for all A locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find no neighborhood vectors for all C locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find no neighborhood vectors for all G locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    it should "find no neighborhood vectors for all I locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer()
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
  }

  /*
      Array('A', 'B', 'C', 'D')
      Array('E', 'E', 'F', 'G')
      Array('E', 'H', 'I', 'J')
      Array('E', 'K', 'I', 'L')

      NW, N, NE, W, E, SW, S, SE
  */
  {
    val row_one = Array('A', 'B', 'C', 'D', 'E')
    val row_two = Array('E', 'E', 'F', 'G', 'A')
    val row_three = Array('E', 'H', 'I', 'J', 'J')
    val row_four = Array('E', 'K', 'I', 'L', 'L')
    val row_five = Array('E', 'M', 'N', 'E', 'C')
    val matrix = new Matrix(4, 4, Array(
        row_one.mkString(""),
        row_two.mkString(""),
        row_three.mkString(""),
        row_four.mkString(""),
        row_five.mkString("")
    ).iterator)

    "A 4x4 Matrix" should List(
        "parse all the input lines and break them into a list of characters, ",
        "ignoring the 5th input row and column."
    ).mkString(new_line_indent) in {
      matrix.grid(0) should equal (row_one.take(4))
      matrix.grid(1) should equal (row_two.take(4))
      matrix.grid(2) should equal (row_three.take(4))
      matrix.grid(3) should equal (row_four.take(4))
    }

    it should "be able to return all locations for the letter A" in {
      val locations = mutable.ArrayBuffer((0,0))
      matrix.locations.get('A').get should equal (locations)
    }

    it should "be able to return all locations for the letter C" in {
      val locations = mutable.ArrayBuffer((0,2))
      matrix.locations.get('C').get should equal (locations)
    }

    it should "be able to return all locations for the letter G" in {
      val locations = mutable.ArrayBuffer((1,3))
      matrix.locations.get('G').get should equal (locations)
    }

    it should "be able to return all locations for the letter I" in {
      val locations = mutable.ArrayBuffer((2,2), (3,2))
      matrix.locations.get('I').get should equal (locations)
    }

    it should "be able to return all locations for the letter E" in {
      val locations = mutable.ArrayBuffer((1,0), (1,1), (2,0), (3,0))
      matrix.locations.get('E').get should equal (locations)
    }

    it should "be able to return all locations for the letter L" in {
      val locations = mutable.ArrayBuffer((3,3))
      matrix.locations.get('L').get should equal (locations)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (0,1), (0,0)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (2,1), (2,0)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (3,1), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (1,1), (1,2)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (1,2), (1,3)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,1), (2,2)),

        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,1), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (3,2), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (1,2), (2,2), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1), (1,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((3,2), (2,2), (1,2), (0,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (1,1), (1,2), (1,3)),

        List((2,0), (2,1), (2,2), (2,3)),

        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,1), (3,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (3,2), (3,1), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (3,3), (2,2)),
        List((0,0), (3,0), (2,0)),
        List((0,0), (3,1), (2,2)),
        List((0,0), (0,3), (0,2)),
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,3), (2,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (3,1), (2,0)),
        List((0,2), (3,2), (2,2)),
        List((0,2), (3,3), (2,0)),
        List((0,2), (0,1), (0,0)),
        List((0,2), (0,3), (0,0)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2)),
        List((0,2), (1,3), (2,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (3,1)),
        List((1,3), (0,3), (3,3)),
        List((1,3), (0,0), (3,1)),
        List((1,3), (1,2), (1,1)),
        List((1,3), (1,0), (1,1)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3)),
        List((1,3), (2,0), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (1,3), (0,0)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (2,3), (2,0)),
        List((2,2), (3,1), (0,0)),
        List((2,2), (3,2), (0,2)),
        List((2,2), (3,3), (0,0)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (2,3), (1,0)),
        List((3,2), (3,1), (3,0)),
        List((3,2), (3,3), (3,0)),
        List((3,2), (0,1), (1,0)),
        List((3,2), (0,2), (1,2)),
        List((3,2), (0,3), (1,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (0,3), (3,2)),
        List((1,0), (0,0), (3,0)),
        List((1,0), (0,1), (3,2)),
        List((1,0), (1,3), (1,2)),
        List((1,0), (1,1), (1,2)),
        List((1,0), (2,3), (3,2)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (0,0), (3,3)),
        List((1,1), (0,1), (3,1)),
        List((1,1), (0,2), (3,3)),
        List((1,1), (1,0), (1,3)),
        List((1,1), (1,2), (1,3)),
        List((1,1), (2,0), (3,3)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,3), (0,2)),
        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,3), (2,2)),
        List((2,0), (2,1), (2,2)),
        List((2,0), (3,3), (0,2)),
        List((2,0), (3,0), (0,0)),
        List((2,0), (3,1), (0,2)),

        List((3,0), (2,3), (1,2)),
        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,3), (3,2)),
        List((3,0), (3,1), (3,2)),
        List((3,0), (0,3), (1,2)),
        List((3,0), (0,0), (1,0)),
        List((3,0), (0,1), (1,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (2,0), (1,1)),
        List((3,3), (3,2), (3,1)),
        List((3,3), (3,0), (3,1)),
        List((3,3), (0,2), (1,1)),
        List((3,3), (0,3), (1,3)),
        List((3,3), (0,0), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (3,3), (2,2), (1,1)),
        List((0,0), (3,0), (2,0), (1,0)),
        List((0,0), (3,1), (2,2), (1,3)),
        List((0,0), (0,3), (0,2), (0,1)),
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,3), (2,2), (3,1)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (3,1), (2,0), (1,3)),
        List((0,2), (3,2), (2,2), (1,2)),
        List((0,2), (3,3), (2,0), (1,1)),
        List((0,2), (0,1), (0,0), (0,3)),
        List((0,2), (0,3), (0,0), (0,1)),
        List((0,2), (1,1), (2,0), (3,3)),
        List((0,2), (1,2), (2,2), (3,2)),
        List((0,2), (1,3), (2,0), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (3,1), (2,0)),
        List((1,3), (0,3), (3,3), (2,3)),
        List((1,3), (0,0), (3,1), (2,2)),
        List((1,3), (1,2), (1,1), (1,0)),
        List((1,3), (1,0), (1,1), (1,2)),
        List((1,3), (2,2), (3,1), (0,0)),
        List((1,3), (2,3), (3,3), (0,3)),
        List((1,3), (2,0), (3,1), (0,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0), (3,3)),
        List((2,2), (1,2), (0,2), (3,2)),
        List((2,2), (1,3), (0,0), (3,1)),
        List((2,2), (2,1), (2,0), (2,3)),
        List((2,2), (2,3), (2,0), (2,1)),
        List((2,2), (3,1), (0,0), (1,3)),
        List((2,2), (3,2), (0,2), (1,2)),
        List((2,2), (3,3), (0,0), (1,1)),

        List((3,2), (2,1), (1,0), (0,3)),
        List((3,2), (2,2), (1,2), (0,2)),
        List((3,2), (2,3), (1,0), (0,1)),
        List((3,2), (3,1), (3,0), (3,3)),
        List((3,2), (3,3), (3,0), (3,1)),
        List((3,2), (0,1), (1,0), (2,3)),
        List((3,2), (0,2), (1,2), (2,2)),
        List((3,2), (0,3), (1,0), (2,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (0,3), (3,2), (2,1)),
        List((1,0), (0,0), (3,0), (2,0)),
        List((1,0), (0,1), (3,2), (2,3)),
        List((1,0), (1,3), (1,2), (1,1)),
        List((1,0), (1,1), (1,2), (1,3)),
        List((1,0), (2,3), (3,2), (0,1)),
        List((1,0), (2,0), (3,0), (0,0)),
        List((1,0), (2,1), (3,2), (0,3)),

        List((1,1), (0,0), (3,3), (2,2)),
        List((1,1), (0,1), (3,1), (2,1)),
        List((1,1), (0,2), (3,3), (2,0)),
        List((1,1), (1,0), (1,3), (1,2)),
        List((1,1), (1,2), (1,3), (1,0)),
        List((1,1), (2,0), (3,3), (0,2)),
        List((1,1), (2,1), (3,1), (0,1)),
        List((1,1), (2,2), (3,3), (0,0)),

        List((2,0), (1,3), (0,2), (3,1)),
        List((2,0), (1,0), (0,0), (3,0)),
        List((2,0), (1,1), (0,2), (3,3)),
        List((2,0), (2,3), (2,2), (2,1)),
        List((2,0), (2,1), (2,2), (2,3)),
        List((2,0), (3,3), (0,2), (1,1)),
        List((2,0), (3,0), (0,0), (1,0)),
        List((2,0), (3,1), (0,2), (1,3)),

        List((3,0), (2,3), (1,2), (0,1)),
        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,3), (3,2), (3,1)),
        List((3,0), (3,1), (3,2), (3,3)),
        List((3,0), (0,3), (1,2), (2,1)),
        List((3,0), (0,0), (1,0), (2,0)),
        List((3,0), (0,1), (1,2), (2,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (2,0), (1,1), (0,2)),
        List((3,3), (3,2), (3,1), (3,0)),
        List((3,3), (3,0), (3,1), (3,2)),
        List((3,3), (0,2), (1,1), (2,0)),
        List((3,3), (0,3), (1,3), (2,3)),
        List((3,3), (0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
  }

  {
    val row_one = Array('A', 'B', 'C', 'D', 'E')
    val row_two = Array('E', 'E', 'F', 'G', 'A')
    val row_three = Array('E', 'H', 'I', 'J', 'J')
    val row_four = Array('E', 'K', 'I', 'L', 'L')
    val row_five = Array('E', 'M', 'N', 'E', 'C')
    val matrix = new Matrix(4, 5, Array(
      row_one.mkString(""),
      row_two.mkString(""),
      row_three.mkString(""),
      row_four.mkString(""),
      row_five.mkString("")
    ).iterator)

    "A 4x5 Matrix" should List(
      "parse all the input lines and break them into a list of characters, ",
      "ignoring the 5th input row"
    ).mkString(new_line_indent) in {
      matrix.grid(0) should equal (row_one.take(5))
      matrix.grid(1) should equal (row_two.take(5))
      matrix.grid(2) should equal (row_three.take(5))
      matrix.grid(3) should equal (row_four.take(5))
    }

    it should "be able to return all locations for the letter A" in {
      val locations = mutable.ArrayBuffer((0,0), (1,4))
      matrix.locations.get('A').get should equal (locations)
    }

    it should "be able to return all locations for the letter C" in {
      val locations = mutable.ArrayBuffer((0,2))
      matrix.locations.get('C').get should equal (locations)
    }

    it should "be able to return all locations for the letter G" in {
      val locations = mutable.ArrayBuffer((1,3))
      matrix.locations.get('G').get should equal (locations)
    }

    it should "be able to return all locations for the letter I" in {
      val locations = mutable.ArrayBuffer((2,2), (3,2))
      matrix.locations.get('I').get should equal (locations)
    }

    it should "be able to return all locations for the letter E" in {
      val locations = mutable.ArrayBuffer((0,4), (1,0), (1,1), (2,0), (3,0))
      matrix.locations.get('E').get should equal (locations)
    }

    it should "be able to return all locations for the letter L" in {
      val locations = mutable.ArrayBuffer((3,3), (3,4))
      matrix.locations.get('L').get should equal (locations)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2)),

        List((1,4), (1,3), (1,2)),
        List((1,4), (2,3), (3,2)),
        List((1,4), (2,4), (3,4))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (0,1), (0,0)),
        List((0,2), (0,3), (0,4)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2)),
        List((0,2), (1,3), (2,4))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (1,3), (0,4)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (2,3), (2,4)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (2,3), (1,4)),
        List((3,2), (3,1), (3,0)),
        List((3,2), (3,3), (3,4))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((0,4), (0,3), (0,2)),
        List((0,4), (1,3), (2,2)),
        List((0,4), (1,4), (2,4)),

        List((1,0), (1,1), (1,2)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (1,2), (1,3)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,1), (2,2)),

        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,1), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (3,2), (3,1)),

        List((3,4), (2,3), (1,2)),
        List((3,4), (2,4), (1,4)),
        List((3,4), (3,3), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D', 'E')
       ('E', 'E', 'F', 'G', 'A')
       ('E', 'H', 'I', 'J', 'J')
       ('E', 'K', 'I', 'L', 'L')

       NW, N, NE, W, E, SW, S, SE
   */

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3)),

        List((1,4), (1,3), (1,2), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (1,2), (2,2), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1), (1,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((3,2), (2,2), (1,2), (0,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((0,4), (0,3), (0,2), (0,1)),
        List((0,4), (1,3), (2,2), (3,1)),
        List((0,4), (1,4), (2,4), (3,4)),

        List((1,0), (1,1), (1,2), (1,3)),

        List((1,1), (1,2), (1,3), (1,4)),

        List((2,0), (2,1), (2,2), (2,3)),

        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,1), (3,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (3,2), (3,1), (3,0)),

        List((3,4), (2,3), (1,2), (0,1)),
        List((3,4), (2,4), (1,4), (0,4)),
        List((3,4), (3,3), (3,2), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D', 'E')
       ('E', 'E', 'F', 'G', 'A')
       ('E', 'H', 'I', 'J', 'J')
       ('E', 'K', 'I', 'L', 'L')

       NW, N, NE, W, E, SW, S, SE
   */
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (3,4), (2,3)),
        List((0,0), (3,0), (2,0)),
        List((0,0), (3,1), (2,2)),
        List((0,0), (0,4), (0,3)),
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,4), (2,3)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2)),

        List((1,4), (0,3), (3,2)),
        List((1,4), (0,4), (3,4)),
        List((1,4), (0,0), (3,1)),
        List((1,4), (1,3), (1,2)),
        List((1,4), (1,0), (1,1)),
        List((1,4), (2,3), (3,2)),
        List((1,4), (2,4), (3,4)),
        List((1,4), (2,0), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (3,1), (2,0)),
        List((0,2), (3,2), (2,2)),
        List((0,2), (3,3), (2,4)),
        List((0,2), (0,1), (0,0)),
        List((0,2), (0,3), (0,4)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2)),
        List((0,2), (1,3), (2,4))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (3,1)),
        List((1,3), (0,3), (3,3)),
        List((1,3), (0,4), (3,0)),
        List((1,3), (1,2), (1,1)),
        List((1,3), (1,4), (1,0)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3)),
        List((1,3), (2,4), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (1,3), (0,4)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (2,3), (2,4)),
        List((2,2), (3,1), (0,0)),
        List((2,2), (3,2), (0,2)),
        List((2,2), (3,3), (0,4)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (2,3), (1,4)),
        List((3,2), (3,1), (3,0)),
        List((3,2), (3,3), (3,4)),
        List((3,2), (0,1), (1,0)),
        List((3,2), (0,2), (1,2)),
        List((3,2), (0,3), (1,4))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((0,4), (3,3), (2,2)),
        List((0,4), (3,4), (2,4)),
        List((0,4), (3,0), (2,1)),
        List((0,4), (0,3), (0,2)),
        List((0,4), (0,0), (0,1)),
        List((0,4), (1,3), (2,2)),
        List((0,4), (1,4), (2,4)),
        List((0,4), (1,0), (2,1)),

        List((1,0), (0,4), (3,3)),
        List((1,0), (0,0), (3,0)),
        List((1,0), (0,1), (3,2)),
        List((1,0), (1,4), (1,3)),
        List((1,0), (1,1), (1,2)),
        List((1,0), (2,4), (3,3)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (0,0), (3,4)),
        List((1,1), (0,1), (3,1)),
        List((1,1), (0,2), (3,3)),
        List((1,1), (1,0), (1,4)),
        List((1,1), (1,2), (1,3)),
        List((1,1), (2,0), (3,4)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,4), (0,3)),
        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,4), (2,3)),
        List((2,0), (2,1), (2,2)),
        List((2,0), (3,4), (0,3)),
        List((2,0), (3,0), (0,0)),
        List((2,0), (3,1), (0,2)),

        List((3,0), (2,4), (1,3)),
        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,4), (3,3)),
        List((3,0), (3,1), (3,2)),
        List((3,0), (0,4), (1,3)),
        List((3,0), (0,0), (1,0)),
        List((3,0), (0,1), (1,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (2,4), (1,0)),
        List((3,3), (3,2), (3,1)),
        List((3,3), (3,4), (3,0)),
        List((3,3), (0,2), (1,1)),
        List((3,3), (0,3), (1,3)),
        List((3,3), (0,4), (1,0)),

        List((3,4), (2,3), (1,2)),
        List((3,4), (2,4), (1,4)),
        List((3,4), (2,0), (1,1)),
        List((3,4), (3,3), (3,2)),
        List((3,4), (3,0), (3,1)),
        List((3,4), (0,3), (1,2)),
        List((3,4), (0,4), (1,4)),
        List((3,4), (0,0), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D', 'E')
       ('E', 'E', 'F', 'G', 'A')
       ('E', 'H', 'I', 'J', 'J')
       ('E', 'K', 'I', 'L', 'L')

       NW, N, NE, W, E, SW, S, SE
   */
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (3,4), (2,3), (1,2)),
        List((0,0), (3,0), (2,0), (1,0)),
        List((0,0), (3,1), (2,2), (1,3)),
        List((0,0), (0,4), (0,3), (0,2)),
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,4), (2,3), (3,2)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3)),

        List((1,4), (0,3), (3,2), (2,1)),
        List((1,4), (0,4), (3,4), (2,4)),
        List((1,4), (0,0), (3,1), (2,2)),
        List((1,4), (1,3), (1,2), (1,1)),
        List((1,4), (1,0), (1,1), (1,2)),
        List((1,4), (2,3), (3,2), (0,1)),
        List((1,4), (2,4), (3,4), (0,4)),
        List((1,4), (2,0), (3,1), (0,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (3,1), (2,0), (1,4)),
        List((0,2), (3,2), (2,2), (1,2)),
        List((0,2), (3,3), (2,4), (1,0)),
        List((0,2), (0,1), (0,0), (0,4)),
        List((0,2), (0,3), (0,4), (0,0)),
        List((0,2), (1,1), (2,0), (3,4)),
        List((0,2), (1,2), (2,2), (3,2)),
        List((0,2), (1,3), (2,4), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (3,1), (2,0)),
        List((1,3), (0,3), (3,3), (2,3)),
        List((1,3), (0,4), (3,0), (2,1)),
        List((1,3), (1,2), (1,1), (1,0)),
        List((1,3), (1,4), (1,0), (1,1)),
        List((1,3), (2,2), (3,1), (0,0)),
        List((1,3), (2,3), (3,3), (0,3)),
        List((1,3), (2,4), (3,0), (0,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0), (3,4)),
        List((2,2), (1,2), (0,2), (3,2)),
        List((2,2), (1,3), (0,4), (3,0)),
        List((2,2), (2,1), (2,0), (2,4)),
        List((2,2), (2,3), (2,4), (2,0)),
        List((2,2), (3,1), (0,0), (1,4)),
        List((2,2), (3,2), (0,2), (1,2)),
        List((2,2), (3,3), (0,4), (1,0)),

        List((3,2), (2,1), (1,0), (0,4)),
        List((3,2), (2,2), (1,2), (0,2)),
        List((3,2), (2,3), (1,4), (0,0)),
        List((3,2), (3,1), (3,0), (3,4)),
        List((3,2), (3,3), (3,4), (3,0)),
        List((3,2), (0,1), (1,0), (2,4)),
        List((3,2), (0,2), (1,2), (2,2)),
        List((3,2), (0,3), (1,4), (2,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((0,4), (3,3), (2,2), (1,1)),
        List((0,4), (3,4), (2,4), (1,4)),
        List((0,4), (3,0), (2,1), (1,2)),
        List((0,4), (0,3), (0,2), (0,1)),
        List((0,4), (0,0), (0,1), (0,2)),
        List((0,4), (1,3), (2,2), (3,1)),
        List((0,4), (1,4), (2,4), (3,4)),
        List((0,4), (1,0), (2,1), (3,2)),

        List((1,0), (0,4), (3,3), (2,2)),
        List((1,0), (0,0), (3,0), (2,0)),
        List((1,0), (0,1), (3,2), (2,3)),
        List((1,0), (1,4), (1,3), (1,2)),
        List((1,0), (1,1), (1,2), (1,3)),
        List((1,0), (2,4), (3,3), (0,2)),
        List((1,0), (2,0), (3,0), (0,0)),
        List((1,0), (2,1), (3,2), (0,3)),

        List((1,1), (0,0), (3,4), (2,3)),
        List((1,1), (0,1), (3,1), (2,1)),
        List((1,1), (0,2), (3,3), (2,4)),
        List((1,1), (1,0), (1,4), (1,3)),
        List((1,1), (1,2), (1,3), (1,4)),
        List((1,1), (2,0), (3,4), (0,3)),
        List((1,1), (2,1), (3,1), (0,1)),
        List((1,1), (2,2), (3,3), (0,4)),

        List((2,0), (1,4), (0,3), (3,2)),
        List((2,0), (1,0), (0,0), (3,0)),
        List((2,0), (1,1), (0,2), (3,3)),
        List((2,0), (2,4), (2,3), (2,2)),
        List((2,0), (2,1), (2,2), (2,3)),
        List((2,0), (3,4), (0,3), (1,2)),
        List((2,0), (3,0), (0,0), (1,0)),
        List((2,0), (3,1), (0,2), (1,3)),

        List((3,0), (2,4), (1,3), (0,2)),
        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,4), (3,3), (3,2)),
        List((3,0), (3,1), (3,2), (3,3)),
        List((3,0), (0,4), (1,3), (2,2)),
        List((3,0), (0,0), (1,0), (2,0)),
        List((3,0), (0,1), (1,2), (2,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (2,4), (1,0), (0,1)),
        List((3,3), (3,2), (3,1), (3,0)),
        List((3,3), (3,4), (3,0), (3,1)),
        List((3,3), (0,2), (1,1), (2,0)),
        List((3,3), (0,3), (1,3), (2,3)),
        List((3,3), (0,4), (1,0), (2,1)),

        List((3,4), (2,3), (1,2), (0,1)),
        List((3,4), (2,4), (1,4), (0,4)),
        List((3,4), (2,0), (1,1), (0,2)),
        List((3,4), (3,3), (3,2), (3,1)),
        List((3,4), (3,0), (3,1), (3,2)),
        List((3,4), (0,3), (1,2), (2,1)),
        List((3,4), (0,4), (1,4), (2,4)),
        List((3,4), (0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D', 'E')
       ('E', 'E', 'F', 'G', 'A')
       ('E', 'H', 'I', 'J', 'J')
       ('E', 'K', 'I', 'L', 'L')

       NW, N, NE, W, E, SW, S, SE
   */

  }

  {
    val row_one = Array('A', 'B', 'C', 'D', 'E')
    val row_two = Array('E', 'E', 'F', 'G', 'A')
    val row_three = Array('E', 'H', 'I', 'J', 'J')
    val row_four = Array('E', 'K', 'I', 'L', 'L')
    val row_five = Array('E', 'M', 'N', 'E', 'C')
    val matrix = new Matrix(5, 4, Array(
      row_one.mkString(""),
      row_two.mkString(""),
      row_three.mkString(""),
      row_four.mkString(""),
      row_five.mkString("")
    ).iterator)

    "A 5x4 Matrix" should List(
      "parse all the input lines and break them into a list of characters, ",
      "ignoring the 5th input column."
    ).mkString(new_line_indent) in {
      matrix.grid(0) should equal (row_one.take(4))
      matrix.grid(1) should equal (row_two.take(4))
      matrix.grid(2) should equal (row_three.take(4))
      matrix.grid(3) should equal (row_four.take(4))
      matrix.grid(4) should equal (row_five.take(4))
    }

    it should "be able to return all locations for the letter A" in {
      val locations = mutable.ArrayBuffer((0,0))
      matrix.locations.get('A').get should equal (locations)
    }

    it should "be able to return all locations for the letter C" in {
      val locations = mutable.ArrayBuffer((0,2))
      matrix.locations.get('C').get should equal (locations)
    }

    it should "be able to return all locations for the letter G" in {
      val locations = mutable.ArrayBuffer((1,3))
      matrix.locations.get('G').get should equal (locations)
    }

    it should "be able to return all locations for the letter I" in {
      val locations = mutable.ArrayBuffer((2,2), (3,2))
      matrix.locations.get('I').get should equal (locations)
    }

    it should "be able to return all locations for the letter E" in {
      val locations = mutable.ArrayBuffer((1,0), (1,1), (2,0), (3,0), (4,0), (4,3))
      matrix.locations.get('E').get should equal (locations)
    }

    it should "be able to return all locations for the letter L" in {
      val locations = mutable.ArrayBuffer((3,3))
      matrix.locations.get('L').get should equal (locations)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (0,1), (0,0)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (3,1), (4,0)),
        List((2,2), (3,2), (4,2)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (3,1), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    it should "find all neighborhood vectors for all E locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (1,1), (1,2)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (1,2), (1,3)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,1), (2,2)),
        List((2,0), (3,0), (4,0)),
        List((2,0), (3,1), (4,2)),

        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,1), (3,2)),

        List((4,0), (3,0), (2,0)),
        List((4,0), (3,1), (2,2)),
        List((4,0), (4,1), (4,2)),

        List((4,3), (3,2), (2,1)),
        List((4,3), (3,3), (2,3)),
        List((4,3), (4,2), (4,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, no wrap" in {
      val (length, wrap_mode) = (3, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (3,2), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D')
       ('E', 'E', 'F', 'G')
       ('E', 'H', 'I', 'J')
       ('E', 'K', 'I', 'L')
       ('E', 'M', 'N', 'E')

       NW, N, NE, W, E, SW, S, SE
   */

    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (1,2), (2,2), (3,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (1,2), (1,1), (1,0)),
        List((1,3), (2,2), (3,1), (4,0)),
        List((1,3), (2,3), (3,3), (4,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((3,2), (2,2), (1,2), (0,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (1,1), (1,2), (1,3)),
        List((1,0), (2,0), (3,0), (4,0)),
        List((1,0), (2,1), (3,2), (4,3)),

        List((1,1), (2,1), (3,1), (4,1)),

        List((2,0), (2,1), (2,2), (2,3)),

        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,1), (3,2), (3,3)),

        List((4,0), (3,0), (2,0), (1,0)),
        List((4,0), (3,1), (2,2), (1,3)),
        List((4,0), (4,1), (4,2), (4,3)),

        List((4,3), (3,2), (2,1), (1,0)),
        List((4,3), (3,3), (2,3), (1,3)),
        List((4,3), (4,2), (4,1), (4,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, no wrap" in {
      val (length, wrap_mode) = (4, false)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (3,2), (3,1), (3,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D')
       ('E', 'E', 'F', 'G')
       ('E', 'H', 'I', 'J')
       ('E', 'K', 'I', 'L')
       ('E', 'M', 'N', 'E')

       NW, N, NE, W, E, SW, S, SE
   */


    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (4,3), (3,2)),
        List((0,0), (4,0), (3,0)),
        List((0,0), (4,1), (3,2)),
        List((0,0), (0,3), (0,2)),
        List((0,0), (0,1), (0,2)),
        List((0,0), (1,3), (2,2)),
        List((0,0), (1,0), (2,0)),
        List((0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (4,1), (3,0)),
        List((0,2), (4,2), (3,2)),
        List((0,2), (4,3), (3,0)),
        List((0,2), (0,1), (0,0)),
        List((0,2), (0,3), (0,0)),
        List((0,2), (1,1), (2,0)),
        List((0,2), (1,2), (2,2)),
        List((0,2), (1,3), (2,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (4,1)),
        List((1,3), (0,3), (4,3)),
        List((1,3), (0,0), (4,1)),
        List((1,3), (1,2), (1,1)),
        List((1,3), (1,0), (1,1)),
        List((1,3), (2,2), (3,1)),
        List((1,3), (2,3), (3,3)),
        List((1,3), (2,0), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0)),
        List((2,2), (1,2), (0,2)),
        List((2,2), (1,3), (0,0)),
        List((2,2), (2,1), (2,0)),
        List((2,2), (2,3), (2,0)),
        List((2,2), (3,1), (4,0)),
        List((2,2), (3,2), (4,2)),
        List((2,2), (3,3), (4,0)),

        List((3,2), (2,1), (1,0)),
        List((3,2), (2,2), (1,2)),
        List((3,2), (2,3), (1,0)),
        List((3,2), (3,1), (3,0)),
        List((3,2), (3,3), (3,0)),
        List((3,2), (4,1), (0,0)),
        List((3,2), (4,2), (0,2)),
        List((3,2), (4,3), (0,0))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (0,3), (4,2)),
        List((1,0), (0,0), (4,0)),
        List((1,0), (0,1), (4,2)),
        List((1,0), (1,3), (1,2)),
        List((1,0), (1,1), (1,2)),
        List((1,0), (2,3), (3,2)),
        List((1,0), (2,0), (3,0)),
        List((1,0), (2,1), (3,2)),

        List((1,1), (0,0), (4,3)),
        List((1,1), (0,1), (4,1)),
        List((1,1), (0,2), (4,3)),
        List((1,1), (1,0), (1,3)),
        List((1,1), (1,2), (1,3)),
        List((1,1), (2,0), (3,3)),
        List((1,1), (2,1), (3,1)),
        List((1,1), (2,2), (3,3)),

        List((2,0), (1,3), (0,2)),
        List((2,0), (1,0), (0,0)),
        List((2,0), (1,1), (0,2)),
        List((2,0), (2,3), (2,2)),
        List((2,0), (2,1), (2,2)),
        List((2,0), (3,3), (4,2)),
        List((2,0), (3,0), (4,0)),
        List((2,0), (3,1), (4,2)),

        List((3,0), (2,3), (1,2)),
        List((3,0), (2,0), (1,0)),
        List((3,0), (2,1), (1,2)),
        List((3,0), (3,3), (3,2)),
        List((3,0), (3,1), (3,2)),
        List((3,0), (4,3), (0,2)),
        List((3,0), (4,0), (0,0)),
        List((3,0), (4,1), (0,2)),

        List((4,0), (3,3), (2,2)),
        List((4,0), (3,0), (2,0)),
        List((4,0), (3,1), (2,2)),
        List((4,0), (4,3), (4,2)),
        List((4,0), (4,1), (4,2)),
        List((4,0), (0,3), (1,2)),
        List((4,0), (0,0), (1,0)),
        List((4,0), (0,1), (1,2)),

        List((4,3), (3,2), (2,1)),
        List((4,3), (3,3), (2,3)),
        List((4,3), (3,0), (2,1)),
        List((4,3), (4,2), (4,1)),
        List((4,3), (4,0), (4,1)),
        List((4,3), (0,2), (1,1)),
        List((4,3), (0,3), (1,3)),
        List((4,3), (0,0), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 3, wrap" in {
      val (length, wrap_mode) = (3, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1)),
        List((3,3), (2,3), (1,3)),
        List((3,3), (2,0), (1,1)),
        List((3,3), (3,2), (3,1)),
        List((3,3), (3,0), (3,1)),
        List((3,3), (4,2), (0,1)),
        List((3,3), (4,3), (0,3)),
        List((3,3), (4,0), (0,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D')
       ('E', 'E', 'F', 'G')
       ('E', 'H', 'I', 'J')
       ('E', 'K', 'I', 'L')
       ('E', 'M', 'N', 'E')

       NW, N, NE, W, E, SW, S, SE
   */


    ////////////////////////////////////////////////////////////////////////////////////////////////////

    it should "find all neighborhood vectors for all A locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('A').get
      val vectors = mutable.ArrayBuffer(
        List((0,0), (4,3), (3,2), (2,1)),
        List((0,0), (4,0), (3,0), (2,0)),
        List((0,0), (4,1), (3,2), (2,3)),
        List((0,0), (0,3), (0,2), (0,1)),
        List((0,0), (0,1), (0,2), (0,3)),
        List((0,0), (1,3), (2,2), (3,1)),
        List((0,0), (1,0), (2,0), (3,0)),
        List((0,0), (1,1), (2,2), (3,3))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all C locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('C').get
      val vectors = mutable.ArrayBuffer(
        List((0,2), (4,1), (3,0), (2,3)),
        List((0,2), (4,2), (3,2), (2,2)),
        List((0,2), (4,3), (3,0), (2,1)),
        List((0,2), (0,1), (0,0), (0,3)),
        List((0,2), (0,3), (0,0), (0,1)),
        List((0,2), (1,1), (2,0), (3,3)),
        List((0,2), (1,2), (2,2), (3,2)),
        List((0,2), (1,3), (2,0), (3,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all G locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('G').get
      val vectors = mutable.ArrayBuffer(
        List((1,3), (0,2), (4,1), (3,0)),
        List((1,3), (0,3), (4,3), (3,3)),
        List((1,3), (0,0), (4,1), (3,2)),
        List((1,3), (1,2), (1,1), (1,0)),
        List((1,3), (1,0), (1,1), (1,2)),
        List((1,3), (2,2), (3,1), (4,0)),
        List((1,3), (2,3), (3,3), (4,3)),
        List((1,3), (2,0), (3,1), (4,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all I locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('I').get
      val vectors = mutable.ArrayBuffer(
        List((2,2), (1,1), (0,0), (4,3)),
        List((2,2), (1,2), (0,2), (4,2)),
        List((2,2), (1,3), (0,0), (4,1)),
        List((2,2), (2,1), (2,0), (2,3)),
        List((2,2), (2,3), (2,0), (2,1)),
        List((2,2), (3,1), (4,0), (0,3)),
        List((2,2), (3,2), (4,2), (0,2)),
        List((2,2), (3,3), (4,0), (0,1)),

        List((3,2), (2,1), (1,0), (0,3)),
        List((3,2), (2,2), (1,2), (0,2)),
        List((3,2), (2,3), (1,0), (0,1)),
        List((3,2), (3,1), (3,0), (3,3)),
        List((3,2), (3,3), (3,0), (3,1)),
        List((3,2), (4,1), (0,0), (1,3)),
        List((3,2), (4,2), (0,2), (1,2)),
        List((3,2), (4,3), (0,0), (1,1))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all E locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('E').get
      val vectors = mutable.ArrayBuffer(
        List((1,0), (0,3), (4,2), (3,1)),
        List((1,0), (0,0), (4,0), (3,0)),
        List((1,0), (0,1), (4,2), (3,3)),
        List((1,0), (1,3), (1,2), (1,1)),
        List((1,0), (1,1), (1,2), (1,3)),
        List((1,0), (2,3), (3,2), (4,1)),
        List((1,0), (2,0), (3,0), (4,0)),
        List((1,0), (2,1), (3,2), (4,3)),

        List((1,1), (0,0), (4,3), (3,2)),
        List((1,1), (0,1), (4,1), (3,1)),
        List((1,1), (0,2), (4,3), (3,0)),
        List((1,1), (1,0), (1,3), (1,2)),
        List((1,1), (1,2), (1,3), (1,0)),
        List((1,1), (2,0), (3,3), (4,2)),
        List((1,1), (2,1), (3,1), (4,1)),
        List((1,1), (2,2), (3,3), (4,0)),

        List((2,0), (1,3), (0,2), (4,1)),
        List((2,0), (1,0), (0,0), (4,0)),
        List((2,0), (1,1), (0,2), (4,3)),
        List((2,0), (2,3), (2,2), (2,1)),
        List((2,0), (2,1), (2,2), (2,3)),
        List((2,0), (3,3), (4,2), (0,1)),
        List((2,0), (3,0), (4,0), (0,0)),
        List((2,0), (3,1), (4,2), (0,3)),

        List((3,0), (2,3), (1,2), (0,1)),
        List((3,0), (2,0), (1,0), (0,0)),
        List((3,0), (2,1), (1,2), (0,3)),
        List((3,0), (3,3), (3,2), (3,1)),
        List((3,0), (3,1), (3,2), (3,3)),
        List((3,0), (4,3), (0,2), (1,1)),
        List((3,0), (4,0), (0,0), (1,0)),
        List((3,0), (4,1), (0,2), (1,3)),

        List((4,0), (3,3), (2,2), (1,1)),
        List((4,0), (3,0), (2,0), (1,0)),
        List((4,0), (3,1), (2,2), (1,3)),
        List((4,0), (4,3), (4,2), (4,1)),
        List((4,0), (4,1), (4,2), (4,3)),
        List((4,0), (0,3), (1,2), (2,1)),
        List((4,0), (0,0), (1,0), (2,0)),
        List((4,0), (0,1), (1,2), (2,3)),

        List((4,3), (3,2), (2,1), (1,0)),
        List((4,3), (3,3), (2,3), (1,3)),
        List((4,3), (3,0), (2,1), (1,2)),
        List((4,3), (4,2), (4,1), (4,0)),
        List((4,3), (4,0), (4,1), (4,2)),
        List((4,3), (0,2), (1,1), (2,0)),
        List((4,3), (0,3), (1,3), (2,3)),
        List((4,3), (0,0), (1,1), (2,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }

    it should "find all neighborhood vectors for all L locations of size 4, wrap" in {
      val (length, wrap_mode) = (4, true)
      val locations = matrix.locations.get('L').get
      val vectors = mutable.ArrayBuffer(
        List((3,3), (2,2), (1,1), (0,0)),
        List((3,3), (2,3), (1,3), (0,3)),
        List((3,3), (2,0), (1,1), (0,2)),
        List((3,3), (3,2), (3,1), (3,0)),
        List((3,3), (3,0), (3,1), (3,2)),
        List((3,3), (4,2), (0,1), (1,0)),
        List((3,3), (4,3), (0,3), (1,3)),
        List((3,3), (4,0), (0,1), (1,2))
      )
      matrix.get_neighborhood_vectors(locations, length, wrap_mode) should equal (vectors)
    }
    /*
       ('A', 'B', 'C', 'D')
       ('E', 'E', 'F', 'G')
       ('E', 'H', 'I', 'J')
       ('E', 'K', 'I', 'L')
       ('E', 'M', 'N', 'E')

       NW, N, NE, W, E, SW, S, SE
   */
  }

}