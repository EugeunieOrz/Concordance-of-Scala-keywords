package concordance

import scala.io.Source
import scala.io.StdIn
import scala.collection.mutable.Map
import scala.collection.immutable.ListMap
import scala.annotation.tailrec

/**
 * The program "Concordance.scala" analyses a Scala source file and produces a concordance
 * of the Scala keywords used in a file.
 * The following functional programming patterns were implemented in the program:
 * - currying
 * - partially applied functions
 * - @tailrec annotation
 * - pattern matching and deconstruction
 * - value definition
 * - for comprehension
 * - map function
 */

object Concordance extends App {

  /**
   * Asked the user to supply a command line argument containing the file name to be used.
   * If the user types wrong path or doesn't type anything, the file in "resources" folder is used.
   */
  println("Please enter a name of file you'd like to read from: ")
  // value definition
  val filename = {
    val name = scala.io.StdIn.readLine()
    if(name.length != 0) name
    else "file.txt"
  }

  // Created a list of Scala reserved words using "cons" syntax.
  val keywords = "abstract" :: "case" :: "catch" :: "class" :: "def" :: "do" :: "else" ::
            "extends" :: "false" :: "final" :: "finally" :: "for" :: "forSome" :: "if" ::
            "implicit" :: "import" :: "lazy" :: "macro" :: "match" :: "new" :: "null" ::
            "object" :: "override" :: "package" :: "private" :: "protected" :: "return" ::
            "sealed" :: "super" :: "this" :: "throw" :: "trait" :: "try" :: "true" ::
            "type" :: "val" :: "var" :: "while" :: "with" :: "yield" :: "-" :: ":" ::
            "=" :: "=>" :: "<-" :: "<:" :: "<%" :: ">:" :: "#" :: "@" :: Nil

  // Created an empty Map of strings as keys and lists of integers as values
  val m = Map[String, List[Int]]()

  /**
   * Implemented a for comprehension to do the following:
   *   - fill the empty map with keywords as keys and empty list of ints for each keyword as values
   *     using map function
   *   - traverse list of keywords
   *   - get lines with index from the file
   *   - check if every line from the file contains a Scala keyword
   *   - if it does, then add line number to the list of ints in a map.
   */
  for {
    _ <- keywords.map(k => m += k -> List[Int]())
    k <- keywords
    (line,index) <- Source.fromResource(filename)("UTF-8").getLines().zipWithIndex
    if(line.contains(k))
  } yield {
    m(k) = m(k) :+ (index + 1)
  }

  // Sorted the map by keyword using scala.collection.immutable.ListMap
  def sortMapByKey(m: Map[String, List[Int]]) = ListMap(m.toSeq.sortBy(_._1):_*)

  /**
   * Checked for ranges of consecutive numbers in a list of line numbers of the Map m
   * by defining the checkForRange method which traverses the list in orderly manner and checks
   * for ranges of consecutive numbers throughout the list.
   * In first case the list is matched against two possible cases:
   * The base case: the list is Nil, and an empty list matches Nil as it has zero elements
   * The general case: the non-empty list is matched against the two cases:
   *   The base case: the list is deconstructed into range and Nil
   *   The recursive case: the list consists of range of consecutive numbers and
   *   the rest of numbers is checked for ranges again by applying recursion.
   * The checkForRange method uses for the second case the splitList method which splits the list
   * into two parts if an element is less or equal to the next element.
   * The makeRangeString method is called with list of ints returned by the checkForRange method,
   * if the list consists of three elements or more, it's deconstructed into head and last element
   * which are connected by "-".
   */


   // find ranges of consecutive numbers
   def findConsecNums(list: List[Int])(f: (Int, Int) => Boolean): List[List[Int]] = {
     /**
      * @tailrec makes sure that the method will be compiled with tail call optimization
      * that converts the recursive form into a loop
      */
     @tailrec
     def findNums(lst: List[Int], result: List[List[Int]], acc: List[Int]): List[List[Int]] = lst match {
       case Nil => acc.reverse :: result
       case x :: xs => acc match {
         case Nil => findNums(xs, result, x :: acc)
         case y :: ys if(f(x,y)) => findNums(xs, result, x :: acc)
         case _ => findNums(xs, acc.reverse :: result, x :: List())
       }
     }
     val r = findNums(list, List(), List())
     r.reverse
   }

  // concatenate the first and last element of range with "-"
  def makeRangeString(list: List[List[Int]]) = list map { x =>
     if(x.length < 3) x mkString ","
     else x.head + "-" + x.last
  } mkString ","

  /**
   * Defined the limitMaxWidthOutput method which splits into parts long strings of line numbers
   * and applies a stripMargin.
   */
  def limitMaxWidthOutput(list: List[Int]) = {
    // value definition
    val r = findConsecNums(list) _
    val str = makeRangeString(r((x,y) => x - y == 1))
    if(str.length > 5) {
			val strForPrint = str.split(",").grouped(5).map(s => s.mkString(","))
         .foldLeft("") { (s: String, l: String) =>
           s + s"""$l
                   |${"\t\t\t      "}""".stripMargin
         }
      s"""${strForPrint}""".stripMargin
    }  else str
  }

  // print formatted result
  val k = "Keyword"
  val ln = "Line Numbers"
  val count = "Count"

  /**
   * Formatted the final output as follows:
   * left align for column "Keyword" with max-width of 20 characters,
   * left align for column "count" with max-width of 10 characters,
   * left align for column "lines" with max-width of 22 characters.
   */
  val format = "%-20s%-10s%-22s\n"

  // print the headers for the table
  printf(format, k, count, ln)

  /**
   * Print each key and value from the map of keywords and corresponding list of line numbers.
   * Distinct the list of line numbers to remove possible duplicates.
   * The length of list of line numbers is the occurrence of the keyword on each line.
   */
  for ((k,v) <- sortMapByKey(m)) printf(
    format, k, v.distinct.length, limitMaxWidthOutput(v.distinct)
  )
}
