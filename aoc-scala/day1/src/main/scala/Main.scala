import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import common.file.readFileLines

@main def run(filePath: String): Unit = {
  println(processFile(filePath).sum)
}

def processFile(filePath: String) = {
  readFileLines(filePath)
    .map(extractFirstLast)
    .map(firstLastToInt)
}

/**
  * Converts string to a tuple of the (first, last) integers
  *
  * @param string containing digits, word representation of numbers and other characters
  * @return a tuple of the first and last numbers in the string
  */
def extractFirstLast(string: String): (Int, Int) = {
  val firstNumber: Regex =
    "(one|two|three|four|five|six|seven|eight|nine|[0-9])".r
  val lastNumber: Regex =
    "(?:.*)(one|two|three|four|five|six|seven|eight|nine|[0-9])".r

  (getDigitMatching(firstNumber)(string), getDigitMatching(lastNumber)(string))
}

/**
  * Converts the first match group to an Int
  *
  * @param regex expression to match the number
  * @return Int representation of the matched number
  */
def getDigitMatching(regex: Regex) = (input: String) => regex.findFirstMatchIn(input) match {
  case Some(value) => stringToInt(value.group(1))
  case None => 0
}

/**
  * Maps a String to an Int
  * 
  * @param string word representation of a number 1-9 or the number itself
  * @return the equivalent Integer value of the represented number
  */
def stringToInt(string: String): Int = string match {
  case "one"   => 1
  case "two"   => 2
  case "three" => 3
  case "four"  => 4
  case "five"  => 5
  case "six"   => 6
  case "seven" => 7
  case "eight" => 8
  case "nine"  => 9
  case _       => string.toInt
}

/**
  * Maps a tuple of (Int, Int) to an Int
  * 
  * @param tup where the number in slot 0 is in the 10s and slot 1 is the 1s
  * @return an Int
  */
def firstLastToInt(tup: (Int, Int)): Int = tup match {
  case (a, b) => (a * 10) + b
}
