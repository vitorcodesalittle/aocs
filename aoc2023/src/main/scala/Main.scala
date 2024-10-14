import scala.collection.JavaConverters._
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import java.util.regex.Pattern

def solve1Part1(inputFilePath: String): Long =
  Files
    .readAllLines(Path.of(inputFilePath))
    .asScala
    .map(str => {
      val trimmed = str
        .dropWhile(!_.isDigit)
        .reverse
        .dropWhile(!_.isDigit)
        .toList
      (trimmed.last.toString + trimmed.head.toString).toLong
    })
    .sum

def solve1Part2(inputFilePath: String): Long =
  // 0 to 9 map
  val map = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9',
    "0" -> '0',
    "1" -> '1',
    "2" -> '2',
    "3" -> '3',
    "4" -> '4',
    "5" -> '5',
    "6" -> '6',
    "7" -> '7',
    "8" -> '8',
    "9" -> '9'
  )
  Files
    .readAllLines(Path.of(inputFilePath))
    .asScala
    .map(str => {
      var strCopy = str.substring(
        (0 to str.length - 1)
          .find(index =>
            map.keys.exists(key => str.substring(index).startsWith(key))
          )
          .get
      )
      val digit1 = map(map.keys.find(key => strCopy.startsWith(key)).get)
      strCopy = str.substring(
        0,
        ((0 to str.length() - 1).reverse)
          .find(index => {
            // println(s"Looking at ${str.substring(0, index + 1)}")
            map.keys.exists(key => {
              // println(key + " " + str.substring(0, index + 1).endsWith(key))
              str.substring(0, index + 1).endsWith(key)
            })
          })
          .get + 1
      )
      val digit2 = map(map.keys.find(strCopy.endsWith(_)).get)
      (digit1.toString + digit2.toString).toLong
    })
    .sum

// Get the sum of the IDS of the impossible games
// Only 12 red cubes, 13 green cubes and 14 blue cubes
def solve2Part1(inputFIlePath: Path): Long =
  Files
    .readAllLines(inputFIlePath)
    .asScala
    .map(str => {
      val gameIdAndRounds = str.split(":")
      val gameId = gameIdAndRounds.head
        .split(" ")
        .last
        .toLong
      val good = gameIdAndRounds(1)
        .split(";")
        .forall(s =>
          s.split(",")
            .map(_.trim)
            .forall(s => {
              val arr = s.split(" ");
              val number = arr(0);
              val color = arr(1)
              color match {
                case "blue"  => number.toLong <= 14
                case "red"   => number.toLong <= 12
                case "green" => number.toLong <= 13
                case _       => false
              }
            })
        )
      if good then gameId
      else
        println(s"IMPOSSIBLE GAME $gameId")
        0
    })
    .sum
def max(a: Int, b: Int) =
  if a > b then a else b
// Get the minimum number of cubes of each color that would make the game possible
// Over game ids, return the sum of the power of the minimum number of cubes from each color.
// The power is just multiplying the numbers
def solve2Part2(inputFIlePath: Path): Long =
  Files
    .readAllLines(inputFIlePath)
    .asScala
    .map(str => {
      val gameIdAndRounds = str.split(":")
      val gameId = gameIdAndRounds.head
        .split(" ")
        .last
        .toLong
      val minColors = gameIdAndRounds(1)
        .split(";")
        .foldRight((0, 0, 0))((s, acc) =>
          s.split(",")
            .map(_.trim)
            .foldRight(acc)((s, acc) => {
              val arr = s.split(" ");
              val number = arr(0).toInt;
              val color = arr(1)
              color match {
                case "red"   => (max(number, acc(0)), acc(1), acc(2))
                case "green" => (acc(0), max(number, acc(1)), acc(2))
                case "blue"  => (acc(0), acc(1), max(number, acc(2)))
                case _       => sys.exit(1)
              }
            })
        )
      println(s"Game id $gameId has answer $minColors")
      val power = minColors(0) * minColors(1) * minColors(2)
      println(power)
      power
    })
    .sum

trait Point {
  def line: Int
  def start: Int
  def end: Int

}
case class NumberPoint(line: Int, start: Int, end: Int, value: Int)
    extends Point
case class SpecialPoint(line: Int, start: Int, end: Int, value: Char)
    extends Point

def checkTouches = (specials: List[SpecialPoint]) =>
  (line: Int, col: Int) => {
    specials.exists(special => {
      val (targetLine, targetColumn) = (special.line, special.start)
      val touchs = for
        i <- -1 to 1
        j <- -1 to 1
        if i != 0 || j != 0
      yield line + i == targetLine && col + j == targetColumn
      touchs.exists(_ == true)
    })
  }

def checkIsGears = (numbers: List[NumberPoint]) =>
  (line: Int, col: Int) => {
    numbers
      .filter(number => {
        (number.start to number.end - 1).exists(col2 =>
          val (targetLine, targetColumn) = (number.line, col)
          val touchs = for
            i <- -1 to 1
            j <- -1 to 1
            if i != 0 || j != 0
          yield line + i == targetLine && col2 + j == targetColumn
          touchs.exists(_ == true)
        )
      })
  }

def solve3GetNumbersAndSpecials(
    inputFilePath: String
): (List[NumberPoint], List[SpecialPoint]) =
  val lines =
    Files.readAllLines(Path.of(inputFilePath)).asScala.toList.zipWithIndex
  val patternInt = Pattern.compile("[0-9]+")
  val patternSpecial = Pattern.compile("[^0-9\\.]")
  val numbers = lines.foldLeft(List[NumberPoint]())((acc, lineIndex) => {
    val (line, index) = lineIndex
    val matcherInt = patternInt.matcher(line.dropRight(0))
    acc ++ matcherInt.results
      .map(result => {
        NumberPoint(
          index,
          result.start(),
          result.end(),
          result.group().toInt
        )
      })
      .toList
      .asScala
  })
  val specials = lines.foldLeft(List[SpecialPoint]())((acc, lineIndex) => {
    val (line, index) = lineIndex
    val matcherChar = patternSpecial.matcher(line.dropRight(0))
    acc ++ matcherChar.results
      .map(result => {
        SpecialPoint(index, result.start(), result.end(), result.group().head)
      })
      .toList
      .asScala
  })
  (numbers, specials)

def solve3Part1(inputFilePath: String): Long =
  val (numbers, specials) = solve3GetNumbersAndSpecials(inputFilePath)
  val validNumbers = numbers.filter(number => {
    number match {
      case NumberPoint(line, start, end, value) => {
        (start to end - 1).exists(checkTouches(specials)(line, _))
      }
    }
  })
  validNumbers.foldRight(0)((entity, acc) => entity.value + acc)

def solve3Part2(inputFilePath: String): Long =
  val (numbers, specials) = solve3GetNumbersAndSpecials(inputFilePath)
  specials
    .filter(_.value == '*')
    .map(special => {
      checkIsGears(numbers)(special.line, special.start)
    })
    .filter(_.size == 2)
    .map(numbers => numbers.map(_.value).foldRight(1)(_ * _))
    .sum()

def getWinningAndMyNumbers(str: String) = {
  val parts = str
    .split(':')
    .drop(1)
    .head
    .split('|')
  def processCars = (str: String) => {
    str.trim
      .split("\\s+")
      .map(_.trim)
      .map(_.toLong)
      .toList
  }
  (processCars(parts.take(1).head), processCars(parts.drop(1).head))
}
def getCardNumberAndWinningAndMyNumbers(
    str: String
) = {
  val parts = str
    .split(':')
    .drop(1)
    .head
    .split('|')
  def processCars = (str: String) => {
    str.trim
      .split("\\s+")
      .map(_.trim)
      .map(_.toLong)
      .toList
  }
  val cardNumber = str
    .split(':')
    .head
    .replace("Card ", "")
    .trim
    .toInt
  (
    cardNumber,
    processCars(parts.take(1).head),
    processCars(parts.drop(1).head)
  )
}
def solve4Part1(inputFilePath: String): Long = {
  val lines = Files.readAllLines(Path.of(inputFilePath)).asScala.toList
  lines
    .map(line => {
      val (winning, my) = getWinningAndMyNumbers(line)
      val contained = my.filter(winning.contains(_)).size
      if contained == 0 then 0
      else 1 << contained - 1
    })
    .sum
}

def solve4Part2(inputFilePath: String): Long = {
  val lines = Files.readAllLines(Path.of(inputFilePath)).asScala.toList
  val map = (1 to lines.size).map(i => (i, 1)).toMap
  val map2 = lines
    .foldLeft(map)((acc, line) => {
      val (cardNumber, winning, my) = getCardNumberAndWinningAndMyNumbers(line)
      val n = my.filter(winning.contains(_)).size 
      val entries =
        for (i <- cardNumber + 1 to cardNumber + n)
          yield (i, acc.getOrElse(i, 0) + acc.get(cardNumber).get)
       acc ++ entries
    })
  map2
    .values
    .sum
}

@main def hello(args: String*): Unit =
  val out1 = solve4Part2("./inputs/day4_input.txt")
  assert(out1 == 30, s"example failed! wanted 30 but got $out1")
  println(solve4Part2("./inputs/day4_input2.txt"))
