import scala.collection.JavaConverters._

import java.nio.file.Path
import java.util.regex.Pattern

import scala.collection.Searching.InsertionPoint

import scala.collection.mutable
import java.nio.file.Files

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
  map2.values.sum
}

def applyFns(fns: Array[(Long) => Long], a: Long) = {
  var ans = a
  for (fn <- fns) do ans = fn(ans)
  ans
}

def getFn(lines: Seq[String]): Long => Long =
  var fns: Array[(Long) => Long] = lines
    .drop(2)
    .mkString("\n")
    .split("\n\n")
    .map(chunk => {
      val intervalsMapping = chunk
        .split('\n')
        .drop(1)
        .foldLeft(Set[(Long, Long, Long)]())((acc, line) => {
          // <destination range start> <source range start> <range length>
          val l = line.split(' ').map(_.toLong)
          acc + ((l(0), l(1), l(2)))
        });
      def a(num: Long): Long = {
        intervalsMapping
          .find((t) => {
            val (_, sourceStart, rangeLen) = t;
            num >= sourceStart && num <= sourceStart + rangeLen
          })
          .map(t => {
            val (destinationStart, sourceStart, _) = t;
            destinationStart + (num - sourceStart)
          })
          .getOrElse(num)
      }
      a
    })
  applyFns(fns, _)

def solve5Seeds(
    seeds: List[Long],
    f: Long => Long
): Long = {
  seeds.zip(seeds.map(f)).minBy(_._2)._2
}
def solve5Part1(inputFilePath: String): Long = {
  val lines = Files.readAllLines(Path.of(inputFilePath)).asScala.toList
  val seeds =
    lines.head.split(':').tail.head.split(' ').filter(_.nonEmpty).map(_.toLong)
  solve5Seeds(seeds.toList, getFn(lines))
}


def max(a: Long, b: Long) = if a > b then a else b
def min(a: Long, b: Long) = if a < b then a else b

/** Hello
 */
final case class Func5i(val b: Long, val a: Long, val l: Long) extends Ordered[Func5i]:
  val srcStart =  a;
  // Exclusive
  val srcEnd = a + l
  val destineStart = b;
  val destineEnd = b + l

  import scala.math.Ordered.orderingToOrdered

  def apply(x: Long) =
    val result = b + (x - a)
    assert(result < destineEnd, s"invalid apply on $x not belonging to [$srcStart, $srcEnd)")
    assert(result >= destineStart, s"invalid apply on $x not belonging to [$srcStart, $srcEnd)")
    result

  def accepts(x: Long) =
    x >= srcStart && x < srcEnd

  def accepts(x: (Long, Long)): Option[(Long, Long)] =
    val (s, e) = if x._1 <= x._2 then x else x.swap
    if e < srcStart || s >= srcEnd then None
    else Some(max(srcStart, s), min(srcEnd, e))

  def compare(that: Func5i): Int = {
    tuple.compare(that.tuple)
  }
  def tuple: (Long, Long, Long) = {
    (srcStart, destineStart, l)
  }
  override def toString = s"[$srcStart, $srcEnd) -> [$destineStart, $destineEnd)"


case class Func5(val funcs: Set[Func5i]):
  def apply(x: Int): Long =
    funcs.find(f => f.accepts(x)).map(f => f(x)).getOrElse(x)

  def apply(x: (Long, Long)): Set[(Long, Long)] =
    var i = 0
    var result = mutable.Set[(Long, Long)]()
    var (s, e) = if x._1 > x._2 then x.swap else x
    val intervalLen = e - s;
    var cur = s
    funcs.toList.sorted.foreach(func => {
      if s < e then func.accepts((s, e)) match {
        case Some((ss, ee)) => {
          if ss != s then
            result += (s, ss)
          result += (func(ss), func(ee-1)+1)
          s = ee
        }
        case None => {}
      }
    })

    if s < e then result += (s, e)

    assert(result.foldLeft(0L)((cur, v) => cur + (v._2 - v._1)) <= intervalLen, s"Problem detected input interval ${x} < output sum of intervals ${result}")

    Set.from(result)

  def apply(xs: Set[(Long, Long)]): Set[(Long, Long)] = 
    xs.toList.sorted.foldLeft(Set[(Long, Long)]())((acc, v) => {
      val n = apply(v)
      acc ++ n
    })

 

object Func5:
  def fromChunk(chunk: String) =
    Func5(chunk.split('\n').drop(1).map(fline => {
            val nums = fline.split(" ").map(_.toLong)
            Func5i(nums(0), nums(1), nums(2))
          }).sorted.toSet)


def intervalsSumLength(intervals: Set[(Long, Long)]): Long =
  intervals.foldLeft(0L)((cur, interval) => cur + (interval._2 - interval._1))

def locations(intervals: Set[(Long, Long)], fs: List[Func5]): Set[(Long, Long)] =
  var currentIntervalSumLength = intervalsSumLength(intervals)
  fs.foldLeft(intervals.toList.sorted.toSet)((cur, f) => {
    var newIntervalLength = intervalsSumLength(f(cur))
    assert (currentIntervalSumLength == newIntervalLength, s"$f FAILED! Intervals change length $cur ($currentIntervalSumLength) ${f(cur)} (${newIntervalLength})")
    f(cur)
  })

def solve5Hard(intervals: Set[(Long, Long)], fs: List[Func5]): Long = {
  val ls = locations(intervals, fs)
  if ls.nonEmpty then ls.toList.sorted.head._1
  else -1L
}

def seedsAndFuncs(inputFilePath: String) =
  val lines = Files.readAllLines(Path.of(inputFilePath))
    .asScala
    .toList
  val seeds = lines
    .head
    .split(':')(1)
    .trim
    .split(' ')
    .map(_.toLong)
  val seedIntervals: Set[(Long, Long)] = seeds.slice(0, seeds.size-1)
    .zip(seeds.slice(1, seeds.size))
    .zipWithIndex
    .filter((pair, index) => index % 2 == 0)
    .map(_._1)
    .map(t => (t(0), t(0) + t(1)))
    .toSet
  val funcs: List[Func5] = lines.drop(2)
    .mkString("\n")
    .split("\n\n")
    .map(Func5.fromChunk)
    .toList
  (seedIntervals, funcs)

def solve5Part2(inputFilePath: String): Long =
  val (seeds, funcs) = seedsAndFuncs(inputFilePath)
  solve5Hard(seeds, funcs)

final case class RaceRecord(val time: Long, val distance: Long)

object RaceRecord:
  def hold(ms: Long, maxMs: Long) = RaceRecord(
      time = maxMs,
      distance = max(maxMs - ms, 0L) * ms 
    )


def solve6(input: List[RaceRecord]): Int =
  def countWaysToBeat(raceRecord: RaceRecord): Int =
    (1L until raceRecord.time)
      .map(ms => {
      RaceRecord.hold(ms, raceRecord.time)
    }).count(myRace => myRace.distance > raceRecord.distance)
  input.map(countWaysToBeat).foldLeft(1)((acc, cur) => acc * cur)

def parse6Input1(path: String): List[RaceRecord] =
  def parseNumbers(line: String) =
    println(line)
    line.split(':')
    .drop(1)
    .head
    .trim
    .split(" +")
    .map(_.toLong)
  val lines  = Files.readAllLines(Path.of(path)).asScala
  parseNumbers(lines(0))
  .zip(parseNumbers(lines(1)))
  .map(p => RaceRecord(p._1, p._2))
  .toList


def parse6Input2(path: String): RaceRecord =
  def parseNumbers(line: String) =
    line.split(':')
    .drop(1)
    .head
    .trim
    .split(" +")
    .mkString
    .toLong
  val lines = Files.readAllLines(Path.of(path)).asScala
  assert (lines.size == 2)
  val t = parseNumbers(lines(0))
  val d = parseNumbers(lines(1))
  RaceRecord(t, d)

def roots(t: Long, d: Long) =
    val delta = t * t - 4 * d;
    if delta <= 0 then List()
    else
        List(
            (-t - math.sqrt(delta)) / -2,
            (-t + math.sqrt(delta)) / -2).sorted

def solve6Fast(t: Long, d:Long) =
    val rs = roots(t, d)
    if rs.size < 2 then 0
    else math.floor(rs(1)).toLong - math.ceil(rs(0)).toLong + 1

object CamelPoker {
  final private case class Hand(private val content: String) extends Ordered[Hand] {
    val handType: HandType = getHandType(content)
    def compare(that: Hand) = {
      val handTypeComparison = handType.compare(that.handType)
      if handTypeComparison != 0 then handTypeComparison
      else
        cardStrengthComparison(content, that.content)
    }
  }

  final case class Hand2(private val content: String) extends Ordered[Hand2] {
    val handType: HandType = getHandType2(content)
    def compare(that: Hand2) = {
      val handTypeComparison = handType.compare(that.handType)
      if handTypeComparison != 0 then handTypeComparison
      else
        cardStrengthComparison2(content, that.content)
    }

    override def toString(): String = 
      s"Hand2($content, $handType)"
  }

  private def cardStrengthComparison(a: String, b: String): Int = {
    if a == b then 0
    else
      assert(a.length() == b.length())
      val (ca, cb) = a.zip(b).find(cs => cs._1 != cs._2).get
      val (orderA, orderB) = (cardStrenghts(ca), cardStrenghts(cb))
      orderA - orderB
  }

  def cardStrengthComparison2(a: String, b: String): Int = {
    if a == b then 0
    else
      assert(a.length() == b.length())
      val (ca, cb) = a.zip(b).find(cs => cs._1 != cs._2).get
      val (orderA, orderB) = (cardStrenghts2(ca), cardStrenghts2(cb))
      orderA - orderB
  }

  private def countDistincts(content: String): Map[Char, Int] = {
    content.foldLeft(Map[Char, Int]())((acc, cur) => {
      if acc.contains(cur) then acc.updated(cur, acc(cur) + 1)
      else acc.updated(cur, 1)
    })
  }

  private def getHandType(content: String): HandType = {
    val charCounts = countDistincts(content)
    val counts = charCounts.values.toList
    if counts.contains(5) then HandType.FiveOfAKind
    else if counts .contains(4) then HandType.FourOfAKind
    else if counts.contains(3) && counts.contains(2) then HandType.FullHouse
    else if counts.contains(3) then HandType.ThreeofAKind
    else if counts.count(_ == 2) == 2 then HandType.TwoPair
    else if counts.count(_ == 2) == 1 then HandType.OnePair
    else HandType.HighCard
  }

  def getHandType2(content: String): HandType = {
    val charCounts = countDistincts(content)
    val jokerCount = charCounts.getOrElse('J', 0)
    val counts = charCounts.values.toList
    // 5 of a kind
    if counts.contains(5) then HandType.FiveOfAKind
    else if counts.contains(5 - jokerCount) then HandType.FiveOfAKind
    // 4 of a kind
    else if counts.contains(4) then HandType.FourOfAKind
    else if counts.count(_ == 2) == 2 && jokerCount == 2 then HandType.FourOfAKind
    else if counts.contains(1) && jokerCount == 3 then HandType.FourOfAKind
    else if counts.contains(3) && jokerCount == 1 then HandType.FourOfAKind
    // full house
    else if counts.contains(3) && counts.contains(2) then HandType.FullHouse
    else if counts.contains(3) &&  counts.contains(2 - jokerCount) then HandType.FullHouse
    else if counts.count(_ == 2) == 2 && jokerCount >= 1 then HandType.FullHouse
    // 3 of a kind
    else if counts.contains(3) then HandType.ThreeofAKind
    else if counts.contains(2) && jokerCount == 1 then HandType.ThreeofAKind
    else if jokerCount == 2 then HandType.ThreeofAKind
    // 2 pairs
    else if counts.count(_ == 2) == 2 then HandType.TwoPair
    else if counts.count(_ == 2) == 1 && jokerCount == 1 then HandType.TwoPair
    // 1 pair
    else if counts.count(_ == 2) == 1 && jokerCount == 0 then HandType.OnePair
    else if jokerCount == 1 then HandType.OnePair
    // high card
    else HandType.HighCard
  }

  enum HandType(val order: Int) extends Ordered[HandType] {
    case FiveOfAKind extends HandType(1)
    case FourOfAKind extends HandType(2)
    case FullHouse extends HandType(3)
    case ThreeofAKind extends HandType(4)
    case TwoPair extends HandType(5)
    case OnePair extends HandType(6)
    case HighCard extends HandType(7)
    def compare(that: HandType) = {
      order - that.order
    }
  }

  private def cardStrenghts = Map(
    'A' -> 1,
    'K' -> 2,
    'Q' -> 3,
    'J' -> 4,
    'T' -> 5,
    '9' -> 6,
    '8' -> 7,
    '7' -> 8,
    '6' -> 9,
    '5' -> 10,
    '4' -> 11,
    '3' -> 12,
    '2' -> 13,
  )

  private def cardStrenghts2 = Map(
    'A' -> 1,
    'K' -> 2,
    'Q' -> 3,
    'T' -> 5,
    '9' -> 6,
    '8' -> 7,
    '7' -> 8,
    '6' -> 9,
    '5' -> 10,
    '4' -> 11,
    '3' -> 12,
    '2' -> 13,
    'J' -> 14,
  )

  private def sumRankedBids(handsAndBids: Seq[(Hand, Int)]) = {
    val sortedHands = handsAndBids.sorted
    sortedHands
      .map(_._2) // largest bid first
      .reverse
      .zipWithIndex
      .foldLeft(0)((acc, cur) => acc + (cur._2 + 1) * cur._1)
  }

  private def sumRankedBids2(handsAndBids: Seq[(Hand2, Int)]) = {
    val sortedHands = handsAndBids.sorted
    sortedHands
      .map(_._2) // largest bid first
      .reverse
      .zipWithIndex
      .foldLeft(0)((acc, cur) => acc + (cur._2 + 1) * cur._1)
  }
  private def parseInput(lines: Seq[String]): Seq[(Hand, Int)] = {
    lines.map(line => {
      val pieces = line.split(' ')
      (Hand(pieces(0)), pieces(1).toInt)
    })
  }
  private def parseInput(inputFile: String): Seq[(Hand, Int)] = {
    parseInput(Files.readAllLines(Path.of(inputFile)).asScala.toList)
  }
  private def parseInput2(lines: Seq[String]): Seq[(Hand2, Int)] = {
    lines.map(line => {
      val pieces = line.split(' ')
      (Hand2(pieces(0)), pieces(1).toInt)
    })
  }
  private def parseInput2(inputFile: String): Seq[(Hand2, Int)] = {
    parseInput2(Files.readAllLines(Path.of(inputFile)).asScala.toList)
  }
  def solve(inputFile: String) = sumRankedBids(parseInput(inputFile))
  def solve2(inputFile: String) = {
    val input = parseInput2(inputFile)
    sumRankedBids2(input)
  }
}


def main(): Unit =
  val sampleInput = "./inputs/7_sample.txt"
  val sampleOutput = CamelPoker.solve2(sampleInput)
  assert(5905 == sampleOutput, s"example failed! wanted 5905 but got $sampleOutput")
  val puzzleInput = "./inputs/7_puzzle.txt"
  val puzzleOutput = CamelPoker.solve2(puzzleInput)
  assert(249836800 < puzzleOutput, s"$puzzleOutput is too low")
  assert(250564972 < puzzleOutput, s"$puzzleOutput is too low")
  println(puzzleOutput)
  
