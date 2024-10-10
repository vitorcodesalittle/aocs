import scala.collection.JavaConverters._
import java.nio.file.Files
import java.nio.file.Path

def solve1(inputFilePath: Path): Long =
  Files
    .readAllLines(inputFilePath)
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

@main def hello(args: String*): Int =
  if args.isEmpty then 1
  else
    println(solve2Part2(Path.of(args(0))).toString)
    0
