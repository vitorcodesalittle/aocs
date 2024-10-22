import scala.collection.JavaConverters._
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable

object Day7 {
  sealed case class Node(val key: String)
  sealed case class Graph(val nodes: List[Node], val edges: List[List[Int]]) {
    assert(nodes.size == edges.size, "nodes and edges list must have same size")
    def getNext(nodeIndex: Int, edgeIndex: Int): Int =
      assert(nodeIndex < nodes.size)
      assert(edgeIndex < edges(nodeIndex).size)
      edges(nodeIndex)(edgeIndex)
  }
  sealed case class Input(
      path: String,
      tree: Graph
  ) {
    def mapStartEnd: Map[Int, (Int, Option[Int])] =
      (0 until tree.nodes.size).foldLeft(Map[Int, (Int, Option[Int])]())(
        (acc, nodeIndex) => {
          var currentIndex = nodeIndex
          var zIndex = -1
          (0 until path.size).foreach(pathIndex => {
            if path(pathIndex) == 'L' then
              currentIndex = tree.edges(currentIndex)(0)
            else currentIndex = tree.edges(currentIndex)(1)
            if tree.nodes(currentIndex).key == "ZZZ" then zIndex = pathIndex + 1
          })
          acc + (nodeIndex -> (currentIndex, if zIndex != -1 then Some(zIndex)
          else None))
        }
      )
    def mapStartEnd2: Map[Int, (Int, List[Int])] = {
      (0 until tree.nodes.size).foldLeft(Map[Int, (Int, List[Int])]())(
        (acc, nodeIndex) => {
          var currentIndex = nodeIndex
          val zIndices = (0 until path.size)
            .map(pathIndex => {
              if path(pathIndex) == 'L' then
                currentIndex = tree.edges(currentIndex)(0)
              else currentIndex = tree.edges(currentIndex)(1)
              if tree.nodes(currentIndex).key.endsWith("Z") then pathIndex + 1
              else -1
            })
            .toList
            .filter(_ != -1)
          acc + (nodeIndex -> (currentIndex, zIndices))
        }
      )
    }
  }
  def parseInput(pathStr: String): Input = {
    val lines = Files.readAllLines(Path.of(pathStr)).asScala
    val nodes = lines
      .drop(2)
      .map(line => {
        val pieces = line.split('=')
        val key = pieces(0).trim
        Node(key)
      })
      .toList
    val nodesIndexByKey =
      nodes.zipWithIndex.foldLeft(Map[String, Int]())((acc, cur) => {
        val (node, index) = cur
        acc + (node.key -> index)
      })
    val edges = lines
      .drop(2)
      .map(line => {
        val pieces = line.split('=')
        val right = pieces(1).replace("(", "").replace(")", "").split(',')
        val leftKey = right(0).trim
        val rightKey = right(1).trim
        val leftIndex = nodesIndexByKey(leftKey)
        val rightIndex = nodesIndexByKey(rightKey)
        List(leftIndex, rightIndex)
      })
      .toList
    Input(
      lines(0),
      Graph(nodes, edges)
    )
  }

  def solve(input: Input): Int = {
    val mapStartEnd: Map[Int, (Int, Option[Int])] = input.mapStartEnd
    var walks = 0
    var currentIndex = input.tree.nodes.map(_.key).indexOf("AAA")
    while mapStartEnd(currentIndex)._2 == None do
      currentIndex = mapStartEnd(currentIndex)._1
      walks += 1
    end while
    walks * input.path.size + mapStartEnd(currentIndex)._2.get
  }

  def solve2(input: Input): Int = {
    val mapStartEnd: Map[Int, (Int, List[Int])] = input.mapStartEnd2
    var walks = 0
    var currentIndices =
      input.tree.nodes.zipWithIndex.filter(_._1.key.endsWith("A")).map(_._2)
    def done = () => {
      val positions = currentIndices.map(index => mapStartEnd(index)._2)
      if positions.size != currentIndices.size then
        throw new RuntimeException(
          "positions deve ter sempre tamanho igual currentIndices.size"
        )
      positions(0).find((offset) =>
        positions.drop(1).forall(_.contains(offset))
      ) match
        case None        => -1
        case Some(value) => value
    }
    while done() == -1 do
      currentIndices = currentIndices.map(index => {
        mapStartEnd(index)._1
      })
      walks += 1
    end while
    walks * input.path.size + done()
  }

  @main
  def main(): Unit = {
    val sampleInput = parseInput("./inputs/8_sample.txt")
    assert(sampleInput.tree.nodes.size == 7)
    assert(sampleInput.tree.edges.forall(_.size == 2))
    val wantSample = 2
    val gotSample = solve(sampleInput)
    assert(
      wantSample == gotSample,
      s"Failed on sample input. Wanted $wantSample but got $gotSample"
    )
    println("Sample 1 ok!")

    val sampleInput2 = parseInput("./inputs/8_sample2.txt")
    val wantSample2 = 6
    val gotSample2 = solve(sampleInput2)
    assert(
      wantSample2 == gotSample2,
      s"Failned on 2nd sample input. Wanted $wantSample2 but got $gotSample2"
    )
    println("Sample 2 ok!")

    println("Starting puzzle")
    println(solve(parseInput("./inputs/8_puzzle.txt")))

    val sampleInput3 = parseInput("./inputs/8_sample3.txt")
    val gotSample3 = solve2(sampleInput3)
    val wantSample3 = 6
    assert(
      wantSample3 == gotSample3,
      s"Failed on sample input 3. Wanted $wantSample but got $gotSample"
    )

    println("Starting puzzle part 2")
    println(solve2(parseInput("./inputs/8_puzzle.txt")))
  }

}
