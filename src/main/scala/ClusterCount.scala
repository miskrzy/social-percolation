import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ClusterCount(m: Array[Array[Int]]) {

  val clusters = mutable.Map[Int, Int]()
  val matrix = m.map(_.map(elem => if (elem == 1) 1 else 0))

  def getClusters(): Map[Int, Int] = {
    //printView(matrix)
    var i = 2
    var north = 0
    var west = 0
    for (row <- matrix.indices) {
      for (col <- matrix(row).indices) {
        //println(row,col)
        if (matrix(row)(col) == 1) {
          //neighbours
          if (row > 0) {
            north = getCKey(matrix(row - 1)(col))
          } else north = 0
          if (col > 0) {
            west = getCKey(matrix(row)(col - 1))
          } else west = 0

          //check
          if (north == 0 && west == 0) {
            clusters(i) = 1
            matrix(row)(col) = i
            i += 1
          } else if (north != 0 && west == 0) {
            clusters(north) += 1
            matrix(row)(col) = north
          } else if (north == 0 && west != 0) {
            clusters(west) += 1
            matrix(row)(col) = west
          } else if (north == west) {
            clusters(north) += 1
            matrix(row)(col) = north
          } else if (north != west) {
            clusters(north) += 1 + clusters(west)
            clusters(west) = -north
            matrix(row)(col) = north
          }
        }
      }
    }
    val result = mutable.Map[Int, Int]()
    clusters.foreach(pair => {
      if (pair._2 > 0){
        if(result.contains(pair._2)){
          result += pair._2 -> (result(pair._2) + 1)
        }else result += (pair._2 -> 1)
      }
    })
    result.toMap
  }

  private def getCKey(key: Int): Int = {
    if (key == 0) 0 else if (clusters(key) < 0) getCKey(-clusters(key)) else key
  }

  def printView(view: Array[Array[Int]]): Unit = {
    view.foreach(row => {
      row.foreach(elem => {
        print(elem + "\t")
      })
      println()
    })
  }
}
