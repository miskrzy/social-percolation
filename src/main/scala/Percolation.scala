
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class Percolation(dim: Int, dp: Double = 0, startq: Double = 0.593, dq: Double = 0) {

  private var q = startq

  private val random = new Random()

  private val matrix = Array.ofDim[Double](dim, dim).map(row => {
    row.map(_ => {
      Random.nextDouble()
    })
  })

  private var viewMatrix = matrix.map(row => row.map(p => 0))

  def printMatrix(): Unit = {
    matrix.foreach(row => {
      row.foreach(elem => {
        print(elem)
        print(" ")
      })
      println()
    })
  }

  def singleFilmView(): Boolean = {
    viewMatrix = matrix.map(row => row.map(p => 0))

    //last iteration film goers
    var lastGoersOld = ListBuffer[(Int, Int)]()
    val lastGoersNew = ListBuffer[(Int, Int)]()

    //first row
    viewMatrix(0).indices.foreach(i => {
      if (q > matrix(0)(i)) {
        lastGoersNew.append((0, i))
        viewMatrix(0)(i) = 1
        matrix(0)(i) += dp
      }
      else {
        viewMatrix(0)(i) = -1
        matrix(0)(i) -= dp
      }
    })

    //rest of iterations
    var canContinue = true
    while (canContinue) {

      canContinue = false

      lastGoersOld = lastGoersNew.clone()
      lastGoersNew.clear()
      lastGoersOld.foreach(agentCoord => {

        val x = agentCoord._1
        val y = agentCoord._2


        //upper
        var xn = if (x > 0) x - 1 else x
        var yn = y
        if (viewMatrix(xn)(yn) == 0) {
          if (q > matrix(xn)(yn)) {
            lastGoersNew.append((xn, yn))
            viewMatrix(xn)(yn) = 1
            matrix(xn)(yn) += dp
          }
          else {
            viewMatrix(xn)(yn) = -1
            matrix(xn)(yn) -= dp
          }
          canContinue = true
        }
        //lower
        xn = if (x < viewMatrix.length - 1) x + 1 else x
        yn = y
        if (viewMatrix(xn)(yn) == 0) {
          if (q > matrix(xn)(yn)) {
            lastGoersNew.append((xn, yn))
            viewMatrix(xn)(yn) = 1
            matrix(xn)(yn) += dp
          }
          else {
            viewMatrix(xn)(yn) = -1
            matrix(xn)(yn) -= dp
          }
          canContinue = true
        }
        //right
        xn = x
        yn = if (y < viewMatrix.head.length - 1) y + 1 else y
        if (viewMatrix(xn)(yn) == 0) {
          if (q > matrix(xn)(yn)) {
            lastGoersNew.append((xn, yn))
            viewMatrix(xn)(yn) = 1
            matrix(xn)(yn) += dp
          }
          else {
            viewMatrix(xn)(yn) = -1
            matrix(xn)(yn) -= dp
          }
          canContinue = true
        }
        //left
        xn = x
        yn = if (y > 0) y - 1 else y
        if (viewMatrix(xn)(yn) == 0) {
          if (q > matrix(xn)(yn)) {
            lastGoersNew.append((xn, yn))
            viewMatrix(xn)(yn) = 1
            matrix(xn)(yn) += dp
          }
          else {
            viewMatrix(xn)(yn) = -1
            matrix(xn)(yn) -= dp
          }
          canContinue = true
        }
      })
    }

    var reachedEnd = false
    viewMatrix.last.foreach(dec => {
      if (dec == 1) reachedEnd = true
    })
    if (reachedEnd) q -= dq else q += dq

    reachedEnd
  }

  def getDistribution(resolution: Int): Map[Double, Int] = {
    val disp = 1.0 / resolution.toDouble / 2.0
    val distribution = new mutable.HashMap[Double, Int]() ++
      (0 until resolution).map(p => p.toDouble / resolution.toDouble + disp -> 0).toMap

    matrix.foreach(_.foreach(p => {
      distribution(math.round((p - disp) * resolution.toDouble) / resolution.toDouble + disp) += 1
    }))

    distribution.toMap
  }

  def getQ(): Double = {
    q
  }

  def getClusters(): Map[Int,Int] ={
    val clusterCount = new ClusterCount(viewMatrix)
    clusterCount.getClusters()
  }

  //only aux
  def printView(): Unit = {
    viewMatrix.foreach(row => {
      row.foreach(elem => {
        print(elem + "\t")
      })
      println()
    })
  }


}
