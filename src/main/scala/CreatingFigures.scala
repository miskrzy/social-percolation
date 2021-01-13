import scala.collection.mutable

object CreatingFigures {
  def distributionF1abc(numbLattices: Int, size: Int, steps: Int, res: Int, dp: Double = 0): Map[Double, Int] = {
    var result = Map[Double, Int]()
    (0 until numbLattices).foreach(_ => {
      val model = new Percolation(size, dp)
      (0 until steps).foreach(_ => model.singleFilmView())
      val tempDist = model.getDistribution(res)
      result = (result.keys ++ tempDist.keys).map(key => key -> (result.getOrElse(key, 0) + tempDist.getOrElse(key, 0))).toMap
    })
    result
  }

  def qOfTime(size: Int, dp: Double = 0, startq: Double = 0.593, dq: Double, timeList: List[Int]): Map[Int, Double] = {
    val model = new Percolation(size, dp, startq, dq)
    var result = Map[Int, Double]()
    (0 to timeList.last).foreach(time => {

      model.singleFilmView()
      if (timeList.contains(time)) {
        result += time -> model.getQ()
      }

    })
    result
  }

  def finQofSample(samples: Int, size: Int, dp: Double = 0, startq: Double = 0.5, dq: Double): Map[Int, Double] = {

    (1 to samples).map(nr => {
      val model = new Percolation(size, dp, startq, dq)
      var check_q = model.getQ()
      var cnt = 1000
      while (cnt != 0) {
        model.singleFilmView()
        if (math.abs(model.getQ() - check_q) < dq * 2) {
          cnt -= 1
        } else {
          cnt = 1000
          check_q = model.getQ()
        }
      }
      nr -> model.getQ()
    }).toMap
  }

  def distClusters(samples: Int, size: Int, dp: Double = 0, startq: Double = 0.5, dq: Double): Map[Int, Int] = {
    var result = mutable.Map[Int, Int]()
    (1 to samples).foreach(nr => {
      println("sample: " + nr)
      val model = new Percolation(size, dp, startq, dq)
      var check_q = model.getQ()
      var cnt = 100
      while (cnt != 0) {
        model.singleFilmView()
        if (math.abs(model.getQ() - check_q) < dq * 4) {
          cnt -= 1
        } else {
          cnt = 100
          check_q = model.getQ()
        }
      }
      println("stablility found")
      model.getClusters().foreach(pair => {
        result(pair._1) = result.getOrElse(pair._1, 0) + pair._2
      })
      println("clusters found")
    })
    result.toMap
  }

  def distClustersTEST(samples: Int, size: Int, dp: Double = 0, startq: Double = 0.5, dq: Double): Map[Int, Int] = {
    var result = mutable.Map[Int, Int]()
    (1 to samples).foreach(nr => {
      println("sample: " + nr)
      val model = new Percolation(size, dp, startq, dq)
      model.singleFilmView()
      model.getClusters().foreach(pair => {
        result(pair._1) = result.getOrElse(pair._1, 0) + pair._2
      })
      println("clusters found")
    })
    result.toMap
  }

}
