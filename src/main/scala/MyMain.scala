import scala.collection.mutable
import scala.util.Random

object MyMain {

  def fig12(): Unit ={
    val distribution = CreatingFigures.distributionF1abc(8,101,500000,100, 0.001)
    SavingToFile.saveDist(distribution,"target/plots/distribution500k.txt")
    /*distribution.toSeq.sorted.foreach(println)
    println()
    println(distribution.size)*/
  }

  def fig3(): Unit ={
    val distribution = CreatingFigures.distributionF1abc(1,4001,5,1000, 0.0001)
    SavingToFile.saveDist(distribution,"target/plots/distributionL4001_5.txt")
    /*distribution.toSeq.sorted.foreach(println)
    println()
    println(distribution.size)*/
  }

  def fig4(): Unit ={
    val result = CreatingFigures.finQofSample(8,1001,0.000,0.5,0.0001)
    SavingToFile.saveIntDouble(result,"target/plots/samples1001_constantP.txt")
  }

  def fig5(): Unit ={
    val result = CreatingFigures.distClusters(80,101,0.0001,0.5,0.0001)
    SavingToFile.saveIntInt(result,"target/plots/clusters101_80.txt")
  }

  def figNone(): Unit ={
    val result = CreatingFigures.distClustersTEST(10000,101,0,0.6,0)
    SavingToFile.saveIntInt(result,"target/plots/clusters_TEST.txt")
  }

  def testClusters(): Unit ={
    val dim = 10
    val matrix = Array.ofDim[Double](dim, dim).map(row => {
      row.map(_ => {
        if(Random.nextDouble()>0.6) 1 else 0
      })
    })
    val counting = new ClusterCount(matrix)
    println(counting.getClusters().toSeq)
    println(counting.getClusters().toSeq.sortBy(_._1))
  }

  def main(args: Array[String]): Unit = {


    //fig12()
    fig4()
    //fig5()
    //figNone()
    //testClusters()
    //fig3()
/*
    val test1 = Array(Array(1,1,-1,-1,-1),
      Array(1,-1,-1,1,-1),
      Array(1,1,1,-1,1),
      Array(-1,-1,1,-1,1),
      Array(1,1,1,1,1))
    val test2 = Array.ofDim[Double](20, 20).map(row => {
      row.map(_ => {
        if(Random.nextDouble()>0.6) 1 else 0
      })
    })
    val testC = new ClusterCount(test2)
    testC.printView(testC.matrix)
    println(testC.getClusters().toSeq.sortBy(_._1))
*/
  }
}
