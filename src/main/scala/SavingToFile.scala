import java.io.File
import java.io.PrintWriter


object SavingToFile {

  def saveDist(dist:Map[Double,Int],path:String): Unit ={
    val writer = new PrintWriter(new File(path))
    dist.toSeq.sorted.foreach(pair =>{
      writer.println(pair._1 + " " + pair._2)
    })
    writer.close()
  }

  def saveIntDouble(dist:Map[Int,Double],path:String): Unit ={
    val writer = new PrintWriter(new File(path))
    dist.toSeq.sorted.foreach(pair =>{
      writer.println(pair._1 + " " + pair._2)
    })
    writer.close()
  }

  def saveIntInt(dist:Map[Int,Int],path:String): Unit ={
    val writer = new PrintWriter(new File(path))
    dist.toSeq.sorted.foreach(pair =>{
      writer.println(pair._1 + " " + pair._2)
    })
    writer.close()
  }
}
