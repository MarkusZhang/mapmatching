package mapmatch

import java.io._

import scala.io.Source._

object DataSerializer {
  def main(args: Array[String]): Unit = {
    // read the file into lines
    var arr = fromFile("D:/fyp/LTA_graph.txt").getLines().map(x=>x.split(",").map(s=>s.toDouble)).toArray
    // write the array to file
    val oos = new ObjectOutputStream(new FileOutputStream("D:/fyp/LTA_serialized.txt"))
    oos.writeObject(arr)
    oos.close
  }
}