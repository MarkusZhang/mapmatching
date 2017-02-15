import mapmatch._

import java.io._
import java.util._
import scala.math._


object SeatleMapSerializer {
  def main(args:Array[String]) = {
    var G = new SeatleGraph
    G.sourceFile = "e:/HM_MapMatching/road_network.txt"
    println("Converting to Array...")
    var arr = G.ConvertToArray()
    println("Conversion Complete")
    val oos = new ObjectOutputStream(new FileOutputStream("e:/HM_MapMatching/road_network_serialized.txt"))
    oos.writeObject(arr)
    oos.close
    println("Finish!")
  } 
}