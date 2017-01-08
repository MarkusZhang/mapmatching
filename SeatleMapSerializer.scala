import mapmatch._

import java.io._
import java.util._
import scala.math._


object SeatleMapSerializer {
  def main(args:Array[String]) = {
    var G = new SeatleGraph
    //G.sourceFile = "Your road_network.txt file location"
    println("Converting to Array...")
    var arr = G.ConvertToArray()
    println("Conversion Complete")
    val oos = new ObjectOutputStream(new FileOutputStream("Your file destination"))
    oos.writeObject(arr)
    oos.close
    println("Finish!")
  } 
}