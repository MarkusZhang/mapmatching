import mapmatch._

import java.io._
import java.util._
import scala.math._


object SingaporeMapSerializer {
  def main(args:Array[String]) = {
    var G = new SingaporeGraph
    G.  sourceFile = "e:/taxi/LTA.txt"
    println("Converting to Array...")
    var arr = G.ConvertToArray()
    println("Conversion Complete")
    val oos = new ObjectOutputStream(new FileOutputStream("e:/taxi/LTA_serialized.txt"))
    oos.writeObject(arr)
    oos.close
    println("Finish!")
  }
}