
import java.util._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import mapmatch._

object SparkApp_hj {
  def getMatcher:MapMatch_hj = {
    var temp = new SeatleGraph
    temp.Init()
    
    val matcher = new MapMatch_hj(temp)
    
    return matcher
  }
  
  def main(args:Array[String]):Unit = {
    println("Hi, this is ZJ and ZHJ's system for mapmatching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    val matcher = getMatcher
    
  }
}