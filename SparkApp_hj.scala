
import java.util._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import mapmatch._

object SparkApp_hj {
  def getMatcher:MapMatch_hj = {
    var temp = new SingaporeGraph
    temp.Init()
    
    val matcher = new MapMatch_hj(temp)
    
    return matcher
  }
  
  def main(args:Array[String]):Unit = {
    println("Hi, this is ZJ and ZHJ's system for map-matching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    val matcher = getMatcher
    
    var rawPoints = new Array[Array[Double]](2)
    rawPoints(0) = new Array[Double](2)
    rawPoints(0)(0)= 103.90196
    rawPoints(0)(1) = 1.35697
    
    rawPoints(1) = new Array[Double](2)
    rawPoints(1)(0) = 103.849981
    rawPoints(1)(1) = 1.29936
    
    rawPoints(1) = rawPoints(0)
    
    
    
    var re = matcher.getMatchedRoute(rawPoints)
    
    
    
    
    println(re(0).x+"\t"+re(0).y+"\t"+0)
    println(re(1).x+"\t"+re(1).y+"\t"+0)
    
  }
}