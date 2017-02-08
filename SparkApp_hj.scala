
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
    println("Hi, this is ZJ and ZHJ's system for mapmatching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    val matcher = getMatcher
    
    var rawPoints = new Array[Array[Double]](2)
    rawPoints(0) = new Array[Double](2)
    rawPoints(0)(0)= 103.765026
    rawPoints(0)(1) = 1.315564
    
    rawPoints(1) = new Array[Double](2)
    rawPoints(1)(0) = 103.765424
    rawPoints(1)(1) = 1.314633
    
    
    
    var re = matcher.getMatchedRoute(rawPoints);
    
    println(re(0))
    println(re(1))
    
  }
}