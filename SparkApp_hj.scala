
import java.util._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import mapmatch._
import java.io._

object SparkApp_hj {
  def getMatcher:MapMatch_hj = {
    var temp = new SingaporeGraph
    temp.Init()
    
    val matcher = new MapMatch_hj(temp)
    
    return matcher
  }
  
  def getMatcherBySerialized:MapMatch_hj = {
    var temp = new BaseGraph
    //val ois = new ObjectInputStream(new FileInputStream("e:/HM_MapMatching/road_network_serialized.txt"))
    val ois = new ObjectInputStream(new FileInputStream("e:/taxi/LTA_serialized.txt"))
    
    val roadNetwork = ois.readObject.asInstanceOf[Array[roadSeg]]
    ois.close()
    
    temp.Init(roadNetwork)
    
    return new MapMatch_hj(temp)
  }
  
  def main(args:Array[String]):Unit = {
    println("Hi, this is ZJ and ZHJ's system for map-matching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    val matcher = getMatcherBySerialized
    
    var rawPoints = new Array[Array[Double]](2)
    rawPoints(0) = new Array[Double](2)
    rawPoints(0)(0)= 103.765026
    rawPoints(0)(1) = 1.315564
    
    rawPoints(1) = new Array[Double](2)
    rawPoints(1)(0) = 103.777761
    rawPoints(1)(1) = 1.300560
    
    /*var a = matcher.GC.getNeighbours(rawPoints(1)(0), rawPoints(1)(1), 100)
    
    pr(rawPoints(1)(0),rawPoints(1)(1),0)
    for(i <- 0 until a.length){
      println(a(i).x+"\t"+a(i).y+"\t"+1)
    }*/
    
    var re = matcher.getMatchedRoute(rawPoints)
    
    println(re(0).x+"\t"+re(0).y+"\t"+0)
    println(re(1).x+"\t"+re(1).y+"\t"+0)
    
    var a = matcher.getMatchedRouteDetail(re)
    for(i <- 0 until a.length){
      println(a(i).x+"\t"+a(i).y+"\t"+1)
    }
    
  }
  
  def pr(a:Double,b:Double,t:Int):Unit = println(a+"\t"+b+"\t"+t) 
}