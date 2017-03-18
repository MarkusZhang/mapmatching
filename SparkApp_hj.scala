
import java.util._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import mapmatch._
import java.io._
import scala.math._

object SparkApp_hj {
  def getMatcher:MapMatch_hj = {
    //var temp = new SingaporeGraph
    var temp = new SeattleGraph
    temp.Init()
    
    val matcher = new MapMatch_hj(temp)
    
    return matcher
  }
  
  def getMatcherBySerialized:MapMatch_hj = {
    var temp = new BaseGraph
    val ois = new ObjectInputStream(new FileInputStream("e:/HM_MapMatching/road_network_serialized.txt"))
    //val ois = new ObjectInputStream(new FileInputStream("e:/taxi/LTA_serialized.txt"))
    
    val roadNetwork = ois.readObject.asInstanceOf[Array[roadSeg]]
    ois.close()
    
    temp.Init(roadNetwork)
    
    return new MapMatch_hj(temp)
  }
  
  def main(args:Array[String]):Unit = {
    println("Hi, this is ZJ and ZHJ's system for map-matching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    //val matcher = getMatcherBySerialized
    
    var pw = new PrintWriter(new File("e:/HM_MapMatching/Gapped_Point.txt" ))

    
    var datafile = "e:/HM_MapMatching/gps_data.txt"
    
    var in = new Scanner(new File(datafile));
    in.nextLine()
    var p = 0
    //total num is 7531
    var sz:Int = 7531
    //println(sz)
    var rawPoints = new Array[Array[Double]](sz)
    
    for(i<- 0 until sz){
      in.next()
      in.next()
      rawPoints(p) = new Array[Double](2)
      rawPoints(p)(1) = in.nextDouble()
      rawPoints(p)(0) = in.nextDouble()
      p=p+1
      
    }
    
    in = new Scanner(new File("E:/HM_MapMatching/ground_truth_route.txt"))
    in.nextLine()
    var szgt = 0
    var GT = new LinkedList[String]
    while(in.hasNext()){
      var a = in.next()
      var b = in.nextInt()
      if(b==1){
        a=a+"+"
      }else{
        a=a+"-"
      }
      GT.addLast(a)
    }
    var GroundTruth = ConvertToArray(GT)
   
    var noise = 0.0
    var gap = 10
    


    
    
    val matcher = getMatcher
    
    matcher.MEASUREMENT_STD = 4.07+noise
    matcher.BETA = 1.0*gap/log(2.0)*8*(1.0-sqrt(2)/2.0)/2.0
    matcher.NEIGHBOUR_RADIUS=10*(matcher.MEASUREMENT_STD.toInt+1)
    
    rawPoints = AddNoise(matcher,noise,rawPoints)
    
    var rawPoints2 = SampleRateFilter(rawPoints,gap)
    
    for(i <- 0 until rawPoints2.length){
      pw.write(rawPoints2(i)(1)+","+rawPoints2(i)(0)+"\r\n")

    }
    pw.close
    
    var re = matcher.getMatchedRoute(rawPoints2)
    println("Matched Route Found!")
    
    pw = new PrintWriter(new File("e:/HM_MapMatching/result.txt"))

    for(i <- 0 until re.length){
      if(i>0) println(matcher.GC.getShortestRouteDistance(re(i-1),re(i)))
      println(i+" "+re(i)+" LinkId:"+matcher.GC.G.LinkId(re(i).roadSegId))
      pw.write(re(i).y+","+re(i).x+"\r\n")
    }
    
    pw.close

    var a = matcher.getMatchedRouteDetail(re)
    println("Matched Route Detail Found!")
		
    pw = new PrintWriter(new File("e:/HM_MapMatching/RouteDetail.txt"))
    var RP = new LinkedList[String]
    var end = "-1"
    for(i <- 0 until a.length){
      pw.write(a(i).y+","+a(i).x+"\r\n")
      
      var t = matcher.GC.G.LinkId(a(i).roadSegId)
      if(t.last=='-'){
        t=t.substring(0,t.length()-4)
        t=t+"-"
      }else{
        t=t.substring(0,t.length()-3)
        t=t+"+"
      }
      if(t!=end){
        RP.addLast(t)
        end=t
      }
    }
    pw.close
    
    var ResultPath = ConvertToArray(RP)

    
    var LCSCal = new LCS
    var lcs = LCSCal.getResult(GroundTruth,ResultPath)
    println(GroundTruth.length)
    println(ResultPath.length)
    println(1.0-1.0*(GroundTruth.length+ResultPath.length-2*lcs)/GroundTruth.length)
    
    
    //884147800805
    
    //TODO: preprocess the raw data. Noise Generating Function
    
    //47.66965,-122.1051667
    //47.67098333,-122.1066
    
    /*
    var matcher = getMatcher
    var rawPoints = new Array[Array[Double]](2)
    rawPoints(0) = new Array[Double](2)
    rawPoints(0)(0)= -122.1051667
    rawPoints(0)(1) = 47.66965
    
    rawPoints(1) = new Array[Double](2)
    rawPoints(1)(0) = -122.1066
    rawPoints(1)(1) = 47.67098333
    
    var a = matcher.GC.getNeighbours(rawPoints(1)(0), rawPoints(1)(1), 100)
    
    pr(rawPoints(1)(0),rawPoints(1)(1),0)
    for(i <- 0 until a.length){
      println(a(i))
    }
    
    var sp = a(0)
    println(sp)
    
    println(matcher.GC.G.LinkId(sp.roadSegId))
    
    println(matcher.GC.G.NormalToGeoLoc(matcher.GC.G.Point(sp.roadSegId)(0)))
    println(matcher.GC.G.NormalToGeoLoc(matcher.GC.G.Point(sp.roadSegId)(1)))
    */
    
    
    /*var re = matcher.getMatchedRoute(rawPoints)
    
    println(re(0).x+"\t"+re(0).y+"\t"+0)
    println(re(1).x+"\t"+re(1).y+"\t"+0)
    
    var a = matcher.getMatchedRouteDetail(re)
    for(i <- 0 until a.length){
      println(a(i).x+"\t"+a(i).y+"\t"+1)
    }
    */
    

  }
  
  def SampleRateFilter(rawPoints:Array[Array[Double]],gap:Int):Array[Array[Double]] = {
    var sz = rawPoints.length
    var sz2 = (sz/gap).toInt
    var re = new Array[Array[Double]](sz2)
    for(i <- 0 until sz2){
      re(i) = rawPoints(i*gap)
    }
    return re
  }
  
  def AddNoise(matcher:MapMatch_hj,t:Double,rawPoints:Array[Array[Double]]):Array[Array[Double]]={
    for(i <- 0 until rawPoints.length){
      var a = matcher.GenerateNoise(t)
      rawPoints(i)(0)+=a.x
      rawPoints(i)(1)+=a.y
    }
    return rawPoints
  }
  
  def ConvertToArray(a:LinkedList[String]):Array[String]={
    var re = new Array[String](a.size)
    var p = 0
    var c = a.iterator()
    while(c.hasNext()){
      re(p)=c.next()
      p=p+1
    }
    return re
    
  }

  
  def pr(a:Double,b:Double,t:Int):Unit = println(a+"\t"+b+"\t"+t) 
}