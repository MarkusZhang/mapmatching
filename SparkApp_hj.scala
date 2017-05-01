
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
  
  /*def getMatcherBySerialized:MapMatch_hj = {
    var temp = new BaseGraph
    val ois = new ObjectInputStream(new FileInputStream("e:/HM_MapMatching/road_network_serialized.txt"))
    //val ois = new ObjectInputStream(new FileInputStream("e:/taxi/LTA_serialized.txt"))
    
    val roadNetwork = ois.readObject.asInstanceOf[Array[roadSeg]]
    ois.close()
    
    temp.Init(roadNetwork)
    
    return new MapMatch_hj(temp)
  }*/
  
  def main(args:Array[String]):Unit = {
    println("Hi, this is ZJ and ZHJ's system for map-matching.")
    
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)

    //val matcher = getMatcherBySerialized
    
    var ti = System.nanoTime()

    val matcher = getMatcher
    
    println("Matcher Build Time: "+ ((System.nanoTime-ti)/1e9) +"s")
    
    var tg = 15
    var noise = 5.0

    solveSeattle(tg,noise,matcher);
    
    /*for(i <- 1 to 3){
      for(j <- 0 until 7){
        tg = i*10
        noise = 5.0*j
        ti = System.nanoTime()
        println("Time Gap: "+tg+" Noise: "+noise)
        solveSeattle(tg,noise,matcher)
        ti=System.nanoTime()-ti
        println(ti/1e9+"s")
        println("")
      }
    }*/
    
    //solveSingapore(matcher)
  }
  
  def solveSingapore(matcher:MapMatch_hj){
    matcher.MEASUREMENT_STD = 10.0
    matcher.BETA = 1.0*30/log(2.0)*3*(1.0-sqrt(2)/2.0)/2.0
    matcher.NEIGHBOUR_RADIUS=50+3*(matcher.MEASUREMENT_STD.toInt+1)
    
    var datafile = "e:/taxi/taxidata.txt"
    var in = new Scanner(new File(datafile))
    var Data = new Array[LinkedList[GPSwD]](4385)
    
    var pre = -1;
    var precar = ""
    while(in.hasNext){
      var car = in.next()
      if(car!=precar){
        pre=pre+1
        Data(pre) = new LinkedList[GPSwD]
        precar=car
      }
      var po = Array(in.nextDouble(),in.nextDouble())
      var speed = in.nextDouble()
      var dir = getDirection(in.nextDouble())
      Data(pre).addLast(new GPSwD(po(0),po(1),dir))
      in.nextLine()
    }
    
    var c = Data(0).iterator()
    while(c.hasNext){
      var t=c.next()
      //pr(t(0),t(1),0)
    }
    
    var ti = System.nanoTime()
    var cnt = 0
    for(i <- 0 until Data.length){
      
      println("Processing Car: "+i)
      var re2 = ConvertToArray(Data(i))
      var re = PreProcessRawData(matcher,re2,2.0*matcher.MEASUREMENT_STD)
      println(re.length)
      var ma = matcher.getMatchedRoute(re)
      var a = matcher.getMatchedRouteDetail(ma)
    
      if(re.length>50) cnt=cnt+1
      for(i <- 0 until re.length){
        pr(re(i).x,re(i).y,0)
      }
    
      for(i <- 0 until a.length){
        //pr(a(i).x,a(i).y,1)
      }
      println((System.nanoTime()-ti)/1e9/cnt)
      println("")
    }

    
    
    /*var rawPoints = new Array[Array[Double]](2)
    rawPoints(0) = new Array[Double](2)
    rawPoints(0)(0)= 103.765026
    rawPoints(0)(1) = 1.314633
    
    rawPoints(1) = new Array[Double](2)
    rawPoints(1)(0) = 103.97236
    rawPoints(1)(1) = 1.32538
    
    var a = matcher.GC.getNeighbours(rawPoints(1)(0), rawPoints(1)(1), 200)
    
    pr(rawPoints(1)(0),rawPoints(1)(1),0)
    for(i <- 0 until a.length){
      pr(a(i).x,a(i).y,1)
    }
    
    var sp = a(0)
    println(sp)*/

  }
  
  def solveSeattle(gap:Int,noise:Double,matcher:MapMatch_hj){
    
    println("Time Gap: "+gap+" Noise: "+noise)
    

    
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

    //var pw = new PrintWriter(new File("e:/HM_MapMatching/Ground_Truth_Points.txt"))

    /*for(i <- 0 until GroundTruth.length){
      var s = GroundTruth(i)
      for(j <- 0 until matcher.GC.G.LinkId.length){
        var t = matcher.GC.G.LinkId(j)
        if(t!=null){
          while(t.last=='E'||t.last=='S') t=t.substring(0,t.length()-1)
          if(t.last=='-'){
            t=t.substring(0,t.length()-4)
            t=t+"-"
          }else{
            t=t.substring(0,t.length()-4)
            t=t+"+"
          }
          if(t==s){
            var re = matcher.GC.G.NormalToGeoLoc(matcher.GC.G.Point(j)(0))
            pw.write(re.y+","+re.x+"\r\n")
          }
        }
      }
    }
    pw.close()*/
    
    matcher.MEASUREMENT_STD = 4.07+noise
    matcher.BETA = 1.0*gap/log(2.0)*3*(1.0-sqrt(2)/2.0)/2.0
    matcher.NEIGHBOUR_RADIUS=50+3*(matcher.MEASUREMENT_STD.toInt+1)
    
    rawPoints = AddNoise(matcher,noise,rawPoints)
    
    var rawPointst = SampleRateFilter(rawPoints,gap)
    var rawPoints2 = PreProcessRawData(matcher,rawPointst,2.0*matcher.MEASUREMENT_STD)
    
    var pw = new PrintWriter(new File("e:/HM_MapMatching/Gapped_Point.txt" ))

    
    for(i <- 0 until rawPoints2.length){
      pw.write(rawPoints2(i)(1)+","+rawPoints2(i)(0)+"\r\n")

    }
    pw.close
    
    var re = matcher.getMatchedRoute(rawPoints2)
    println("Matched Route Found!")
    
    pw = new PrintWriter(new File("e:/HM_MapMatching/result.txt"))

    for(i <- 0 until re.length){
      //if(i>0) println(matcher.GC.getShortestRouteDistance(re(i-1),re(i)))
      //println(i+" "+re(i)+" LinkId:"+matcher.GC.G.LinkId(re(i).roadSegId))
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
      
      if(a(i).roadSegId!=(-1)){
        var t = matcher.GC.G.LinkId(a(i).roadSegId)
        while(t.last=='E'||t.last=='S') t=t.substring(0,t.length()-1)
        if(t.last=='-'){
          t=t.substring(0,t.length()-4)
          t=t+"-"
        }else{
          t=t.substring(0,t.length()-4)
          t=t+"+"
        }
        if(t!=end){
          RP.addLast(t)
          end=t
        }
      }

    }
    pw.close
    
    var ResultPath = ConvertToArray(RP)
    

    
    var LCSCal = new LCS
    var lcs = LCSCal.getResult(GroundTruth,ResultPath)
    
    var cnt = 0
    
    /*for(i <- 0 until ResultPath.length) {
      print(ResultPath(i))
      if(cnt<lcs.length) {
        print(" "+lcs(cnt))
        if(lcs(cnt)==ResultPath(i)) {
          print(" *")
          cnt=cnt+1
        }
      }
      println("")
    }*/
    
    //println(GroundTruth.length+" "+ResultPath.length+" "+lcs.length)

    println("Accuracy: "+(1.0-1.0*(GroundTruth.length+ResultPath.length-2*lcs.length)/GroundTruth.length))
    
    
    
    

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
  
  def getDirection(a:Double):vector = {
    var theta:Double = 1.0*a/32.0*2.0*Pi+Pi/2.0
    return new vector(cos(theta),sin(theta))
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
  
  def PreProcessRawData(matcher:MapMatch_hj,rawPoints:Array[Array[Double]],dis:Double):Array[Array[Double]]={
    var re = new LinkedList[Array[Double]]
    re.addLast(rawPoints(0))
    var pre = rawPoints(0)
    for(i <- 1 until rawPoints.length){
      if(matcher.GC.getDistance(pre, rawPoints(i))>1.0*dis){
        re.addLast(rawPoints(i))
        pre = rawPoints(i)
      }
    }
    
    var result = ConvertToArray(re)

    return result
  }
  
  def PreProcessRawData(matcher:MapMatch_hj,rawPoints:Array[GPSwD],dis:Double):Array[GPSwD]={
    var re = new LinkedList[GPSwD]
    re.addLast(rawPoints(0))
    var pre = Array(rawPoints(0).x,rawPoints(0).y)
    for(i <- 1 until rawPoints.length){
      if(matcher.GC.getDistance(pre, Array(rawPoints(i).x,rawPoints(i).y))>1.0*dis){
        re.addLast(rawPoints(i))
        pre = Array(rawPoints(i).x,rawPoints(i).y)
      }
    }
    
    var result = ConvertToArray(re)

    return result
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
  def ConvertToArray(a:LinkedList[Array[Double]]):Array[Array[Double]]={
    var re = new Array[Array[Double]](a.size)
    var p = 0
    var c = a.iterator()
    while(c.hasNext()){
      re(p)=c.next()
      p=p+1
    }
    return re
  }
    def ConvertToArray(a:LinkedList[GPSwD]):Array[GPSwD]={
    var re = new Array[GPSwD](a.size)
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