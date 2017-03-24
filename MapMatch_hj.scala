package mapmatch

import java.util._
import scala.math._

class MapMatch_hj(G:SingaporeGraph) {
  val GC = new GraphCal(G)
  
  var MEASUREMENT_STD = 4.07
  var NEIGHBOUR_RADIUS = 100
  var BETA = 0.2
  
  def getMatchedRoute(rawPoints:Array[Array[Double]]): Array[GeoPoint] = {

    val matches = _getBestMatch(Points = rawPoints)
    return matches
  }
  
  class pairII(var a:Int= -1,var b:Int= -1)
  
  def _getBestMatch(Points:Array[Array[Double]]):Array[GeoPoint] = {
    var candidates = new Array[Array[GeoPoint]](Points.length)
    var scores = new Array[Array[Double]](Points.length)
    var parents = new Array[Array[pairII]](Points.length)
    for(i <- 0 until Points.length){
      val p = Points(i)
      if (p==null){
        println("======================================")
        println("point is null")
      }
      candidates(i) = GC.getNeighbours(p(0),p(1),NEIGHBOUR_RADIUS)
      if(candidates(i).length==0){
        println("No candidate point find for the point "+i)
      }      
      // initialize parents and scores
      parents(i) = new Array[pairII](candidates(i).length)
      scores(i) = new Array[Double](candidates(i).length)
    }
            

    var st = 0
    while(st<candidates.length&&candidates(st).length==0) st=st+1
    if(st>=candidates.length) return new Array[GeoPoint](0)
    for(i <- 0 until scores(st).length){
      scores(st)(i) = _getMeasurementProb(candidates(st)(i).dist)
      parents(st)(i) = new pairII(-1,-1)
    }
    var pre = st
    for( i <- st+1 until candidates.length){
      //println("Finding Route At Point "+i)
      //println(GC.getDistance(Points(i-1),Points(i)))
      var flag=false
      for(j <- 0 until candidates(i).length){
        scores(i)(j) = -1000000000000000.0
        parents(i)(j)= new pairII(-1,-1)
        for(k <- 0 until candidates(pre).length){
          var t = _getTransitionProb(candidates(pre)(k),candidates(i)(j),Points(pre),Points(i))
          if(t> -1000000000.0){
            var ts = scores(pre)(k) + t
            flag=true
            if(scores(i)(j)<ts){
              scores(i)(j)=ts
              parents(i)(j)=new pairII(pre,k)
            }
          }

        }
        scores(i)(j) = scores(i)(j) + _getMeasurementProb(candidates(i)(j).dist)
      }
      if(flag) {
        pre=i
      }else{
        println("Impossible Point at "+i)
      }
    }
    
    var End = scores.length-1
    
    var maxEnd = -1
    
    while(End>=st&&(scores(End).length==0||parents(End)(scores(End).indexOf(scores(End).max)).a == -1)){
      End=End-1
    }
    if(End<st) return new Array[GeoPoint](0)
    maxEnd=scores(End).indexOf(scores(End).max)

    var index = new Array[Int](Points.length)

    var p = maxEnd
    pre = End
    while(pre!= -1){
      index(pre) = p
      var t = p
      p=parents(pre)(t).b
      pre = parents(pre)(t).a
    }
    var result = new LinkedList[GeoPoint]
    pre = End
    while(pre!= -1){
      result.addFirst(candidates(pre)(index(pre)))
      pre=parents(pre)(index(pre)).a
    }
    
    var re = new Array[GeoPoint](result.size)
    var c = result.iterator()
    pre = 0
    while(c.hasNext){
      re(pre)=c.next()
      pre=pre+1
    }
    
    return re
  }
  
  def getMatchedRouteDetail(Points:Array[GeoPoint]):Array[GeoPoint] = {
    if(Points.length==0) return new Array[GeoPoint](0)
    var a = new LinkedList[GeoPoint]
    a.addLast(Points(0))
    for(i <- 0 until Points.length-1){
      //println("Processing Matched Route Detail At Point "+i)
      //println(GC.getShortestRouteDistance(Points(i),Points(i+1)))
      var b = GC.getRouteDetail(Points(i),Points(i+1))
      for(j <- 1 until b.length){
        a.addLast(b(j))
      }
    }
    var re = new Array[GeoPoint](a.size())
    var pos = 0
    var c = a.iterator()
    while(c.hasNext()){
      var temp = c.next()
      re(pos) = new GeoPoint(temp.x,temp.y,temp.dist,temp.roadSegId,temp.t)
      pos=pos+1
    }
    return re
  }
  

  
  def _getMeasurementProb(dist:Double): Double={
    val coff = 1 / (sqrt(2 * Pi) * MEASUREMENT_STD)
    val power = -0.5 * pow(dist/MEASUREMENT_STD,2)
    //coff * exp(power)
    return log(coff) + power
  }

  def _getTransitionProb(candP1:GeoPoint, candP2:GeoPoint, rawP1:Array[Double], rawP2:Array[Double]): Double = {
    // get distance difference
    val rawDist = GC.getDistance(rawP1,rawP2)
    //var t = System.nanoTime()
    val candDist = GC.getShortestRouteDistance(candP1,candP2)
    //t = System.nanoTime()-t
    //println(t/1e9)
    val diff = abs(rawDist-candDist)

    // get probability
    //return 1.0 / BETA * exp(-diff/BETA)
    return log(1.0/BETA) + (-diff/BETA)
    //return log(rawDist/candDist)
  }
  
  def GenerateNoise(t:Double):vector = {
    val r = scala.util.Random
    var l = r.nextGaussian()
    if(l<0.0) l=0.0-l
    l*=t
    var th = r.nextDouble()
    th*= 2.0*Pi
    var x = l*sin(th)
    var y = l*cos(th)
    y/=GC.G.MeterPerDegree
    x/=GC.G.MeterPerDegree
    x/=GC.G.LongitudeNormalizeFactor
    return new vector(x,y)
  }
  
}