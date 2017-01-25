package mapmatch

import java.util._
import scala.math._

class MapMatch_hj(G:BaseGraph) {
  val GC = new GraphCal(G)
  
  val MEASUREMENT_STD = 4.07
  val NEIGHBOUR_RADIUS = 100
  val BETA = 0.2
  
  def getMatchedRoute(rawPoints:Array[Array[Double]]): Array[GeoPoint] = {

    val matches = _getBestMatch(Points = rawPoints)
    return matches
  }
  
  def _getBestMatch(Points:Array[Array[Double]]):Array[GeoPoint] = {
    var candidates = new Array[Array[GeoPoint]](Points.length)
    var scores = new Array[Array[Double]](Points.length)
    var parents = new Array[Array[Int]](Points.length)
    for(i <- 0 until Points.length){
      val p = Points(i)
      if (p==null){
        println("======================================")
        println("point is null")
      }
      candidates(i) = GC.getNeighbours(p(0),p(1),NEIGHBOUR_RADIUS)
      // initialize parents and scores
      parents(i) = new Array[Int](candidates(i).length)
      scores(i) = new Array[Double](candidates(i).length)
    }
    
    //scores(0) = candidates(0).map(x=>_getMeasurementProb(x.dist))
        
    for(i <- 0 until scores(0).length){
      scores(0)(i) = _getMeasurementProb(candidates(0)(i).dist)
      parents(0)(i) = -1
    }
    
    for( i <- 1 until Points.length){
      for(j <- 0 until Points(i).length){
        scores(i)(j) = -1000000000.0
        for(k <- 0 until Points(i-1).length){
          var ts = scores(i-1)(k) + _getTransitionProb(candidates(i)(j),candidates(i-1)(k),Points(i),Points(i-1));
          if(scores(i)(j)<ts){
            scores(i)(j)=ts
            parents(i)(j)=k
          }
        }
        scores(i)(j) = scores(i)(j) + _getMeasurementProb(candidates(i)(j).dist)
      }
    }
    
    var maxEnd = scores(scores.length-1).indexOf(scores(scores.length-1).max)
    val index = new Array[Int](Points.length)
    
    var p = maxEnd
    for(i <- 1 to Points.length){
      index(Points.length-i) = p
      p=parents(Points.length-i)(p)
    }
    
    var result = new Array[GeoPoint](Points.length)
    for(i <- 0 until Points.length){
      result(i) = candidates(i)(index(i))
    }
    
    return result
    
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
    val candDist = GC.getShortestRouteDistance(candP1,candP2)
    val diff = abs(rawDist-candDist)

    // get probability
    //return 1.0 / BETA * exp(-diff/BETA)
    return log(1.0/BETA) + (-diff/BETA)
  }
}