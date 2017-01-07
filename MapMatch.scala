package mapmatch

import java.io.{FileInputStream, ObjectInputStream}

import scala.math._
/**
  * Created by zhangjie on 12/22/2016.
  */
class MapMatch(roadNetwork:Array[Array[Double]]) {
  // constructor
  val distanceCalc = new DistanceCalc(roadNetwork)
  val indexer = new SpatialIndex(roadNetwork)

  val MEASUREMENT_STD = 4.07
  val NEIGHBOUR_RADIUS = 100
  val BETA = 0.2
  val DEBUG = false

  def getMatchedRoute(rawPoints:Array[Array[Double]]): Array[GeoPoint] = {

    val matches = _getBestMatch(points = rawPoints,dCalc = distanceCalc,indexer=indexer)
    return matches
  }

  /**\
    * private function
    * @param points array like [[103.0,1.0],[...]]
    * @return
    */
  def _getBestMatch(points:Array[Array[Double]], dCalc:DistanceCalc, indexer:SpatialIndex): Array[GeoPoint]={
    var candidates = new Array[Array[GeoPoint]](points.length)
    var scores = new Array[Array[Double]](points.length)
    var parents = new Array[Array[Int]](points.length)

    // prepare candidates
    for(i <- points.indices){
      val p = points(i)
      if (p==null){
        println("======================================")
        println("point is null")
      }
      val matches = indexer.getNeighbours(p(0),p(1),NEIGHBOUR_RADIUS) //TODO: add graph as input
      candidates(i) = matches
      // initialize parents and scores
      parents(i) = new Array[Int](matches.length)
      scores(i) = new Array[Double](matches.length)
    }

    // initialize scores
    scores(0) = candidates(0).map(x=>_getMeasurementProb(x.dist))

    // find most likely path
    for(j <- 1 until points.length){
      val curRawPoint = points(j)
      val preRawPoint = points(j-1)
      val candPoints = candidates(j)
      val prePoints = candidates(j-1)

      for(cur <- candPoints.indices){
        var max = -1.0
        val curPoint = candPoints(cur)
        if (j==1 && DEBUG){
          println("Candidate point #" + cur + ":\n" + curPoint.toString)
        }
        // find the most likely parent of p
        for(pre <- prePoints.indices){
          val prePoint = prePoints(pre)
          val transitionProb = _getTransitionProb(prePoint,curPoint,preRawPoint,curRawPoint,dCalc)
          val alt = scores(j-1)(pre) * transitionProb
          if (j==1 && DEBUG){
            println("Pre point #" + pre + ":\n" + prePoint.toString)
            println("Transition prob: " + transitionProb)
            println("Sensor prob: " + scores(j-1)(pre))
            println("Product: " + alt)
          }
          if (alt > max){
            max = alt
            parents(j)(cur) = pre
          }
        }
        scores(j)(cur) = max * _getMeasurementProb(curPoint.dist)
      }
    }

    println("The final scores are: \n" + scores.map(_.mkString(" ")).mkString("\n"))
    println("The final parents are: \n" + parents.map(_.mkString(" ")).mkString("\n"))
    // construct the most likely path reversely
    var maxEnd = scores(scores.length-1).indexOf(scores(scores.length-1).max)
    val indexList = new Array[Int](points.length)
    for(i <- 1 until points.length){
      indexList(points.length-i) = maxEnd
      maxEnd = parents(points.length-i)(maxEnd)
    }
    indexList(0) = maxEnd

    // construct the array of points to return
    val result = new Array[GeoPoint](points.length)
    for(k <- indexList.indices){
      result(k) = candidates(k)(indexList(k))
    }
    return result
  }

//  def getNeighbourPoints(dataPoint:Array[Double]): Array[GeoPoint]={
//    //TODO: remove dummy
//    if (dataPoint(0)==1.0){
//      Array(new GeoPoint(1,1,1.0,1,1),new GeoPoint(1,2,2.0,1,1))
//    }
//    else if(dataPoint(0)==2.0){
//      Array(new GeoPoint(2,1,1.0,1,1),new GeoPoint(2,2,2.0,1,1))
//    }
//    else{
//      Array(new GeoPoint(3,1,1.0,1,1),new GeoPoint(3,2,2.0,1,1))
//    }
//  }

  def _getMeasurementProb(dist:Double): Double={
    val coff = 1 / (sqrt(2 * Pi) * MEASUREMENT_STD)
    val power = -0.5 * pow(dist/MEASUREMENT_STD,2)
    coff * exp(power)
  }

  def _getTransitionProb(candP1:GeoPoint, candP2:GeoPoint, rawP1:Array[Double], rawP2:Array[Double], dCalc:DistanceCalc): Double = {
    // get distance difference
    val rawDist = dCalc.getDistance(rawP1,rawP2)
    val candDist = dCalc.getShortestRouteDistance(candP1,candP2)
    val diff = abs(rawDist-candDist)

    // get probability
    return 1.0 / BETA * exp(-diff/BETA)
  }

}


