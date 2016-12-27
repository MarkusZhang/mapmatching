package mapmatch

import java.io.{FileInputStream, ObjectInputStream}

import scala.math._
/**
  * Created by zhangjie on 12/22/2016.
  */
object MapMatch {

  val MEASUREMENT_STD = 4.07
  val NEIGHBOUR_RADIUS = 100
  val BETA = 0.2

  def main(args: Array[String]): Unit ={
    // read road segments
    val ois = new ObjectInputStream(new FileInputStream("D:/fyp/LTA_serialized.txt"))
    val arr = ois.readObject.asInstanceOf[Array[Array[Double]]]
    ois.close()

    // init distance calc and spatial index
    val distanceCalc = new DistanceCalc(arr)
    val indexer = new SpatialIndex(arr)

    val points = Array(
      Array(103.70737552659193,1.3414189015502869),
      Array(103.70917797104994,1.3416226935619133),
      Array(103.71087312714735,1.340678814628501),
      Array(103.71293306367079,1.3418157596625286),
      Array(103.71481060998121,1.3428025417165426)
    )
    val matches = getBestMatch(points = points,dCalc = distanceCalc,indexer=indexer)
    matches.foreach(println)
  }

  

  /**\
    *
    * @param points array like [[103.0,1.0],[...]]
    * @return
    */
  def getBestMatch(points:Array[Array[Double]],dCalc:DistanceCalc,indexer:SpatialIndex): Array[GeoPoint]={
    var candidates = new Array[Array[GeoPoint]](points.length)
    var scores = new Array[Array[Double]](points.length)
    var parents = new Array[Array[Int]](points.length)

    // prepare candidates
    for(i <- points.indices){
      val p = points(i)
      val matches = indexer.getNeighbours(p(0),p(1),NEIGHBOUR_RADIUS) //TODO: add graph as input
      candidates(i) = matches
      // initialize parents and scores
      parents(i) = new Array[Int](matches.length)
      scores(i) = new Array[Double](matches.length)
    }

    // initialize scores
    scores(0) = candidates(0).map(x=>getMeasurementProb(x.dist))

    // find most likely path
    for(j <- 1 until points.length){
      val curRawPoint = points(j)
      val preRawPoint = points(j-1)
      val candPoints = candidates(j)
      val prePoints = candidates(j-1)

      for(cur <- candPoints.indices){
        var max = -1.0
        val curPoint = candPoints(cur)
        // find the most likely parent of p
        for(pre <- prePoints.indices){
          val prePoint = prePoints(pre)
          val alt = scores(j-1)(pre) * getTransitionProb(prePoint,curPoint,preRawPoint,curRawPoint,dCalc)
          if (alt > max){
            max = alt
            parents(j)(cur) = pre
          }
        }
        scores(j)(cur) = max * getMeasurementProb(curPoint.dist)
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

  def getMeasurementProb(dist:Double): Double={
    val coff = 1 / (sqrt(2 * Pi) * MEASUREMENT_STD)
    val power = -0.5 * pow(dist/MEASUREMENT_STD,2)
    coff * exp(power)
  }

  def getTransitionProb(candP1:GeoPoint, candP2:GeoPoint, rawP1:Array[Double], rawP2:Array[Double],dCalc:DistanceCalc): Double = {
    // get distance difference
    val rawDist = dCalc.getDistance(rawP1,rawP2)
    val candDist = dCalc.getDistance(Array(candP1.x,candP1.y),Array(candP2.x,candP2.y))
    val diff = abs(rawDist-candDist)

    // get probability
    return 1.0 / BETA * exp(-diff/BETA)
  }

}
