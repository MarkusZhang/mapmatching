import scala.collection.mutable.ArrayBuffer
import scala.math._
/**
  * Created by zhangjie on 12/22/2016.
  */
object MapMatch {

  val MEASUREMENT_STD = 4.07

  def main(args: Array[String]): Unit ={
    val points = Array(
      Array(1.0,1.0),
      Array(2.0,2.0),
      Array(3.0,3.0)
    )
    val matches = getBestMatch(points)
    matches.foreach(println)
  }

  

  /**\
    *
    * @param points array like [[103.0,1.0],[...]]
    * @return
    */
  def getBestMatch(points:Array[Array[Double]]): Array[GeoPoint]={
    var candidates = new Array[Array[GeoPoint]](points.length)
    var scores = new Array[Array[Double]](points.length)
    var parents = new Array[Array[Int]](points.length)

    // prepare candidates
    for(i <- points.indices){
      val p = points(i)
      val matches = getNeighbourPoints(p) //TODO: add graph as input
      candidates(i) = matches
      // initialize parents and scores
      parents(i) = new Array[Int](matches.length)
      scores(i) = new Array[Double](matches.length)
    }

    // initialize scores
    scores(0) = candidates(0).map(x=>getMeasurementProb(x.dist))

    // find most likely path
    for(j <- 1 until points.length){
      val candPoints = candidates(j)
      val prePoints = candidates(j-1)

      for(cur <- candPoints.indices){
        var max = -1.0
        val curPoint = candPoints(cur)
        // find the most likely parent of p
        for(pre <- prePoints.indices){
          val prePoint = prePoints(pre)
          val alt = scores(j-1)(pre) * getTransitionProb(prePoint,curPoint)
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

  def getNeighbourPoints(dataPoint:Array[Double]): Array[GeoPoint]={
    //TODO: remove dummy
    if (dataPoint(0)==1.0){
      Array(new GeoPoint(1,1,1.0,1),new GeoPoint(1,2,2.0,1))
    }
    else if(dataPoint(0)==2.0){
      Array(new GeoPoint(2,1,1.0,1),new GeoPoint(2,2,2.0,1))
    }
    else{
      Array(new GeoPoint(3,1,1.0,1),new GeoPoint(3,2,2.0,1))
    }
  }

  def getMeasurementProb(dist:Double): Double={
    val coff = 1 / (sqrt(2 * Pi) * MEASUREMENT_STD)
    val power = -0.5 * pow(dist/MEASUREMENT_STD,2)
    coff * exp(power)
  }

  def getTransitionProb(p1:GeoPoint,p2:GeoPoint): Double = {
    //TODO: remove dummy line
    1.0
  }

}
