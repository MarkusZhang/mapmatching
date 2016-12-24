

class DistanceCalc(roadSegs:Array[Array[Double]]) {
  /**
   * roadSegs will be something like
   * [
   * 		[start_lng,start_lat,end_lng,end_lat,road_id],
   *    ....
   * ]
   */
  //TODO: process the roadSegs into graph class construction
  
  /**
   * return the great circle distance between p1 and p2
   */
  def getDistance(p1:Array[Double],p2:Array[Double]): Double = {
    //TODO: implement
    1.0
  }
  
  /**
   * return the length of the shortest path from p1 to p2 along roads
   */
  def getShortestRouteDistance(p1:GeoPoint,p2:GeoPoint): Double ={
    //TODO: implement
    1.0
  }
  
}