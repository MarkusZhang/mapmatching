package mapmatch

class DistanceCalc(roadSegs:Array[Array[Double]]) {
  /**
   * roadSegs will be something like
   * [
   * 		[start_lng,start_lat,end_lng,end_lat,road_id],
   *    ....
   * ]
   */
  //TODO: process the roadSegs into graph class construction
   
  var G = new Graph
  G.Init(roadSegs)

  /**
   * return the great circle distance between p1 and p2 (in meter)
   */
  def getDistance(p1:Array[Double],p2:Array[Double]): Double = {
    //TODO: implement
    var p1x = p1(0)-G.lowx
    var p2x = p2(0)-G.lowx
    var p1y = p1(1)-G.lowy
    var p2y = p2(1)-G.lowy
    var u = new vector(p1x,p1y)
    var v = new vector(p2x,p2y)
    return G.length(u-v)*G.MeterPerDegree
  }
  /**
   * return the length of the shortest path from p1 to p2 along roads
   */
  def getShortestRouteDistance(p1:GeoPoint,p2:GeoPoint): Double = {
    //TODO: implement
    return G.NormalToMeter(G.PointGraphDistance(p1.roadSegId,p1.t,p2.roadSegId,p2.t))
  }
  
}