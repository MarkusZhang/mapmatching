package mapmatch

import java.util._

class GraphCal(var G:BaseGraph) {
  
  //var G = new BaseGraph
  //G.Init(roadSegs)
  
  def getNeighbours(px:Double,py:Double,radius:Double): Array[GeoPoint] = {
    //TODO: remove the dummy implementation
    var npx = px-G.lowx
    npx*=G.LongitudeNormalizeFactor
    npx/=G.wd
    var npy= py-G.lowy
    npy/=G.wd
    var range = (radius/G.wd/G.MeterPerDegree+1.0).toInt
    var re = G.queryTop(npx,npy,range)
    var a = new LinkedList[GeoPoint]
    var i = 0
    for(i <- 0 until re.size){
      var p = G.NormalToGeoLoc(new vector(re(i).x,re(i).y))
      
      re(i).x = p.x
      re(i).y = p.y
      re(i).dist = G.NormalToMeter(re(i).dist)
      if(re(i).dist<=radius) a.addLast(re(i))
    }
    
    var b = new Array[GeoPoint](a.size)
    
    var c = a.iterator()
    var pos:Int = 0
    while(c.hasNext()){
      b(pos)=c.next()
      pos=pos+1
    }
    
    return b
  }
  
  
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
  
  def getRouteDetail(p1:GeoPoint,p2:GeoPoint):Array[GeoPoint] = {
    return G.PointGraphRoute(p1,p2)
  }
  
  
}