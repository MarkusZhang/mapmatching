package mapmatch

import java.util._

class SpatialIndex(roadSegs:Array[Array[Double]]) {
  //TODO: process the roadSegs into graph upon class construction
  
  var G = new Graph
  G.Init(roadSegs)

  /**
   * return an array of GeoPoints that are within the given radius of (px,py)
   */
  def getNeighbours(px:Double,py:Double,radius:Double): Array[GeoPoint] = {
    //TODO: remove the dummy implementation
    var npx = px-G.lowx
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
}