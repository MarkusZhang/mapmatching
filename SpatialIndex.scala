
import java.io._
import java.util._
import scala.math._

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
      re(i).x = G.NormalToReal(re(i).x)
      re(i).y = G.NormalToReal(re(i).y)
      re(i).dist = G.NormalToReal(re(i).dist)
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