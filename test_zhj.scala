
import java.io._
import java.util._
import scala.math._
import mapmatch._
object test {
  var G = new Graph
  G.OldInit()
  
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
  def main(args:Array[String]):Unit = {
    var cin = new Scanner(System.in)
    while(true){
      var x:Double = 0.0
      var y = 0.0
      var r = 0.0
      x=cin.nextDouble()
      y=cin.nextDouble()
      r=cin.nextDouble()
      var re = getNeighbours(x,y,r)
      //println(re.length)
      var i=0;
      for(i<- 0 until re.length){
        println(re(i))
        println(G.RoadName(re(i).roadSegId)+" "+G.Point(re(i).roadSegId)(0)+" "+G.Point(re(i).roadSegId)(1))
        println()
      }
    }
    
  }
}
