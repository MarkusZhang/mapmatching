package mapmatch



class GeoPoint(var x:Double,var y:Double,var dist:Double,var roadSegId:Int,var t:Double) {
    override def toString: String = x+","+y + " dist:" + dist+" ratio:"+t + " segId: " + roadSegId
    def < (that:GeoPoint):Boolean = dist<that.dist

    def getCoord():Array[Double] = {
      return Array(x,y)
    }
  }