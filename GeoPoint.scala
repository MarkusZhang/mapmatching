

class GeoPoint(var x:Double,var y:Double,var dist:Double,var roadSegId:Int,var t:Double) {
    override def toString: String = x+" "+y + " dist:" + dist+" ratio:"+t
    def < (that:GeoPoint):Boolean = dist<that.dist
  }