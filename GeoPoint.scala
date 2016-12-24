

class GeoPoint(var x:Double,var y:Double,var dist:Double,var roadSegId:Int) {
    override def toString: String = x+" "+y + " dist:" + dist
  }