

class SpatialIndex(roadSegs:Array[Array[Double]]) {
  //TODO: process the roadSegs into graph upon class construction
  
  /**
   * return an array of GeoPoints that are within the given radius of (px,py)
   */
  def getNeighbours(px:Double,py:Double,radius:Double): Array[GeoPoint] ={
    //TODO: remove the dummy implementation
    Array(
      new GeoPoint(130,1,1,1)
    )
  }
}