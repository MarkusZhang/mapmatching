package mapmatch

class vector(var x:Double, var y:Double) extends java.io.Serializable{
  override def toString: String = x+" "+y
  def +(that:vector): vector = new vector(x+that.x,y+that.y)    
  def -(that:vector): vector = new vector(x-that.x,y-that.y)
  def *(a:Double): vector = new vector(x*a,y*a)
}