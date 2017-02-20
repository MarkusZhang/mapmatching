package mapmatch

class roadSeg(var a:vector,var b:vector,var id:String) extends java.io.Serializable {
  override def toString:String = ""+a+" "+b+" "+id
}