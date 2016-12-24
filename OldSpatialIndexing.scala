//import org.apache.spark._
import scala.io.Source
import java.io._
import java.util._
import scala.math._
object SpatialIndexing {
  val sourceFilename = "LTA.txt"
  val MAXN = 60000
  var LinkId = new Array[Int](MAXN)
  var LinkCat = new Array[String](MAXN)
  var LaneNum = new Array[Int](MAXN)
  var ZoneId = new Array[Int](MAXN)
  var RoadName = new Array[String](MAXN)
  var Node = new Array[Int](MAXN)
  var Point = Array.ofDim[vector](MAXN,2)
  var n = 0
  val divx = 500
  val range = 5
  var Table = new Array[LinkedList[Int]]((divx+1)*(divx+1))
  var divy = 100
  
  var highx = 0.0
  var lowx = 0.0
  var highy = 0.0
  var lowy = 0.0
  var gapx = 0.0
  var gapy = 0.0
  var wd = 0.0
  
  var i = 0
  var j = 0
  var k = 0
  
  class vector(var x:Double, var y:Double){
    override def toString: String = x+" "+y
    def +(that:vector): vector = new vector(x+that.x,y+that.y)    
    def -(that:vector): vector = new vector(x-that.x,y-that.y)
  }
  
  def cross(a:vector,b:vector):Double = a.x*b.y-a.y*b.x
  def dot(a:vector,b:vector):Double = a.x*b.x+a.y*b.y
  def length(a:vector):Double = sqrt(dot(a,a))
  def getDistanceToLine(p:vector,a:vector,b:vector):Double = {
    var v1 = b-a
    var v2 = p-a
    return abs(cross(v1,v2))/length(v1)
  }
  
  def getDistanceToSegment(p:vector,a:vector,b:vector):Double = {
    var v1 = b-a
    var v2 = p-a
    var t = dot(v2,v1) / length(v1)
    if(t<=0.0) return length(v2)
    if(t>=1.0) return length((p-b))
    return getDistanceToLine(p,a,b)
  }
  
  def isSegmentProperIntersection(a1:vector,a2:vector,b1:vector,b2:vector):Boolean = {
    var c1 = cross(a2-a1,b1-a1)
    var c2 = cross(a2-a1,b2-a1)
    var c3 = cross(b2-b1,a1-b1)
    var c4 = cross(b2-b1,a2-b1)
    return c1*c2<=0.0&&c3*c4<=0.0
  }
  
  def inBox(x:Double,y:Double,p:vector):Boolean = {
    return p.x>=x&&p.x<=x+1.0&&p.y>=y&&p.y<=y+1.0
  }
  
  def belong(x:Int,y:Int,a1:vector,a2:vector):Boolean = {
    var re:Boolean = false
    re=re||inBox(x,y,a1)
    re=re||inBox(x,y,a2)
    re=re||isSegmentProperIntersection(new vector(x,y),new vector(x+1,y),a1,a2)
    re=re||isSegmentProperIntersection(new vector(x,y),new vector(x,y+1),a1,a2)
    re=re||isSegmentProperIntersection(new vector(x+1,y),new vector(x+1,y+1),a1,a2)
    re=re||isSegmentProperIntersection(new vector(x,y+1),new vector(x+1,y+1),a1,a2)
    return re
  }
  
  def index() = {
    for(i <- 0 until (divx+1)*(divx+1)){
      Table(i) = new LinkedList[Int]
    }
    for(i <- 0 until n){
      var lx:Int = (min(Point(i)(0).x,Point(i)(1).x)).toInt
      var hx:Int = (max(Point(i)(0).x,Point(i)(1).x)).toInt
      var ly:Int = (min(Point(i)(0).y,Point(i)(1).y)).toInt
      var hy:Int = (max(Point(i)(0).y,Point(i)(1).y)).toInt
      var total:Int = 0;
      var cnt:Int = 0;
      for(j <- lx to hx;k <- ly to hy){
        //total=total+1
        if(belong(j,k,Point(i)(0),Point(i)(1))) {
          //cnt=cnt+1
          Table(k*(divx+1)+j).addLast(i)
        }
      }
      //println(total+" "+cnt)
    }
    
    /*for(i <- 0 until (divx+1)*(divx+1)){
      println(Table(i).size())
    }*/
  }
  
  def check(x:Int,y:Int):Boolean = x>=0&&x<divx+1&&y>=0&&y<divx+1
  
  def querySet(x:Double,y:Double):TreeSet[Int] = {
    var bx = x.toInt
    var by = y.toInt
    var s = new TreeSet[Int]
    for(i <- bx-range to bx+range;j <- by-range to range+by){
       if(check(i,j)){
         s.addAll(Table(j*(divx+1)+i))
       }
    }
    return s
  }
  
  def querySize(x:Double,y:Double):Int = querySet(x,y).size()
  
  def queryMini(x:Double,y:Double):Double = {
    var ans:Double = 100.0
    var s = querySet(x,y)
    var c = s.iterator()
    while(c.hasNext()){
      var p = c.next()
      var re = getDistanceToSegment(new vector(x,y),Point(p)(0),Point(p)(1))
      ans=min(ans,re)
    }
    return ans
  }
  
  class dis_ind(var d:Double, var ind:Int){
    def <(that:dis_ind):Boolean = d<that.d
    override def toString():String = d+" "+RoadName(ind)+" "+LinkId(ind)
  }
  
  def queryTop(x:Double,y:Double):Array[dis_ind] = {
    var s = querySet(x,y)
    var c = s.iterator()
    var size:Int = s.size()
    var a = new Array[dis_ind](size)
    var pos = 0 
    while(c.hasNext()){
      var p = c.next()
      var re = getDistanceToSegment(new vector(x,y),Point(p)(0),Point(p)(1))
      a(pos) = new dis_ind(re,p)
      pos=pos+1
    }
    a=a.sortWith(_<_)
    return a
  }
  
  
  def readInput() = {
    var in = new Scanner(new File(sourceFilename))
    //var in = new Scanner(System.in)
    n = in.nextInt()

    var i:Int = 0
    var j=i;
    var k=i;
    for(i <- 0 until n){
      LinkId(i) = in.nextInt()
      LinkCat(i) = in.next
      LaneNum(i) = in.nextInt()
      ZoneId(i) = in.nextInt()
      RoadName(i) = in.nextLine()
      RoadName(i) = RoadName(i).substring(1,RoadName(i).length())
      //println(RoadName(i))
      Node(i) = in.nextInt()
      for(j <- 0 until Node(i)){
        Point(i)(j) = new vector(in.nextDouble(),in.nextDouble())
        //Point(i)(j).x = in.nextDouble()
        //Point(i)(j).y = in.nextDouble()
      }
    }     
  }

  
  
  def normalize() = {
    highx = Point(0)(0).x
    highy = Point(0)(0).y
    lowx=highx
    lowy=highy
    for(i <- 0 until n; j <- 0 until Node(i)){
      highx = max(highx,Point(i)(j).x)
      lowx = min(lowx,Point(i)(j).x)
      highy = max(highy,Point(i)(j).y)
      lowy=min(lowy,Point(i)(j).y)
    }
    wd = highx-lowx
    wd /= divx
    gapx = highx - lowx
    gapy = highy - lowy
    //println(lowx+" "+highx+" "+lowy+" "+highy)
    for(i <- 0 until n;j <- 0 until Node(i)){
      Point(i)(j).x-=lowx
      Point(i)(j).x/=wd
      Point(i)(j).y-=lowy
      Point(i)(j).y/=wd
    }
    
    /*for(i <- 0 until n){
      println(Point(i)(0))
    }*/
    
  }
  
  def bruteforceQuery(a:Double,b:Double):Double = {
    var p = new vector(a,b)
    var ans = 100.0
    for(i <- 0 until n){
      ans = min(ans,getDistanceToSegment(p,Point(i)(0),Point(i)(1)))
    }
    return ans
  }
  
  def main(args: Array[String]){
    readInput()
    println("Readinput Complete!!")
    normalize()
    println("Normalize Complete!!")
    index()
    println("Index Complete!!")
    var cin = new Scanner(System.in)
    var x=0.0
    var y=0.0
    while(true){
      x=cin.nextDouble()
      y=cin.nextDouble()
      var re = queryTop(x,y)
      for(i <- 0 until re.length){
        println(re(i))
      }
      println(queryMini(x,y))
      println(bruteforceQuery(x,y))
    }
  }
}