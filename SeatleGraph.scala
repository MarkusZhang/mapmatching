package mapmatch
import java.io._
import java.util._
import scala.math._
import mapmatch._

class SeatleGraph extends BaseGraph{
  sourceFile = "e:/HM_MapMatching/road_network.txt"
  override def ReadInputFromFile() = {
    var in = new Scanner(new File(sourceFile))
    in.nextLine()
    //var in = new Scanner(System.in)
    var s = ""
    var base=""
    var node1 = ""
    var node2 = ""
    var twoway = 0
    var speed = 0.0
    var vertexCnt = 0
    var lineString = ""
    var maxi = 0
    edgeCnt = 0
    while(in.hasNext()){
      base = in.next()
      node1 = in.next()
      node2 = in.next()
      twoway = in.nextInt()
      speed = in.nextDouble()
      vertexCnt = in.nextInt()
      maxi = max(maxi,vertexCnt)
      lineString = in.nextLine()
      lineString = lineString.substring(12,lineString.length()-1)
      //println(lineString)
      var P = lineString.split(",")
      var ss = new Scanner(P(0))
      var pre = new vector(ss.nextDouble(),ss.nextDouble())
      for(i <- 1 until vertexCnt){
        var edgeID = base
        if(i<10) edgeID = base +"00"+i
        else if(i<100) edgeID = base + "0" + i
        else edgeID = base + i
        LinkId(edgeCnt) = edgeID
        LaneNum(edgeCnt) = 9999
        LinkCat(edgeCnt) = "!"
        ZoneId(edgeCnt) = "Don't Know"
        RoadName(edgeCnt) = "Don't Know"
        Node(edgeCnt) = 2
        ss = new Scanner(P(i))
        var en = new vector(ss.nextDouble(),ss.nextDouble())
        Point(edgeCnt)(0)=new vector(pre.x,pre.y)
        Point(edgeCnt)(1)=new vector(en.x,en.y)
        pre=en
        //println(pre)
        edgeCnt = edgeCnt+1
        if(twoway>0){
          Point(edgeCnt)(0) = new vector(en.x,en.y)
          Point(edgeCnt)(1) = new vector(pre.x,pre.y)
          edgeCnt=edgeCnt+1
        }
      }
      //println(s)
    }
    //println(edgeCnt)
    //println(maxi)
    n=edgeCnt
  }
  
  
  
}