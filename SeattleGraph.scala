package mapmatch
import java.io._
import java.util._
import scala.math._
import mapmatch._

class SeattleGraph extends BaseGraph{
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
      var mark = false
      var start = -1
      for(i <- 1 until vertexCnt){
        ss = new Scanner(P(i))
        var en = new vector(ss.nextDouble(),ss.nextDouble())
        if(length(en-pre)!=0.0){
          if(!mark){
            mark=true
            start = edgeCnt
          }
          var edgeID = base
          if(i<10) edgeID = base +"00"+i
          else if(i<100) edgeID = base + "0" + i
          else edgeID = base + i
          edgeID = edgeID 
          LinkId(edgeCnt) = edgeID + "+"
          LaneNum(edgeCnt) = 9999
          LinkCat(edgeCnt) = "!"
          ZoneId(edgeCnt) = "Don't Know"
          RoadName(edgeCnt) = "Don't Know"
          Node(edgeCnt) = 2
          Point(edgeCnt)(0)=new vector(pre.x,pre.y)
          Point(edgeCnt)(1)=new vector(en.x,en.y)
          //println(pre)
          edgeCnt = edgeCnt+1
          if(twoway>0){
            LinkId(edgeCnt) = edgeID + "-"
            LaneNum(edgeCnt) = 9999
            LinkCat(edgeCnt) = "!"
            ZoneId(edgeCnt) = "Don't Know"
            RoadName(edgeCnt) = "Don't Know"
            Node(edgeCnt) = 2
            Point(edgeCnt)(0) = new vector(en.x,en.y)
            Point(edgeCnt)(1) = new vector(pre.x,pre.y)
            edgeCnt=edgeCnt+1
          }
          pre = new vector(en.x,en.y)
        }
      }
      if(twoway>0&&(start!=(-1))){
        LinkId(start) = LinkId(start)+"S"
        LinkId(edgeCnt-2)=LinkId(edgeCnt-2)+"E"
        LinkId(edgeCnt-1) = LinkId(edgeCnt-1)+"S"
        LinkId(start+1)=LinkId(start+1)+"E"
      }else{
        if(start!=(-1)) {
          LinkId(start) = LinkId(start)+"S"
          LinkId(edgeCnt-1)=LinkId(edgeCnt-1)+"E"
        }
      }
      
      //println(s)
    }
    //println(edgeCnt)
    //println(maxi)
    n=edgeCnt
  }
  
  override def GraphProcessing() = {
    TotalNode = 2*n
    for(i <- 0 until 2*n){
      adj(i) = new LinkedList[pairII]
    }
    for(i <- 0 until n){
      NodeLoc(2*i) = Point(i)(0)
      NodeLoc(2*i+1) = Point(i)(1)
      adj(2*i).addLast(new pairII(2*i+1,i))
    }
    for(i <- 0 until n){
      var a = queryTop(Point(i)(1).x,Point(i)(1).y,1)
      var c = a.iterator
      var rs1 = LinkId(i)
      if(rs1.last=='E'){
        while(c.hasNext){
          var b = c.next()
          var t = LinkId(b.roadSegId)
          if(t.last=='E') t=t.substring(0,t.length()-1)
          if(b.t<1e-5&&NormalToMeter(b.dist)<0.0001&&t.last=='S'){
            adj(2*i+1).addLast(new pairII(2*b.roadSegId,-1))
          }
        }
        
      }else{
        if(rs1.last=='S') rs1=rs1.substring(0,rs1.length()-1)
        var flag = -1
        if(rs1.last=='-') flag=0
        if(rs1.last=='+') flag=1
        rs1=rs1.substring(0,rs1.length()-1)
        var no1 = rs1.substring(rs1.length()-3,rs1.length()).toInt
        rs1=rs1.substring(0,rs1.length()-3)
        while(c.hasNext){
          var b = c.next()
          var rs2 = LinkId(b.roadSegId)
          //println(rs2)
          while(rs2.last=='E'||rs2.last=='S') rs2=rs2.substring(0,rs2.length()-1)
          var flag2 = -2
          if(rs2.last=='+') flag2=1
          if(rs2.last=='-') flag2=0
          rs2=rs2.substring(0,rs2.length()-1)
          var no2 = rs2.substring(rs2.length()-3,rs2.length()).toInt
          rs2=rs2.substring(0,rs2.length()-3)
          if(b.t<1e-5&&NormalToMeter(b.dist)<0.0001&&rs1==rs2){
            if(flag==flag2){
              adj(2*i+1).addLast(new pairII(2*b.roadSegId,-1))
            }
          }
        }
      }

    }
    
  }
  override def PointGraphDistance(u:Int,ut:Double,v:Int,vt:Double):Double = {
    
    if(u==v&&ut<=vt){
      return (vt-ut)*(length(Point(u)(1)-Point(u)(0)))
    }
    //var ans:Double = GraphDistance(map.get(Point(u)(1)),map.get(Point(v)(0)))
    var ans = GraphDistance(2*u+1,2*v)
    var segu = Point(u)(1)-Point(u)(0)
    var segv = Point(v)(1)-Point(v)(0)
    ans+=(1.0-ut)*length(segu)
    ans+=vt*length(segv)
    return ans
  }
  
  override def PointGraphRoute(p1:GeoPoint,p2:GeoPoint):Array[GeoPoint] = {
    if(p1.roadSegId==p2.roadSegId){
      if(p1.t<=p2.t){
        return Array(p1,p2)
      }
    }
    //var a = GraphRoute(map.get(Point(p1.roadSegId)(1)),map.get(Point(p2.roadSegId)(0)))
    var a = GraphRoute(2*p1.roadSegId+1,2*p2.roadSegId)
    var re = new Array[GeoPoint](a.size()+2)
    re(0) = p1
    var pos = 1
    var c = a.iterator()
    while(c.hasNext()){
      var v = c.next()
      var g = NormalToGeoLoc(NodeLoc(v.a))
      re(pos) = new GeoPoint(g.x,g.y,0,v.b,0)
      pos=pos+1
    }
    re(pos) = p2
    return re
    
  }
  
    
  
  
  
}