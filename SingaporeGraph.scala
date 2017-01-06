import java.io._
import java.util._
import scala.math._
import mapmatch._

class SingaporeGraph extends BaseGraph{
  sourceFile = "e:/taxi/LTA.txt"
  override def ReadInputFromFile()={
    var in = new Scanner(new File(sourceFile))
    //var in = new Scanner(System.in)
    n = in.nextInt()

    var i:Int = 0
    var j=i;
    var k=i;
    for(i <- 0 until n){
      LinkId(i) = in.next()
      LinkCat(i) = in.next
      LaneNum(i) = in.nextInt()
      ZoneId(i) = in.next()
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
}