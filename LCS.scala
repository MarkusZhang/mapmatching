package mapmatch

import java.util._
import scala.math._

class LCS {
  var dp:Array[Array[Int]] = new Array(0)
  var t1 = Array("")
  var t2 = Array("")
  var re = new LinkedList[String]
  def getResult(a:Array[String],b:Array[String]):Array[String]={
    t1=a
    t2=b
    var n = t1.length
    var m = t2.length
    dp = new Array(n+1)
    for(i <- 0 until n+1){
      dp(i) = new Array(m+1)
      for(j <- 0 until m+1){
        dp(i)(j) = -1
      }
    }
    g(n,m)
    
    var t = new Array[String](re.size)
    var c = re.iterator
    var p = 0
    while(c.hasNext){
      t(p)=c.next()
      p=p+1
    }
    return t
  }
  
  def f(a:Int,b:Int):Int = {
    if(a==0||b==0) return 0
    if(dp(a)(b)!=(-1)) return dp(a)(b)
    if(t1(a-1)==t2(b-1)) dp(a)(b) = f(a-1,b-1)+1
    dp(a)(b) = max(dp(a)(b),f(a,b-1))
    dp(a)(b) = max(dp(a)(b),f(a-1,b))
    return dp(a)(b)
  }
  
  def g(a:Int,b:Int):Unit={
    if(f(a,b)==0) return
    if(t1(a-1)==t2(b-1)){
      g(a-1,b-1)
      re.addLast(t1(a-1))
    }else{
      if(f(a,b-1)>f(a-1,b)){
        g(a,b-1)
      }else{
        g(a-1,b)
      }
    }
  }
  
}