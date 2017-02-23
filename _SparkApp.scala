package mapmatch

import java.io.{FileInputStream, ObjectInputStream}

import mapmatch.MapMatch
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

object SparkApp {

  val SPARK_URL = "spark://155.69.148.21:7078"
  val SOURCE_FILE = "hdfs://155.69.148.21:9002/users/staff/tongpr/taxi_data.csv"
  val RESULT_FILE = "hdfs://155.69.148.21:9002/users/staff/tongpr/mapmatch_result"

  def arrToArrOfArr(arr: Array[Double]): Array[Array[Double]] = {
    var result = new Array[Array[Double]](arr.length / 2)
    for (i <- arr.indices){
      if (i % 2 ==0){
        result(i/2) = Array(arr(i),arr(i+1))
      }
    }
    return result
  }

  def checkContainNull(arr:Array[Array[Double]]): Boolean = {
    for (item <- arr){
      if (item == null){
        return true
      }
    }
    return false
  }

  def getMapMatcher:MapMatch = {
    val ois = new ObjectInputStream(new FileInputStream("/users/staff/tongpr/LTA_serialized.txt"))
    val roadNetwork = ois.readObject.asInstanceOf[Array[Array[Double]]]
    ois.close()
    return new MapMatch(roadNetwork)
  }

  def startMapMatching(sc:SparkContext): Unit ={
    val matcher = getMapMatcher
    // preprocess data
    val file = sc.textFile(SOURCE_FILE)

    val processedData = file.map(line=>line.split(","))
      .map(x=>(x(0),Array(x(1).toDouble,x(2).toDouble)))
      .reduceByKey(_ ++ _) //TODO: can we reduce to an array of array
      .map(x=>(x._1,matcher.getMatchedRoute(arrToArrOfArr(x._2))))
        .saveAsTextFile(RESULT_FILE)// convert each list to array of (lng,lat) pairs
    // do matching
    println("Finished preprocessing")
//    val matches = processedData
//      .map(x=>(x._1,matcher.getMatchedRoute(x._2)))
//      .reduceByKey(_ ++ _)
//      .saveAsTextFile(RESULT_FILE)
  }


  def main(args: Array[String]) {
    //System.setProperty("hadoop.home.dir", "C:/hadoop")
    val conf = new SparkConf().setAppName("Mapmatch")//.setMaster(SPARK_URL)
    val sc = new SparkContext(conf)
    startMapMatching(sc)
    sc.stop()
  }
}