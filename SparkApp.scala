package mapmatch

import java.io.{FileInputStream, ObjectInputStream}

import mapmatch.MapMatch
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

object SparkApp {

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
    val ois = new ObjectInputStream(new FileInputStream("D:/fyp/LTA_serialized.txt"))
    val roadNetwork = ois.readObject.asInstanceOf[Array[Array[Double]]]
    ois.close()
    return new MapMatch(roadNetwork)
  }

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)
    // preprocess data
    val file = sc.textFile("D:/fyp/raw_data/SG_origindata/OriginTaxiData_20150701.csv")
    val processedData = file.map(line=>line.split(","))
      .map(x=>(x(0),Array(x(1).toDouble,x(2).toDouble)))
      .reduceByKey(_ ++ _) //TODO: can we reduce to an array of array
      .map(x=>(x._1,arrToArrOfArr(x._2))) // convert each list to array of (lng,lat) pairs
      .take(10)
    // do matching
    val matcher = getMapMatcher
    //val nullList = processedData.filter(x=>checkContainNull(x._2))
    val matches = processedData
      .map(x=>(x._1,matcher.getMatchedRoute(x._2)))
      .take(10)
    matches.foreach(println)
  }
}