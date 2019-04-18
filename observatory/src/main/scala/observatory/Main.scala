package observatory

import org.apache.spark.sql.SparkSession
import java.time.LocalDate
import java.nio.file.Paths
import org.apache.spark.sql.types._
import org.apache.spark.sql._
import scala.util.Try

object Main extends App {
  
  val sparkSession = SparkSession.builder().appName("observatory").master("local[4]").getOrCreate()
  val stationColumns = List(("stn", StringType),("wban", StringType),("latitude", DoubleType),("longitude", DoubleType))
  val tempColumns = List(("stn", StringType),("wban", StringType),("month", IntegerType),("day", IntegerType), ("temp", DoubleType))
  
  locateTemperatures(2015, "/stations.csv", "/2015.csv")
  
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Unit = {
    val stationDf = readFlatDfFromFile(stationsFile, stationColumns).na.drop()
    val tempDf = readFlatDfFromFile(temperaturesFile, tempColumns).na.drop()
    tempDf.show()
    stationDf.show()
  }
  
  def readFlatDfFromFile(path : String, typeList: List[(String, DataType)]) : DataFrame = {
    val rdd = sparkSession.sparkContext.textFile(fsPath(path))
    val schema = dfSchema(typeList)
    val rowRdds = rdd.map(_.split(",", -1).toList).map(toRow(typeList, _))
    sparkSession.createDataFrame(rowRdds, schema)
  }
  
  def toRow(typeList : List[(String, DataType)], data: List[String]): Row = {
    val values = typeList.map(_._2).zip(data).map(x => x._1 match {
      case IntegerType => x._2.toInt
      case DoubleType => Try(x._2.toDouble).getOrElse(Double.NaN)
      case _ => x._2
    })
    Row(values: _*)
  }
  
  def dfSchema(typeList : List[(String, DataType)]) : StructType = {
    val fields = typeList.map(x => StructField(x._1, x._2))
    StructType(fields)
  }
  
  def fsPath(resource: String): String = {
    Paths.get(getClass.getResource(resource).toURI).toString
  }

}
