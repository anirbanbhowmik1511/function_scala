package observatory

import org.apache.spark.sql.SparkSession
import java.nio.file.Paths
import org.apache.spark.sql.types._
import org.apache.spark.sql._
import scala.util.Try
import org.apache.log4j.{ Level, Logger }
import java.nio.file.Files
import java.nio.file.Path
//import org.scalameter._

object Main extends App {

  var sparkSession = SparkSession.builder().appName("observatory").master("local[4]").getOrCreate()
  var stationColumns = List(("stn", StringType), ("wban", StringType), ("latitude", DoubleType), ("longitude", DoubleType))
  var tempColumns = List(("stn", StringType), ("wban", StringType), ("month", IntegerType), ("day", IntegerType), ("temp", DoubleType))

  val colorData = List(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))).toIterable

  
  val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  val avgs = Extraction.locationYearlyAverageRecords(temps)
  //println("calculating Image...")
  //val image = Visualization.visualize(avgs, colorData)
  //image.output(new java.io.File("sample.png"))
  //println("Done!")
  println(avgs.size)
  
  val coordinates = (for(i <-  90 until -90 by -1; j <- -180 until 180) yield (i, j)).toSeq.par
  
  
  println("memo done")
  
//  val time = config(
//
//    Key.exec.benchRuns -> 1,
//
//    Key.verbose -> true) withWarmer {
//
//      new Warmer.Default
//
//    } withMeasurer {
//      new Measurer.Default
//    } measure {
//      //val gridFunction = Manipulation.preStore(avgs)
//      val gridFunction = Manipulation.makeGrid(avgs)
//      gridFunction(GridLocation(80, 80))
//      //println(temperature)
//      //println("Printing Images Done ")
//    }
//
//  println(s"total time for yearly average = $time")

  def generateImages(year: Year, tile: Tile, avgtemp: Iterable[(Location, Temperature)]): Unit = {
    val path = Paths.get(s"target/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png")
    Files.createDirectories(path.getParent)
    Interaction.tile(avgtemp, colorData, tile).output(new java.io.File(path.toUri()))
  }

  def initialize(): Unit = {
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    sparkSession = SparkSession.builder().appName("observatory").master("local[4]").getOrCreate()
    stationColumns = List(("stn", StringType), ("wban", StringType), ("latitude", DoubleType), ("longitude", DoubleType))
    tempColumns = List(("stn", StringType), ("wban", StringType), ("month", IntegerType), ("day", IntegerType), ("temp", DoubleType))
  }

  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): DataFrame = {
    val stationDf = readFlatDfFromFile(stationsFile, stationColumns).na.drop().cache()
    val tempDf = readFlatDfFromFile(temperaturesFile, tempColumns).na.drop().cache()
    val joinedDf = stationDf.join(tempDf, List("stn", "wban"), "inner").select("month", "day", "latitude", "longitude", "temp")
    joinedDf
  }

  def readFlatDfFromFile(path: String, typeList: List[(String, DataType)]): DataFrame = {
    val rdd = sparkSession.sparkContext.textFile(fsPath(path))
    val schema = dfSchema(typeList)
    val rowRdds = rdd.map(_.split(",", -1).toList).map(toRow(typeList, _))
    sparkSession.createDataFrame(rowRdds, schema)
  }

  def toCelsius(f: Double): Double = (f - 32) * 5 / 9

  def toRow(typeList: List[(String, DataType)], data: List[String]): Row = {
    val values = typeList.map(_._2).zip(data).map(x => x._1 match {
      case IntegerType => x._2.toInt
      case DoubleType  => Try(x._2.toDouble).getOrElse(Double.NaN)
      case _           => x._2
    })
    Row(values: _*)
  }

  def dfSchema(typeList: List[(String, DataType)]): StructType = {
    val fields = typeList.map(x => StructField(x._1, x._2))
    StructType(fields)
  }

  def fsPath(resource: String): String = {
    Paths.get(getClass.getResource(resource).toURI).toString
  }

}
