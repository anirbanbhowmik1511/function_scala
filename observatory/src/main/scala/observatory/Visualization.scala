package observatory

import com.sksamuel.scrimage.{ Image, Pixel }
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParSeq
import scala.collection.GenSeq
import scala.collection.GenIterable

/**
 * 2nd milestone: basic visualization
 */
object Visualization {

  /**
   * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
   * @param location Location where to predict the temperature
   * @return The predicted temperature at `location`
   */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val parSeq = temperatures.toSeq.par
    predictTemperaturePar(parSeq, location)
  }
 

  /**
   * @param points Pairs containing a value and its associated color
   * @param value The value to interpolate
   * @return The color that corresponds to `value`, according to the color scale defined by `points`
   */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val (left, right) = points.toIndexedSeq.sortBy(x => x._1).span(x => x._1 <= value)
    if(left.isEmpty) right.head._2
    else if(right.isEmpty) left.last._2
    else interpolate(left.last, right.head, value)
  }

  /**
   * @param temperatures Known temperatures
   * @param colors Color scale
   * @return A 360×180 image where each pixel shows the predicted temperature at its location
   */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val parTemps = temperatures.toSeq.par
    val coordinates = (for(i <-  90 until -90 by -1; j <- -180 until 180) yield (i, j)).toSeq.par
    parVisualize(parTemps, colors, coordinates)
  }
  
  def parVisualize(temperatures: ParSeq[(Location, Temperature)], colors: Iterable[(Temperature, Color)], cords : ParSeq[(Int, Int)]): Image = {
    val pixels = cords.map(x => {
      val temp = predictTemperaturePar(temperatures, Location(x._1, x._2))
      val color = interpolateColor(colors, temp)
      color   
    })
    val image = Image(360, 180, pixels.map(x => Pixel(x.red, x.green, x.blue, 255)).toArray)
    image
  }

  def getTemperature(distances: GenIterable[((Location, Temperature), Distance)], pValue: Int) = {
    distances.map(x => (1 / math.pow(x._2, pValue)) * x._1._2).sum / distances.map(x => 1 / math.pow(x._2, pValue)).sum
  }

  def greatCircleDistance(l1: Location, l2: Location): Distance = {
    val deltaLamda = math.abs(l1.lon.toRadians - l2.lon.toRadians)
    val earthRadius = 6371 //in km
    val deltaRho = if (l1 == l2) 0
    else if (isAntipode(l1, l2)){
      math.Pi
    }
    else math.acos((math.sin(l1.lat.toRadians) * math.sin(l2.lat.toRadians)) + (math.cos(l1.lat.toRadians) * math.cos(l2.lat.toRadians) * math.cos(deltaLamda)))
    earthRadius * deltaRho
  }

  def isAntipode(l1: Location, l2: Location) = {
    (-l1.lat == l2.lat) && ((l1.lon + 180 == l2.lon) || (l1.lon - 180 == l2.lon))
  }

  def interpolate(t0: (Temperature, Color), t1: (Temperature, Color), value: Temperature) : Color = {
    val factor = (value - t0._1) / (t1._1 - t0._1)
    t0._2 + ((t1._2 - t0._2) multConst factor)
  }
  
    def predictTemperaturePar(temperatures: GenIterable[(Location, Temperature)], location: Location): Temperature = {
    val pValue = 5
    val distances = temperatures.map(x => (x, greatCircleDistance(x._1, location)))
    val closest = distances.find(x => x._2 < 1)
    closest match {
      case Some(x) => {
        x._1._2
      }
      case None    => {
        getTemperature(distances, pValue)
      }
    }
  }

}

