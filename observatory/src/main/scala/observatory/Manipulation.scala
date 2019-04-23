package observatory

import scala.collection.parallel.ParSeq
import observatory.Visualization._
import scala.collection.GenSeq


/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  val cache = scala.collection.mutable.Map[GridLocation, Temperature]()
  
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val parTemp = temperatures.toSeq.par
    //parMakeGrid(parTemp)
    preStore(parTemp)
  }
  
  def parMakeGrid(temperatures: GenSeq[(Location, Temperature)]) : GridLocation => Temperature = {
    memoGrid(temperatures)((a, g: GridLocation) => predictTemperaturePar(a, Location(g.lat, g.lon)))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
//    val parTemps = temperaturess.toSeq.par.map(x => x.toSeq.par)
//    memoGrid(parTemps)((a, g: GridLocation) => a.map(parMakeGrid(_)(g)).sum / a.size)
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    ???
  }
  
  def memoGrid[A](a: A)(f: (A,GridLocation) => Temperature): GridLocation => Temperature = {    
    g => {
      if(cache.contains(g)){
        println("cache hit")
        cache(g)
      }
      else {
        val res = f(a, g)
        cache += (g -> res)
        res
      }
    }
  }
  
  def preStore(temperatures: ParSeq[(Location, Temperature)]): GridLocation => Temperature = {
    val coordinates = (for(i <-  90 until -90 by -1; j <- -180 until 180) yield (i, j)).toSeq.par
    val map = coordinates.map(x => (GridLocation(x._1, x._2), predictTemperaturePar(temperatures, Location(x._1, x._2)))).toMap
    (g: GridLocation) => map(g)
  }

}

