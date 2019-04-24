package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.collection.parallel.ParSeq

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.getLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val parTemp = temperatures.toSeq.par
    val size = 256
    val coordinates = (for (i <- 0 until size; j <- 0 until size) yield (i, j)).toSeq.par
    val z = 8
    val pixels = coordinates.map { x => 
      val t = Tile((math.pow(2, z).toInt * tile.x) + x._2, (math.pow(2, z).toInt * tile.y) + x._1, tile.zoom + z)
      val location = tileLocation(t)
      val temp = Visualization.predictTemperaturePar(parTemp, location)
      val color = Visualization.interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 127)
    }
    Image(size, size, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val tiles = generateTilesByZoom(3)
    yearlyData.par.foreach(d => {
      tiles.foreach(t => generateImage(d._1, t, d._2))
    })
  }
  
  
  def generateTilesByZoom(zoom : Int): ParSeq[Tile] = {
    val res = for(z <- 0 to zoom; i <- 0 until math.pow(2, z).toInt; j <- 0 until math.pow(2, z).toInt) yield {
      Tile(i, j, z)
    }
    res.toSeq.par
  }

}
