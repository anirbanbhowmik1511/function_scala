package observatory

import com.sksamuel.scrimage.{Image, Pixel}

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
    val pixels = for (i <- 0 until 256; j <- 0 until 256) yield {
      val t = Tile((math.pow(2, 8).toInt * tile.x) + j, (math.pow(2, 8).toInt * tile.y) + i, tile.zoom + 8)
      val location = tileLocation(t)
      val temp = Visualization.predictTemperature(temperatures, location)
      val color = Visualization.interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 127)
    }
    Image(256, 256, pixels.toArray)
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
    yearlyData.par.foreach(d => {
      generateTilesByZoom(2).par.foreach(t => generateImage(d._1, t, d._2))
    })
  }
  
  
  def generateTilesByZoom(zoom : Int): IndexedSeq[Tile] = {
    val res = for(z <- 0 to zoom; i <- 0 until math.pow(2, z).toInt; j <- 0 until math.pow(2, z).toInt) yield {
      Tile(i, j, z)
    }
    res
  }

}
