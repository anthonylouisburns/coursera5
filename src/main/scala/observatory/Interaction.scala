package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  def maxLat = -85.0511287798066

  def maxLon = 180

  /**
    * @param zoom Zoom level
    * @param x_lon    X coordinate
    * @param y_lat    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x_lon: Int, y_lat: Int): Location = {
    val n = maxTileDim(zoom)
    //    lon_deg = xtile / n * 360.0 - 180.0
    val long_deg = (x_lon.toDouble / n * 360) - 180
    //    lat_rad = arctan(sinh(π * (1 - 2 * ytile / n)))
    val lat_rad_deg = Math.PI * (1 - (2 * y_lat.toDouble / n))
    val lat_rad = Math.atan(Math.sinh(lat_rad_deg))
    //    lat_deg = lat_rad * 180.0 / π
    val lat_deg = lat_rad * (180.0 / Math.PI)
    Location(lat = lat_deg, lon = long_deg)
  }

  def maxTileDim(zoom: Int): Int = {
    Math.pow(2, zoom).toInt
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x_lon            X coordinate
    * @param y_lat            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x_lon: Int, y_lat: Int): Image = {
    val points:IndexedSeq[(Int,Int)] = for (
      x <- 0 to 255;
      y <- 0 to 255
    ) yield (x, y)
    val locations:IndexedSeq[(Int,Int)] = points.map(p=>p)
    Visualization.pointsToImage(256,256,locations,temperatures,colors)
  }

  def pixelLocation(zoom: Int, x_parent: Int, y_parent: Int, x: Int, y: Int): Location = {
    val multiplier = Math.pow(2, 8).toInt
    tileLocation(zoom + 8, multiplier * x_parent + x, multiplier * y_parent + y)
  }


  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    ???
  }

}
