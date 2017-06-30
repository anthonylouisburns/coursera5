package observatory

import com.sksamuel.scrimage
import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.pixelLocation

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00*(1-x)*(1-y) + d10*x*(1-y) + d01*(1-x)*y + d11*x*y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val color = (v:Double)=>{Visualization.interpolateColor(colors, value=v)}

    val points:IndexedSeq[(Int,Int)] = for (
      x <- 0 to 255;
      y <- 0 to 255
    ) yield (x, y)
    val temps = points.map(p=>bilinearInterpolation(p._1/256, p._2/256,grid(x,y),grid(x,y+1),grid(x+1,y),grid(x+1,y+1)))
    val cs = temps.map(t=>color(t))

    val allPixels = cs.map(x=>Pixel(scrimage.Color(x.red,x.green,x.blue)))
    Image(256,256,allPixels.toArray)
  }

}
