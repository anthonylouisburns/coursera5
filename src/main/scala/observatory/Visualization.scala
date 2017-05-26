package observatory

import com.sksamuel.scrimage
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val p = 2
  val r_km =  6378.14 // 6356.752
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    distWeighted(distancesAndTemps(temperatures, location))
  }

  def distancesAndTemps(temperatures: Iterable[(Location, Double)], location: Location): Iterable[(Double, Double)] = {
    temperatures.map(e=>(distance(e._1, location), e._2))
  }

  def distWeighted(distTemps: Iterable[(Double, Double)]):Double={
    distTemps.map(e=>e._2*weight(e._1)).sum / distTemps.map(e=>weight(e._1)).sum
  }

  def distWeighted(top:Double, bottom:Double, distTemps: List[(Double, Double)]):Double=distTemps match {
    case Nil => top / bottom
    case h::t => {
      if (h._1 < 1) {
        h._2
      } else {
        val w = weight(h._1)
        distWeighted(top + (w * h._2), bottom + w, t)
      }
    }
  }

  def weight(dist:Double):Double={
    1/(Math.pow(dist,p))
  }

  def distance(one: Location, two: Location):Double={
    val delta_lon = one.lonRad - two.lonRad
    val cos_lon = Math.cos(delta_lon)
    val cos_1 = Math.cos(one.latRad)
    val cos_2 = Math.cos(two.latRad)
    val sin1 = Math.sin(one.latRad)
    val sin2 = Math.sin(two.latRad)
    val radians = Math.acos((sin1 * sin2) + (cos_1 * cos_2 * cos_lon))
    radians * r_km
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val ba= before_after(points, value)
    if(ba._2._1 - ba._1._1 == 0) return ba._1._2
    val bweight = (value - ba._1._1)/(ba._2._1 - ba._1._1)
    val aweight = 1-bweight
    val bcolor = ba._1._2
    val acolor = ba._2._2
    Color(((bcolor.red*bweight)+(acolor.red*aweight)).toInt,
      ((bcolor.green*bweight)+(acolor.green*aweight)).toInt,
      ((bcolor.blue*bweight)+(acolor.blue*aweight)).toInt)
  }


  def before_after(points: Iterable[(Double, Color)], value: Double): ((Double, Color),(Double, Color))={
    (before(points.head, points.tail,value),after(points.head, points.tail,value))
  }

  def after(a:(Double, Color), points: Iterable[(Double, Color)], value: Double): (Double, Color) = points match{
    case Nil => a
    case h::t => {
      if(a._1 > value) {
        if (h._1 < a._1 && h._1 > value) after(h, t, value)
        else after(a, t, value)
      }else{
        if (h._1 > a._1) after(h, t, value)
        else after(a, t, value)
      }
    }
  }

  def before(a:(Double, Color), points: Iterable[(Double, Color)], value: Double): (Double, Color) = points match{
    case Nil => a
    case h::t => {
      if(a._1 < value) {
        if (h._1 > a._1 && h._1 < value) after(h, t, value)
        else after(a, t, value)
      }else{
        if (h._1 < a._1) after(h, t, value)
        else after(a, t, value)
      }
    }
  }

  /**
alpha    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val p = Pixel(0,0,0,0)
    val points = for (
      x <- -180 to 180;
      y <- -90 to 90
    ) yield (x, y)
    val alltemps = points.map(x=>predictTemperature(temperatures, Location(x._1,x._2)))
    val allColors = alltemps.map(x=>interpolateColor(colors, x))
    val allPixels = allColors.map(x=>Pixel(scrimage.Color(x.red,x.green,x.blue)))
    Image(361,181,allPixels.toArray)
  }

}

