package observatory

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
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

