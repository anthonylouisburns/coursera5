package observatory

import com.sksamuel.scrimage
import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.log4j.{Level, Logger}
/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  @transient lazy val conf: SparkConf = new SparkConf()
    .setMaster("local")
    .setAppName("GlobalWarming")
    .set("spark.ui.port", "3080")
    .set("spark.driver.bindAddress", "127.0.0.1")
  @transient lazy val sc: SparkContext = new SparkContext(conf)
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  val p = 2
  val r_km =  6378.14 // 6356.752
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  //use RDD
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val temps:RDD[(Location, Double)] = sc.parallelize(temperatures.toList)
    distWeighted(distancesAndTemps(temps, location))
  }

  def predictTemperature(temperatures: RDD[(Location, Double)], location: Location): Double = {
    distWeighted(distancesAndTemps(temperatures, location))
  }

  //use RDD
//  def distancesAndTemps(temperatures: Iterable[(Location, Double)], location: Location): Iterable[(Double, Double)] = {
//    temperatures.map(e=>(distance(e._1, location), e._2))
////    distancesAndTemps(sc.parallelize(temperatures.toList), location)
//  }

  def distancesAndTemps(temperatures: RDD[(Location, Double)], location: Location): RDD[(Double, Double)] = {
    temperatures.map(e=>distanceAndTemp(location, e))
  }

  def distanceAndTemp(location:Location, tempLocation:(Location, Double)):(Double,Double) = {
    (distance(tempLocation._1, location), tempLocation._2)
  }

  //use RDD
//  def distWeighted(distTemps: Iterable[(Double, Double)]):Double={
////    distTemps.map(e=>e._2*weight(e._1)).sum / distTemps.map(e=>weight(e._1)).sum
//    distWeighted(sc.parallelize(distTemps.toIndexedSeq))
//  }

  def distWeighted(distTemps: RDD[(Double, Double)]):Double={
    distTemps.map(e=>weightedTemp(e._2,e._1)).sum / distTemps.map(e=>weight(e._1)).sum
  }

  //use RDD
//  def distWeighted(top:Double, bottom:Double, distTemps: List[(Double, Double)]):Double=distTemps match {
//    case Nil => top / bottom
//    case h::t => {
//      if (h._1 < 1) {
//        h._2
//      } else {
//        val w = weight(h._1)
//        distWeighted(top + (w * h._2), bottom + w, t)
//      }
//    }
//  }

  def weightedTemp(temp:Double, dist:Double):Double={
    temp * weight(dist)
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
    val ba = before_after(points, value)
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
        if (h._1 > a._1 && h._1 < value) before(h, t, value)
        else before(a, t, value)
      }else{
        if (h._1 < a._1) before(h, t, value)
        else before(a, t, value)
      }
    }
  }

  /**
alpha    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  //use RDD
//  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
//    val temps:RDD[(Location, Double)] = sc.parallelize(temperatures.toList)
//
//    val p = Pixel(0,0,0,0)
//    val points = for (
//      x <- -90 to 90;
//      y <- -180 to 180
//    ) yield (x, y)
//    val allTemps = points.map(x=>predictTemperature(temperatures, Location(x._1,x._2)))
//    val allColors = allTemps.map(x=>interpolateColor(colors, x))
//    val allPixels = allColors.map(x=>Pixel(scrimage.Color(x.red,x.green,x.blue)))
//    Image(361,181,allPixels.toArray)
//  }

  def visualize_old(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val temps:RDD[(Location, Double)] = sc.parallelize(temperatures.toList).cache()

    val points:RDD[(Int,Int)] = sc.parallelize(for (
      x <- -90 to 90;
      y <- -180 to 180
    ) yield (x, y)).cache()

    val p = points.collect()
    val allTemps:RDD[Double] = points.map(x=>predictTemperature(temps, Location(x._1,x._2)))
    val t = allTemps.collect()
    val allColors:RDD[Color] = allTemps.map(x=>interpolateColor(colors, x))
    val c = allColors.collect()
    val allPixels:RDD[Pixel] = allColors.map(x=>Pixel(scrimage.Color(x.red,x.green,x.blue)))
    Image(361,181,allPixels.collect())
  }

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image =  {
    val temps:RDD[(Location, Double)] = sc.parallelize(temperatures.toList)

    val xaxis = sc.parallelize(Range(-90,90))
    val yaxis = sc.parallelize(Range(-180,180))
    val points:RDD[(Int,Int)] = xaxis.cartesian(yaxis)
    val locations:RDD[Location] = points.map(x=>Location(x._1,x._2))
    val locations_x_temps:RDD[(Location, (Location, Double))] = locations.cartesian(temps)
    val locations_x_distance_and_temp:RDD[(Location, (Double, Double))] = locations_x_temps.map(x=>(x._1,distanceAndTemp(x._1, x._2)))
    val locations_x_w_top_bottom:RDD[(Location, (Double, Double))] = locations_x_distance_and_temp.map(x=>(x._1,(weightedTemp(x._2._1,x._2._1),weight(x._2._1))))
    val locations_w_top_bottom:RDD[(Location, (Double, Double))] = locations_x_w_top_bottom.reduceByKey((a,b)=>(a._1+b._1, a._2+b._2))
    val locations_w_temp:RDD[(Location, Double)] = locations_w_top_bottom.map(x=>(x._1,x._2._1/x._2._2))
    val allColors:RDD[(Location, Color)] = locations_w_temp.map(x=>(x._1,interpolateColor(colors, x._2)))
    val locationPixel:RDD[(Location, Pixel)] = allColors.map(x=>(x._1,Pixel(scrimage.Color(x._2.red,x._2.green,x._2.blue))))
    val pixels:RDD[Pixel] = locationPixel.map(x=>x._2)
    val img_pixels = pixels.collect()
    Image(361,181,img_pixels)
  }
}

