package observatory

import com.sksamuel.scrimage
import com.sksamuel.scrimage.{Image, Pixel}

object VizTest2 extends App {

  override def main(args: Array[String]) = {
//    out(viz)
//    t3()
    color_check()
//    t_old()
    //Color(191,0,64) (scale = List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), value = 0.25)
  }

  def color_check(): Unit ={
    //Incorrect predicted color: Color(127,0,127). Expected: Color(128,0,128) (scale = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255))), value = 1.0737418235E9)
    val c = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    val x = 1.0737418235E9
    val out = Visualization.interpolateColor(c,x)
    println(out)//Color(191,0,64)
  }

  def colors(): Iterable[(Double, Color)] = {
    val white = Color(255, 255, 255)
    val red = Color(255, 0, 0)
    val yellow = Color(255, 255, 0)
    val light_blue = Color(0, 255, 255)
    val dark_blue = Color(0, 0, 255)
    val violet = Color(255, 0, 255)
    val navy = Color(33, 0, 107)
    val black = Color(0, 0, 0)
    List((60, white), (32, red), (12, yellow), (0, light_blue), (-15, dark_blue), (-27, violet), (-50, navy), (-60, black))
  }

  def temps: Iterable[(Location, Double)] = List((Location(89.0, 0.0), 0.0),(Location(-89.0, 0.0), -15.0))

  def t(): Unit = {
    val img = Visualization.visualize(temps, colors)
    out(img)
  }

  def t_old(): Unit = {
    val img = VisualizationOld.visualize(temps, colors)
    out_old(img)
  }

  def test(): Unit = {
    val img = Visualization.visualize(temps, colors)
  }

  def out(img:Image): Unit ={
    img.output("/Users/aburns/src/anthony/coursera/scalaSpecialization/coursera5/observatory/some-image.png")
  }
  def out_old(img:Image): Unit ={
    img.output("/Users/aburns/src/anthony/coursera/scalaSpecialization/coursera5/observatory/some-image-old.png")
  }
  def t2(): Unit ={
    val n = Visualization.interpolateColor(colors, -7)
    println(n)
  }

  def t3(): Unit ={
    val l = Visualization.predictTemperature(temps, Location(-89,-180))
    println(l)
  }

  def predictTemperature(): Unit ={
    val l = Visualization.predictTemperature(temps, Location(-89,-180))
    println(l)
  }

  private def viz(): Image ={
    val black = Pixel(scrimage.Color(0,0,0))
    val white = Pixel(scrimage.Color(255,255,255))
    val points = for (
      x <- -90 to 90;
      y <- -180 to 180
    ) yield (x, y)
    val pixels = points.map(p=>{
      if(p._1*p._2>0)black
      else white
    })
    Image(361,181,pixels.toArray)
  }
}

