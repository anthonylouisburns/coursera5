package observatory

import com.sksamuel.scrimage
import com.sksamuel.scrimage.{Image, Pixel}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite {


  test("img test")(
    assert(viztoFile() == 0)
  )

  private def viztoFile() = {
    val myImage = viz()
    myImage.output("/Users/aburns/src/anthony/coursera/scalaSpecialization/coursera5/observatory/some-image.png")
    0
  }

  private def viz(): Image ={
    val p = Pixel(scrimage.Color(0,255,0))
    val points = for (
      x <- -180 to 180;
      y <- -90 to 90
    ) yield (x, y)
    Image(361,181,points.map(e=>p).toArray)
  }
}
