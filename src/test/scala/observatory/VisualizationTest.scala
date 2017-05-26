package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite {
  test("one plus one is two")(assert(1 + 1 == 2))


  test("confirm distance")(
    assert(boulder_to_waterloo.floor == 14515)
  )

  def boulder_to_waterloo():Double = {
    //http://www.dtcenter.org/met/users/docs/write_ups/gc_simple.pdf
    //    A Boulder, Colorado 40.0167 lon, 105.2833 lat
    //    B Wallaroo, Australia −33.9333 lon, -137.65 lat
    //    14520
    val A = Location(40.0167, 105.2833)
    val B = Location(-33.9333, -137.65)
    Visualization.distance(A,B)
  }

  test("confirm color")(
    assert(midColor(50) == Color(50,50,50))
  )

  test("confirm color out of range over")(
    assert(midColor(150) == Color(100,100,100))
  )

  test("confirm color out of range under")(
    assert(midColor(0) == Color(100,100,100))
  )

  private def midColor(v:Double) = {
    val a = (0D, Color(0, 0, 0))
    val b = (100D, Color(100, 100, 100))
    Visualization.interpolateColor(Iterable(a,b),v)
  }
}
