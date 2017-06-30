package observatory

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class ZoomTest extends AssertionsForJUnit {
  @Test
  def zoom_0(): Unit ={
    val l:Location = Interaction.tileLocation(0, 0, 0)
    println(l)
    assert(l.equals(Location(-Interaction.maxLat, -Interaction.maxLon)))
  }

  @Test
  def zoom_1_0_0(): Unit = {
    val l: Location = Interaction.tileLocation(1, 0, 0)
    println(l)
    assert(l.equals(Location(-Interaction.maxLat, -Interaction.maxLon)))
  }

  @Test
  def zoom_1_1_1(): Unit = {
    val l2:Location = Interaction.tileLocation(1, 1, 1)
    println(l2)
    assert(l2.equals(Location(0, 0)))
  }


  @Test
  def pixelLocation():Unit = {
    val br:Location = Interaction.tileLocation(1,1,1)
    val br2:Location = Interaction.tileLocation(9,256,256)
    val pix:Location = Interaction.pixelLocation(1,0,0,256,256)
    println(br)
    println(br2)
    println(pix)
    assert(br.equals(pix))
  }
}
