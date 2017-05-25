package observatory

case class Location(lat: Double, lon: Double){
  def latRad:Double = Math.toRadians(lat)
  def lonRad:Double = Math.toRadians(lon)
}

case class Color(red: Int, green: Int, blue: Int)

