package observatory.util

import observatory.Location

trait DistanceCalcular {
  def calculateDistanceInKilometer(location1: Location, location2: Location): Int
}

object DistanceCalculatorImpl extends DistanceCalcular {
  private val AVERAGE_RADIUS_OF_EARTH_KM = 6371
  override def calculateDistanceInKilometer(location1: Location, location2: Location): Int = {
    val latDistance = Math.toRadians(location1.lat - location2.lat)
    val lngDistance = Math.toRadians(location1.lon - location2.lon)
    val sinLat = Math.sin(latDistance / 2)
    val sinLng = Math.sin(lngDistance / 2)
    val a = sinLat * sinLat +
      (Math.cos(Math.toRadians(location1.lat)) *
        Math.cos(Math.toRadians(location2.lat)) *
        sinLng * sinLng)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    (AVERAGE_RADIUS_OF_EARTH_KM * c).toInt
  }
}