package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.util.DistanceCalculatorImpl
import Visualization._
import observatory.Interaction.zoomTile

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    /*
      import math
      def num2deg(xtile, ytile, zoom):
        n = 2.0 ** zoom
        lon_deg = xtile / n * 360.0 - 180.0
        lat_rad = math.atan(math.sinh(math.pi * (1 - 2 * ytile / n)))
        lat_deg = math.degrees(lat_rad)
        return (lat_deg, lon_deg)
     */
    /*val n = math.pow(2.0, tile.zoom)
    val lon_deg = tile.x / n * 360.0 - 180.0
    val lat_rad = math.atan(math.sinh(math.Pi * (1 - 2 * tile.y / n)))
    val lat_deg = math.toDegrees(lat_rad)

    Location(lat_deg, lon_deg)*/

    Location(toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1<<tile.zoom))))),
      tile.x.toDouble / (1<<tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)],
           colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val pixels = for {
      x <- tile.x*256 to tile.x*256 + 255
      y <- tile.y*256 to tile.y*256 + 255
    } yield {
      val color = interpolateColor(colors, predictTemperature(temperatures, tileLocation(Tile(x, y, tile.zoom + 8))))
      Pixel(color.red, color.green, color.blue, 127)
    }

    Image(256, 256, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    yearlyData.foreach(data => {
      (Tile(0,0,0) +: zoomTile(Tile(0,0,0), 3)).toList
        .foreach(tile => { generateImage(data._1, tile, data._2)})
    })
  }

  def zoomTile(tile: Tile, depth: Int): Seq[Tile] = {
    if (depth <= 0) {
      Seq()
    } else {
      for {
        y <- tile.y * 2 to tile.y * 2 + 1
        x <- tile.x * 2 to tile.x * 2 + 1
      } yield {
        Tile(x, y, tile.zoom + 1) +: zoomTile(Tile(x, y, tile.zoom + 1), depth-1)
      }
      }.flatten
  }

}
