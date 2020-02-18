package observatory

import Interaction._
import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    *
    *         d00  d10
    *         d01  d11
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {

    def linearInterpolation(distance: Double, d0: Temperature, d1: Temperature): Double = {
      d0*distance + d1*(1-distance)
    }

    linearInterpolation(point.y, linearInterpolation(point.x, d00, d10)
                                ,linearInterpolation(point.x, d01, d11))
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val pixels = for {
      x <- tile.x*256 to tile.x*256 + 255
      y <- tile.y*256 to tile.y*256 + 255
    } yield {
      val color = interpolateColor(colors, bilinearInterpolation(CellPoint(x, y),
        grid(tileLocation(Tile(tile.x*256, tile.y*256, tile.zoom+8))),
        grid(tileLocation(Tile(tile.x*256, tile.y*256 + 255, tile.zoom+8))),
        grid(tileLocation(Tile(tile.x*256 + 255, tile.y*256, tile.zoom+8))),
        grid(tileLocation(Tile(tile.x*256 + 255, tile.y*256 + 255, tile.zoom+8))),
      ))
      Pixel(color.red, color.green, color.blue, 127)
    }

    Image(256, 256, pixels.toArray)
  }

  implicit def gridLocationToLocation(gridLocation: GridLocation): Location = {
    Location(gridLocation.lat, gridLocation.lon)
  }

  implicit def locationToGridLocation(location: Location): GridLocation = {
    GridLocation(location.lat.toInt, location.lon.toInt)
  }
}
