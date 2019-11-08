/**
 * Main.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxEntry}

object Main {
  def main(args: Array[String]): Unit = {
    val taxentryFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
    val entries = TaxEntry.loadEntries(taxentryFilename, 25)
    val mapXMLFile = "data/buffalo-map"
    val intersectionNodeXMLFile = "data/export.osm"
    val intersectionIDs = MapUtilities.loadIntersectionIDs(intersectionNodeXMLFile)
//    intersectionIDs.foreach(id => println(id))
    val nodeToStreetMapping = MapUtilities.loadMapInfo(mapXMLFile)
    /*nodeToStreetMapping.keys.foreach{id =>
      println("ID: " + id + " ---" + " Streets: " + nodeToStreetMapping(id))
    }*/
    val streetGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)
//    println(s"${entries(1)} to\n${entries(2)}")
//    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(2)))
//    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(2)))

    println(s"${entries(3)} to\n${entries(24)}")
    println(MapUtilities.computeFewestTurns(streetGraph, entries(3), entries(24)))
//    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(3), entries(24)))


  }
}

