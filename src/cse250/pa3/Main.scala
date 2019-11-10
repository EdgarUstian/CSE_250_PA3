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
    //    entries.foreach(entry => println(entry.infoMap("STREET")))
    val mapXMLFile = "data/buffalo-map"
    val intersectionNodeXMLFile = "data/export.osm"
    val intersectionIDs = MapUtilities.loadIntersectionIDs(intersectionNodeXMLFile)
    //    intersectionIDs.foreach(id => println(id))
    val nodeToStreetMapping = MapUtilities.loadMapInfo(mapXMLFile)
    /*
    nodeToStreetMapping.keys.foreach{id =>
      println("ID: " + id + " ---" + " Streets: " + nodeToStreetMapping(id))
    }
    */
    val streetGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)

    //    streetGraph.edges.foreach(edge => println(edge))

    //    println(s"${entries(1).infoMap("STREET")} to ${entries(2).infoMap("STREET")}")
    //    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(2)))
    //    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(2)))

    //    println(s"${entries(3).infoMap("STREET")} to\n${entries(18).infoMap("STREET")}")
    //    println(MapUtilities.computeFewestTurns(streetGraph, entries(3), entries(18)))
    //    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(3), entries(18)))

    //    println(s"${entries(1).infoMap("STREET")} to\n${entries(1).infoMap("STREET")}")
    //    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(1)))
    //    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(1)))

    //    streetGraph.vertices.foreach(vertex => println(vertex))

    entries.foreach{entry1 =>
      entries.foreach{entry2 =>
        println("-------------------")
        println(entry1.infoMap("STREET") + " --> " + entry2.infoMap("STREET"))
        println(MapUtilities.computeFewestTurns(streetGraph, entry1, entry2))
        println(MapUtilities.computeFewestTurnsList(streetGraph, entry1, entry2))
      }
    }
  }
}

