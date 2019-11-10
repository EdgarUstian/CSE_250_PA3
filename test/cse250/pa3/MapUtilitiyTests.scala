package cse250.pa3

import cse250.objects.{StreetGraph, TaxEntry}
import cse250.pa3.MapUtilities
import org.scalatest.{BeforeAndAfter, FlatSpec}


class MapUtilitiyTests extends FlatSpec with BeforeAndAfter {
  "Find" should "Work" in {
    val graph = new StreetGraph
    for(i <- 0 until 100)
      for (j <- 1 to 5) {
        graph.insertEdge(i.toString,(i+j).toString)
        graph.insertEdge((i+j).toString,i.toString)
      }
    val src = new TaxEntry
    src.infoMap.addOne("STREET"->"0")
    val dest = new TaxEntry
    dest.infoMap.addOne("STREET"->"100")
    assert(MapUtilities.computeFewestTurns(graph,src,dest) == 20)
    // Probably not 20 but the goal is to see how long it takes to run
  }

  behavior of "FewestTurns"
  it should "return 0 for same property in an empty graph" in {
    val taxentryFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
    val entries = TaxEntry.loadEntries(taxentryFilename, 25)
    for (entry <- entries) {
      assert(MapUtilities.computeFewestTurns(new StreetGraph, entry, entry) == 0)
    }
  }

  behavior of "FewestTurnsList"
  it should "return an empty list for the same property" in {
    val taxentryFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
    val entries = TaxEntry.loadEntries(taxentryFilename, 25)
    for (entry <- entries) {
      assert(MapUtilities.computeFewestTurnsList(new StreetGraph, entry, entry) == List(entry.infoMap("STREET").toString))
    }
  }

  behavior of "FewestTurns"
  it should "complete this task efficiently" in {
    val graph = new StreetGraph
    val upper = 20000
    for (i <- 0 until upper)
      for (j <- 1 to upper/20) {
        graph.insertEdge(i.toString,(i+j).toString)
        graph.insertEdge((i+j).toString,i.toString)
      }

    val src = new TaxEntry
    val dest = new TaxEntry
    src.infoMap.addOne("STREET","0")
    dest.infoMap.addOne("STREET",upper.toString)
    assert(MapUtilities.computeFewestTurns(graph,src,dest) == 20)
  }
}

