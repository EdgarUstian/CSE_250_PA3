/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: edgarust
 * Person#: 50230866
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT: edgarust
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxEntry}
import scala.collection.mutable
import scala.xml.{NodeSeq, XML}
import scala.util.control.Breaks

object  MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] = {
    var intersections: mutable.Set[String] = mutable.Set[String]()
    val nodes: NodeSeq = xml.XML.loadFile(filename) \\ "@id"
    nodes.foreach(node => intersections += node.toString())
    intersections
  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {
    val deMap: mutable.Map[String, mutable.Set[String]] = mutable.Map[String, mutable.Set[String]]()
    val deWay: NodeSeq = xml.XML.loadFile(filename) \\ "way"
    for(way <- deWay){
       for(tag <- way \ "tag"){
         if((tag \ "@k").text == "tiger:name_base"){
           for(ref <- way \\ "@ref"){
             val id: String = ref.toString()
             if(deMap.contains(id)){
               deMap(id).addOne((tag \ "@v").toString().toUpperCase)
             }
             else{
               deMap.addOne(id -> mutable.Set((tag \ "@v").toString().toUpperCase))
             }
           }
         }
       }
    }
    deMap
  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {
    val streetGraph = new StreetGraph
    for((id, streets) <- nodeToStreetMapping){
      if(intersectionIDs.contains(id) && streets.size > 1){
        streets.foreach{street =>
//            streetGraph.insertVertex(street)
          streets.foreach(road => if(street != road) streetGraph.insertEdge(street, road))
        }
      }
    }
    streetGraph
  }

  def pathWay(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): mutable.Seq[String] = {
    var path: mutable.Seq[String] = mutable.Seq()
    //Check if TaxEntry in vertices
    if(start.infoMap("STREET") == end.infoMap("STREET")){
//      println("Same")
      path = path :+ start.infoMap("STREET")
      path
    }
    else if(streetGraph.vertices.contains(start.infoMap("STREET")) && streetGraph.vertices.contains(end.infoMap("STREET"))){
      //Create respective Vertex for start && end
      val startGame: streetGraph.Vertex = streetGraph.vertices(start.infoMap("STREET"))
      val endGame: streetGraph.Vertex =streetGraph.vertices(end.infoMap("STREET"))
      //Create variables to store
      var vIterator: streetGraph.Vertex = null
      var toExplore: mutable.Queue[streetGraph.Vertex] = mutable.Queue(startGame)
      var explored: mutable.Set[streetGraph.Vertex] = mutable.Set(startGame)
      var map: mutable.Map[streetGraph.Vertex, streetGraph.Vertex] = mutable.Map(
        startGame -> null
      )
      var found: Boolean = false
      while(!found && toExplore.nonEmpty){
        val nodeExplore: streetGraph.Vertex = toExplore.dequeue()
        nodeExplore.edges.foreach{vertex =>
          if(vertex == endGame){
            map += (vertex -> nodeExplore)
            found = true
            vIterator = vertex
          }
          else if(!found && !explored.contains(vertex)){
            toExplore.enqueue(vertex)
            explored += vertex
            map += (vertex -> nodeExplore)
          }
        }
      }
      if(found){
//        println("Found")
        while(map(vIterator) != null){
          //          println(map(vIterator))
          path = path :+ vIterator.name
          vIterator = map(vIterator)
        }
        path = path :+ vIterator.name
        path.reverse
      }
      else{
//        println("No Path")
        path
      }
    }
    else{
//      println("Not in Graph / Not Same")
      path
    }
  }


  def computeFewestTurns(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Int = {
    pathWay(streetGraph, start, end).size - 1
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    pathWay(streetGraph, start, end).toSeq
  }
}
