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
      if(intersectionIDs.contains(id)){
        if(streets.size > 1){
          streets.foreach{street =>
//            streetGraph.insertVertex(street)
            streets.foreach(road => if(street != road) streetGraph.insertEdge(street, road))
          }
        }
      }
    }
    streetGraph
  }

  def pathWay(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): mutable.Seq[String] = {
    var path: mutable.Seq[String] = mutable.Seq()
    //PART 1:
    //Creates the BFS map from starting location
    val startID = start.infoMap("STREET")
    val endID = end.infoMap("STREET")
//    println(startID + " --> " + endID)
    var visited: Set[String] = Set(startID)
    val explored: mutable.Map[String, String] = mutable.Map[String, String](
        startID -> "StartNode"
    )
    val guests: mutable.Queue[String] = mutable.Queue()
    guests.enqueue(startID)

    //start or end not in streetGraph
    if(streetGraph.vertices.contains(startID) && streetGraph.vertices.contains(endID)){
      var explore: Boolean = true
      while(guests.nonEmpty){
        val nextNode: String = guests.dequeue()
        //      println(nextNode)
        streetGraph.edges.foreach{edge =>
          if(nextNode.toUpperCase == edge._1.toUpperCase){
            //          println(edge._1, edge._2)
            if(!visited.contains(edge._2) && explore){
              //            println("exploring: " + edge._2)
              //            println(edge._1, edge._2)
              explored += (edge._2 -> nextNode)
              guests.enqueue(edge._2)
              visited += edge._2
            }
            if(edge._2 == endID){
              guests.empty
              explore = false
            }
          }
        }
      }
      //    println(visited)
      //    explored.foreach(street => println(street._1 + " -> " + street._2))

      //PART 2:
      //Creates a traversable path if possible
      //start or end in streetGraph but no path
      if(explored.contains(endID)){
//        println("Contains")
        path = path :+ endID
        var prev = explored(endID)
        while(prev != "StartNode"){
          path = path :+ prev
          prev = explored(prev)
        }
        path = path.reverse
      }
      else{
//        println("No Path")
        path = mutable.Seq()
      }
    }
    else if(start == end){
//      println("Not in graph / same")
      path = mutable.Seq(startID)
    }
    else{
//      println("Not in graph / not same")
      path = mutable.Seq()
    }
    path
  }


  def computeFewestTurns(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Int = {
    pathWay(streetGraph, start, end).size - 1
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    pathWay(streetGraph, start, end).toSeq
  }
}
