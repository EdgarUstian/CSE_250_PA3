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
               deMap(id) += (tag \ "@v").toString()
             }
             else{
               deMap += (id -> mutable.Set((tag \ "@v").toString()))
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
            streetGraph.insertVertex(street)
            streets.foreach(road => if(street != road) streetGraph.insertEdge(street, road))
          }
        }
      }
    }
    streetGraph
  }

  def computeFewestTurns(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Int = {
    -1
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    List()
  }
}
