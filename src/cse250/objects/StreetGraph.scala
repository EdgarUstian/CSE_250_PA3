/**
 * cse250.objects.StreetGraph.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

import scala.collection.mutable.{Map, Set}

class StreetGraph {

  // Vertex object holds name and set of edges.
  case class Vertex(name: String) {
    var edges = Set[Vertex]()
  }

  // Hold mapping of vertex name to Vertex object.
  val vertices = Map[String, Vertex]()
  // Set of all edges in graph (by name).
  val edges = Set[(String,String)]()

  // Insert edge from src to dest, adding src and dest if necessary.
  def insertEdge(src: String, dest: String): Unit = {
    val u = {
      if (vertices.contains(src)) vertices(src)
      else {
        vertices.addOne(src -> Vertex(src))
        vertices(src)
      }
    }
    val v = {
      if (vertices.contains(dest)) vertices(dest)
      else {
        vertices.addOne(dest -> Vertex(dest))
        vertices(dest)
      }
    }
    // Add (u,v) to edge list of u.
    u.edges += v
    edges += src -> dest
  }

  // Insert vertex with no edges, if not present.
  def insertVertex(name: String): Unit = {
    if (!vertices.contains(name)) vertices.addOne(name -> Vertex(name))
  }
}
