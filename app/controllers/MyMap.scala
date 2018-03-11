
package controllers

import play.api.mvc._

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap


trait MyMap[K, V] {
  def +=(kv: (K, V))
  def lookup(key: K): Option[V]
}
