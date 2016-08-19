package controllers

import play.api.mvc._
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap


trait Monoid[T] {
  def zero: T
  def add(x: T, y: T): T
}

trait MyMap[K, V] {
  def +=(kv: (K, V))
  def lookup(key: K): Option[V]
}


class AList[K, V](keyValues: (K, V)*) extends MyMap[K, V] {
  val items: Buffer[(K, V)] = keyValues.toBuffer

  def +=(kv: (K, V)) {
    items += kv
  }

  def lookup(key: K): Option[V] = {
    for ((k, v) <- items) {
      if (k == key)
        return Some(v)
    }
    return None
  }
}


class Application extends Controller {
		
	lazy val list = List(1, 2, 3)
	val original = HashMap("A" -> 123, "B" -> 456)
	private[this] val supplier = () => new HashMap[Int, String]	
	val swap: HashMap[Int, String] = swap2[Int, String, HashMap[Int, String], HashMap[String, Int]](original, supplier)	
        var it =Iterator(1,2,3)
	
	def index() = Action {
		Ok("Hello World!")
	}
	def swap2[V, K, R <: Map[V, K], P <: Map[K, V]](origin: P, supplier: () => R): R = {
		val result = supplier()
			origin foreach (result += _.swap)
			result
	}
}

