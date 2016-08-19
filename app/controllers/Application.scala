package controllers

import play.api.mvc._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class Application extends Controller {
		
	val list = List(1, 2, 3)
	val original = HashMap("A" -> 123, "B" -> 456)
	val supplier = () => new HashMap[Int, String]	
	val swap: HashMap[Int, String] = swap2[Int, String, HashMap[Int, String], HashMap[String, Int]](original, supplier)	

	def index() = Action {
		Ok("Hello World!")

	}
	def swap2[V, K, R <: Map[V, K], P <: Map[K, V]](origin: P, supplier: () => R): R = {
		val result = supplier()
			origin foreach (result += _.swap)
			result
	}
}
