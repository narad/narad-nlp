package narad.util

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Index[T] {  
	var isDirty = false
	var elems = new ArrayBuffer[T] //null.asInstanceOf[Array[T]]
	var hash = new HashMap[T, Int]
	
	def index(e: T): Int = {
		if (hash.contains(e)) {
			hash(e)
		}
		else {
			elems += e
			hash  += e -> hash.size
			hash.size-1
		}
	}
	
	def get(idx: Int): T = {
		elems(idx)
	}
}



/*
//import scala.collection.immutable.HashMap

//extends HashMap[T, Int] {   // extends ArrayBuilder[T] {  //with Iterable[T] with Builder[T]{

hash.getOrElseUpdate(e, hash.size)

  def get(idx:Int): T = {
		if (growable) {
			throw new RuntimeException("Cannot grab elements of index while growable (.freeze it)") 			//hash.getOrElse(idx, -1)
		}
		else {
			elems(idx)
		}
	}

  def freeze = growable = false; elems.clear; elems.appendAll(hash.keys)

}
*/