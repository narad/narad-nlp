package narad.util

import scala.collection.mutable.{ArrayBuffer, HashMap}

class Index[T] {  
	var isDirty = false
	var elems = new ArrayBuffer[T] 
	var hash = new HashMap[T, Int]
	
	def contains(e: T): Boolean = {
		hash.contains(e)
	}
	
	def elements: Iterable[T] = {
		elems
	}
	
	def index(e: T): Int = {
		if (hash.contains(e)) {
			hash(e)
		}
		else {
			elems += e
			hash  += e -> (hash.size+1)
			hash.size
		}
	}
	
	def indexOf(e: T): Int = {
		hash.getOrElse(e, -1)
	}
	
	def get(idx: Int): T = {
		elems(idx-1)
	}
	
	def size = elems.size
}





//null.asInstanceOf[Array[T]]

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