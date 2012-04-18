package narad.nlp.phonology
import scala.collection.mutable.HashMap


case class Phone(form: String, features: HashMap[String, Boolean]) {
		
	def hasFeature(feat: String) = features.contains(feat)
	
	def hasFeature(feat: String, value: Boolean) = features(feat) == value
	
	def project = features.toArray
	
	override def toString = "%s".format(form) 
	
	def distance(other: Phone): Double = {
		val keys = (features.keys ++ other.features.keys).toArray.distinct
		var diff = 0
		for (key <- keys) {
			if (!features.contains(key) || !other.features.contains(key)) {
				diff += 1
			}
			else if (features.getOrElse(key, false) != other.features.getOrElse(key, true)) {
				diff += 1
			}
		}
		return diff	
	}
	
	def place: String = {
		if (features.contains("lab") && features("lab")) {
			return "lab"
		}
		else if (features.contains("cor") && features("cor")) {
			return "cor"
		}
		else if (features.contains("dorsal") && features("dorsal")) {
			return "dorsal"
		}
		else return "n/a"
	}
	
	def cons: String = {
		if (features.contains("cons")) {
			return features("cons").toString
		}
		return "n/a"
	}
	
	def ant: String = {
		if (features.contains("ant")) {
			return features("ant").toString
		}
		else {
			return "n/a"
		}
	}
	
	def height: String = {
		if (features.contains("high") && features("high")) {
			return "high"
		}
		else if (features.contains("back") && features("back")) {
			return "back"
		}
		else {
			return "n/a"
		}
	}
	
	def manner: String = {
		if (features.contains("cont") && features("cont")) {
			return "cont"
		}
		else if (features.contains("nasal") && features("nasal")) {
			return "nasal"
		}
		else if (features.contains("lat") && features("lat")) {
			return "lat"
		}
		else if (features.contains("strid") && features("strid")) {
			return "strid"
		}
		else {
			return "n/a"
		}
	}
	
	def approx: String = {
		if (features.contains("approx") && features("approx")) {
			return "approx"
		}
		else {
			return "n/a"
		}
		
	}
	
	def laryngeal: String = {
		if (features.contains("voice") && features("voice")) {
			return "voice"
		}
		else if (features.contains("spread") && features("spread")) {
			return "spread"
		}
		else {
			return "n/a"
		}
	}
	
	def sonority: String = {
		if (features.contains("son") && features("son")) {
			return "son"
		}
		else {
			return "n/a"
		}
	}

}



//Array[ArticulatoryFeature]) {
	// ": %s".format(form, features.mkString(", "))
/*
	val featureMap = new HashMap[String, HashMap[String, String]]
	
	def addVowel(vowel: String, height: String, backness: String, roundedness: String, nasalization: String) = {
		featureMap += vowel -> HashMap("Height" -> height,
																			 "Backness" -> backness,
																			 "Roundedness" -> roundedness,
																			 "Nasalization" -> nasalization)
	}
	
	def addConsonant(consonant: String, place: String, phonation: String, mechanism: String) = { //, vot: String, length: String) = {
		featureMap += consonant -> HashMap("PlaceOfArticulation" -> place,
																					 "Phonation" -> phonation,
																					 "Mechanism" -> mechanism)
																		//			 "VoiceOnsetTime" -> vot,
																		//			 "Length" -> length)
	}
	
	addConsonant("p", "Bilabial",	"Voiceless",	"plosive")
	addConsonant("b", "Bilabial",	"Voiced",			"plosive")
	addConsonant("t", "Alveolar", "Voiceless",	"plosive")
	addConsonant("d", "Alveolar",	"Voiced",			"plosive")
	addConsonant("k",	"Velar",		"Voiceless",	"plosive")
	addConsonant("g",	"Velar",		"Voiced",			"plosive")
	
	override def toString: String = form
	
	def features: HashMap[String, String] = featureMap.getOrElse(form, new HashMap[String, String])

}		
		
		val feats = new HashMap[String, String]
		form match {
			case "p" => {
				feats += "PlaceOfArticulation" -> "Bilabial"
				feats += "Voice" -> "Voiceless"				
			}
			case "b" => {
				feats += "PlaceOfArticulation" -> "Bilabial"
				feats += "Voice" -> "Voiced"				
			}
			case "t" => {
				feats += "PlaceOfArticulation" -> "Alveolar"
				feats += "Voice" -> "Voiceless"				
			}
			case "d" => {
				feats += "PlaceOfArticulation" -> "Alveolar"
				feats += "Voice" -> "Voiced"				
			}
			case "k" => {
				feats += "PlaceOfArticulation" -> "Velar"
				feats += "Voice" -> "Voiceless"				
			}
			case "g" => {
				feats += "PlaceOfArticulation" -> "Velar"
				feats += "Voice" -> "Voiced"				
			}
			case "a" => {
				feats += "Roundness" -> "Unrounded"
				feats += "Closeness" -> "Open"
				feats += "Frontness" -> "Front"
			}
			case "i" => {
				feats += "Roundness" -> "Unrounded"
				feats += "Closeness" -> "Close"
				feats += "Frontness" -> "Front"
			}
			case "e" => {
				feats += "Roundness" -> "Unrounded"
				feats += "Closeness" -> "Mid"
				feats += "Frontness" -> "Front"
			}
		}
		feats
	}
}
*/
