package narad.nlp.trees

import collection.mutable.{HashMap, HashSet}

class Grammar extends HashSet[GrammarRule]{}

class CountGrammar extends HashMap[GrammarRule, Int] {}

class WeightedGrammar extends HashMap[GrammarRule, Double] {

  def add(rule: GrammarRule, weight: Double) = {
    this(rule) = weight
  }

}