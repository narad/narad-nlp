package narad.nlp.srl
import collection.mutable.{HashMap, ArrayBuffer}
import java.io.FileWriter
import scala.math._
import narad.bp.structure.Potential
import narad.bp.util.{PotentialExample, Feature, StringFeature}
import narad.bp.util.index.{Index, ArrayIndex, HashIndex}


trait SRLFeatures {
  val dummyToken = SRLToken("ROOT", "ROOT_LEMMA", "ROOT_POS", "ROOT_CPOS")

  def extractSRLFeatures(datum: SRLDatum, dict: SRLDictionary, index: Index[String], params: SRLParams): PotentialExample = {
    val ex = new PotentialExample
    val tokens = Array(dummyToken) ++ datum.tokens ++ Array(dummyToken)
    val pruneRolesByCount = params.PRUNE_ROLES_BY_COUNT
    val roleThreshold = params.ROLE_PRUNE_THRESHOLD
    val maxDist = params.MAX_DIST
    val labelCorrect = true

    for (i <- 1 to datum.slen if datum.hasPred(i)) {
      val lemma = datum.lemma(i)
      val word = datum.form(i)
      val senseFeats = senseFeatures(i, tokens)
      val senses = if (dict.containsSense(lemma)) dict.senses(lemma).toArray else Array("default-sense")
      val roles = if (pruneRolesByCount) dict.getRoles(roleThreshold) else dict.getRoles()
      assert(roles.size > 0, "No roles found, should have defaulted to using all roles.")
      // Sense Features
      if (dict.containsSense(lemma)) {
        senses.zipWithIndex.foreach { case(sense, senseCount) =>
          val sensePotName = "sense(%d,%d)".format(i, senseCount)
          ex.potentials += new Potential(1.0, sensePotName, (datum.hasSense(i, sense) && labelCorrect))
          ex.features(sensePotName) = senseFeats.map{ f =>
            if (params.INTEGERIZE) {
              new Feature(index.index("%s-%s".format(sense, f)), 1.0, 0)
            }
            else {
              new StringFeature("%s-%s".format(sense, f), index.index("%s-%s".format(sense, f)), 1.0, 0)
            }
          }
        }
      }
      else { // For an unseen word
        val sensePotName = "sense(%d,%d)".format(i, 0)
        ex.potentials += new Potential(1.0, sensePotName, labelCorrect)
        ex.features(sensePotName) = senseFeatures(i, tokens).map{ f =>
          if (params.INTEGERIZE) {
            new Feature(index.index("%s-%s".format(senses(0), f)), 1.0, 0)
          }
          else {
            new StringFeature("%s-%s".format(senses(0), f), index.index("%s-%s".format(senses(0), f)), 1.0, 0)
          }
        }
      }
      if (params.MODEL_ROLE_VALENCY) {
        // ROLE VALENCY
        val vroles = if (roles.size > params.NUM_VALENCY_ROLES) roles.slice(0, params.NUM_VALENCY_ROLES) else roles
        vroles.zipWithIndex.foreach { case(role, roleCount) =>
          Array(0,1,2).foreach { bin =>
            senses.zipWithIndex.foreach { case(sense, senseCount) =>
              val potname = "senseHasRole(%d,%d,%d,%d)".format(i, roleCount, bin, senseCount)
              val hasRole = (datum.hasSense(i, sense) && datum.roles(i).filter(_ == role).size == bin)        //datum.hasSense(i, sense) && !datum.roles(i).contains(role)
              ex.potentials += new Potential(1.0, potname, labelCorrect && hasRole)
              ex.features(potname) = senseToValencyFeatures(i, bin, role, sense, tokens).map{ f =>
                if (params.INTEGERIZE) {
                  new Feature(index.index(f), 1.0, 0)
                }
                else {
                  new StringFeature(f, index.index(f), 1.0, 0)
                }
              }
            }
          }
        }
      }
      if (params.MODEL_ARG_VALENCY) {
        // ARG VALENCY
        val acount = datum.roles(i).size
        for (bin <- 0 until params.NUM_VALENCY_ARGS) {
          senses.zipWithIndex.foreach { case(sense, senseCount) =>
            val potname = "senseHasArg(%d,%d,%d)".format(i, bin, senseCount)
            val afeats =
            ex.potentials += new Potential(1.0, potname, labelCorrect && datum.hasSense(i, sense) && bin == acount)
            ex.features(potname) = valencyFeatures(sense, lemma, word, bin).map { f =>
              if (params.INTEGERIZE) {
                new Feature(index.index(f), 1.0, 0)
              }
              else {
                new StringFeature(f, index.index(f), 1.0, 0)
              }
            }
          }
        }
      }
      // Argument Features
      if (params.MODEL_ARGS) {
        val abound = if (params.PRUNE_ARGS_BY_DIST) maxDist else datum.slen
        for (j <- 1 to datum.slen if abs(i-j) <= abound) {
          if (datum.slen < 10 || !params.PRUNE_ARGS_BY_TAG || dict.countPos(datum.postag(j)) >= params.PRUNE_TAG_THRESHOLD) {
            val argFeats = argumentFeatures(i, j, tokens, params)
            val argPotName = "hasArg(%d,%d)".format(i, j)
            ex.potentials += new Potential(1.0, argPotName, (datum.hasArg(i, j) && labelCorrect))
            ex.features(argPotName) = argFeats.map{ f => // new Feature(index.index(f), 1.0, 0) }
              if (params.INTEGERIZE) {
                new Feature(index.index(f), 1.0, 0)
              }
              else {
                new StringFeature(f, index.index(f), 1.0, 0)
              }
            }
            // Role Features
            if (params.MODEL_ROLES) {
              val truths = roles.map(datum.hasRole(i,j,_))
              val hasTrue = truths.exists(_ == true)
              roles.zipWithIndex.foreach { case(role, r) =>
                val rolePotName = "hasRole(%d,%d,%d)".format(i, j, r)
                val roleIsCorrect = truths(r) || (r == 0 && !hasTrue && datum.hasArg(i,j))
                ex.potentials += new Potential(1.0, rolePotName, roleIsCorrect && labelCorrect)
                ex.features(rolePotName) = argFeats.map{f => //new Feature(index.index("%s-%s".format(role, f)), 1.0, 0)}
                  if (params.INTEGERIZE) {
                    new Feature(index.index("%s-%s".format(role, f)), 1.0, 0)
                  }
                  else {
                    new StringFeature("%s-%s".format(role, f), index.index("%s-%s".format(role, f)), 1.0, 0)
                  }
                }
              }
            }
          }
        }
      }
    }
    ex
  }


  def extractConnectionFeatures(datum: SRLDatum, dict: SRLDictionary, index: Index[String], gpreds: Array[Int], maxDist: Int, params: SRLParams): PotentialExample = {
    val slen = datum.slen
    val words = datum.forms
    val tags  = datum.postags
    val tokens = Array(dummyToken) ++ datum.tokens
    val heads = Array(-1) ++ datum.heads //Array(-1) ++ lines.map(_.split("\t")(8).toInt)
    val ex = new PotentialExample
    val labelCorrect =  params.MODEL != "HIDDEN"
    if (params.CONNECTION_ORDER.contains("1")) {
      for (i <- 0 to slen; j <- 1 to slen if i != j && datum.hasPred(i) && abs(i-j) <= maxDist) {
        if (!params.PRUNE_ARGS_BY_TAG || dict.countPos(datum.postag(j)) >= params.PRUNE_TAG_THRESHOLD) {
          val potname = "sslink(%d,%d)".format(i,j)
          ex.potentials += new Potential(1.0, potname, datum.hasArg(i, j) && heads(j) == i)
          ex.features(potname) = connectFeatures(tokens, i, j).map{f => //new Feature(index.index(f), 1.0, 0)}
            if (params.INTEGERIZE) {
              new Feature(index.index(f), 1.0, 0)
            }
            else {
              new StringFeature(f, index.index(f), 1.0, 0)
            }
          }
        }
      }
    }
    if (params.CONNECTION_ORDER.contains("2")) {
      for (i <- 0 to slen if datum.hasPred(i)) {
        for (j <- 1 to slen if i != j && abs(i-j) <= maxDist) {
          for (k <- 1 to slen if k != i && k != j) {
            val potname = "sslink(%d,%d,%d)".format(i,j,k)
            val isCorrect = datum.hasArg(i, j) && heads(k) == i && heads(j) == k
            ex.potentials += new Potential(1.0, potname, isCorrect)
            ex.features(potname) = connectFeatures(tokens, i, j, k).map{f => //new Feature(index.index(f), 1.0, 0)}
              if (params.INTEGERIZE) {
                new Feature(index.index(f), 1.0, 0)
              }
              else {
                new StringFeature(f, index.index(f), 1.0, 0)
              }
            }
          }
        }
      }
    }
    ex
  }

  def extractSyntacticFeatures(datum: SRLDatum, index: Index[String], params: SRLParams): PotentialExample = {
    val slen = datum.slen
    val ex = new PotentialExample()
    val tokens = Array(dummyToken) ++ datum.tokens
    val heads = Array(-1) ++ datum.heads
    for (i <- 0 to slen; j <- 1 to slen if i != j) {
      val label = (heads(j) == i) // && params.MODEL != "HIDDEN") // "+" else ""
      val potname = "un(%d,%d)".format(i, j)
      ex.potentials += new Potential(1.0, potname, label)
      if (params.MODEL == "ORACLE") {
        ex.features(potname) = Array[Feature]() //new Feature(index.index("X_DUMMY"), 1.0, 0))
      }
      else {
        ex.features(potname) = morphDependency(tokens, i, j).map{f => //new Feature(index.index(f), 1.0, 0))
          if (params.INTEGERIZE) {
            new Feature(index.index(f), 1.0, 0)
          }
          else {
            new StringFeature(f, index.index(f), 1.0, 0)
          }
        }
      }
    }
    ex
  }



  def allcaps(str: String): Boolean = {
    return str.toUpperCase == str
  }

  def capitalized(str: String): Boolean = {
    if (str.size == 0) return true
    return (str.substring(0, 1).toUpperCase == str.substring(0, 1))
  }

  def senseFeatures(idx: Int, tokens: Array[SRLToken]): Array[String] = {
    val feats = new ArrayBuffer[String]
    val token = tokens(idx)
    val slen = tokens.size
    feats += "[sense-word]-%s".format(token.word)
    feats += "[sense-lemma]-%s".format(token.lemma)
    feats += "[sense-pos]-%s".format(token.pos)
    for (i <- 1 to 5) {
      if (idx - i >= 0)   feats += "[sense-context]-%s".format(tokens(idx-i).word)
      if (idx + i < slen) feats += "[sense-context]-%s".format(tokens(idx+i).word)
    }
    for (i <- 1 to 3) {
      if (idx - i >= 0)   feats += "[sense-context]-%s-l%d".format(tokens(idx-i).word, i)
      if (idx + i < slen) feats += "[sense-context]-%s-r%d".format(tokens(idx+i).word, i)
    }
    // These are good for Chinese, bad for English, Catalan
//    if (idx-1 >= 0)   feats += "[sense-prev-word]-%s-%s".format(tokens(idx-1).word, token.word)
//    if (idx+1 < slen) feats += "[sense-post-word]-%s-%s".format(tokens(idx+1).word, token.word)

    return feats.map(_.replaceAll("=", "-")).toArray
  }

  def valencyFeatures(sense: String, lemma: String, word: String, bin: Int): Array[String] = {
    Array(
      "av-%s-%d".format(sense + lemma, bin),
      "av-%s-%d".format(lemma, bin),
      "av-%s-%d".format(word, bin),
      "av-%d".format(bin))
  }




  //    feats += "[sense-morphs]-%s".format(token.morph)
  //    val cap = if (capitalized(token.word)) "UC" else "LC"
    //    feats += "[sense-bias]"
//        feats += "[sense-word-pos]-%s-%s".format(token.word, token.pos)
    //    feats += "[sense-cap]-%s".format(cap)
//        for (m <- token.morph.split("\\|")) {
//          feats += "[sense-morph]-%s".format(m)
//        }


  def senseToValencyFeatures(idx: Int, bin: Int, role: String, sense: String, tokens: Array[SRLToken]): Array[String] = {
    val feats = new ArrayBuffer[String]
    val token = tokens(idx)

    feats += "[s2v-sense-role-bin]-%s-%s-%d".format(sense, role, bin)
    feats += "[s2v-sense-role-bin-word]-%s-%s-%d-%s".format(sense, role, bin, token.word)
    feats += "[s2v-sense-role-bin-lemma]-%s-%s-%d-%s".format(sense, role, bin, token.lemma)
    feats += "[s2v-sense-role-bin-tag]-%s-%s-%d-%s".format(sense, role, bin, token.pos)
/*
    feats += "[s2v-role-bin]-%s-%d".format(role, bin)
    feats += "[s2v-role-bin-word]-%s-%d-%s".format(role, bin, token.word)
    feats += "[s2v-role-bin-lemma]-%s-%d-%s".format(role, bin, token.lemma)
    feats += "[s2v-role-bin-tag]-%s-%d-%s".format(role, bin, token.pos)

        feats += "[s2v-sense-role-bin-morph]-%s-%s-%d-%s".format(sense, role, bin, token.morph)
        feats += "[s2v-sense-role-bin-word-tag]-%s-%s-%d-%s-%s".format(sense, role, bin, token.word, token.pos)
        feats += "[s2v-sense-role-bin-lemma-tag]-%s-%s-%d-%s-%s".format(sense, role, bin, token.lemma, token.pos)
        feats += "[s2v-role-bin-morph]-%s-%d-%s".format(role, bin, token.morph)
     feats += "[s2v-role-bin-word-tag]-%s-%d-%s-%s".format(role, bin, token.word, token.pos)
    feats += "[s2v-role-bin-lemma-tag]-%s-%d-%s-%s".format(role, bin, token.lemma, token.pos)
     */
    return feats.map(_.replaceAll("=", "-")).toArray
  }


  def argumentFeatures(pidx: Int, aidx: Int, tokens: Array[SRLToken], params: SRLParams): Array[String] = {
    val feats = new ArrayBuffer[String]
    val mode = 3
    val pred = tokens(pidx)
    val arg  = tokens(aidx)
    val dist = Math.abs(aidx - pidx)
    val dir  = if (aidx > pidx) "RIGHT" else if (aidx < pidx) "LEFT" else "SAME"

    feats += "ARG-BIAS"
//    if (mode >= 1) {
    feats += "dir-%s".format(dir)
    feats += "dist-%d".format(dist)
    feats += "dir-dist-%s-%d".format(dir, dist)

    feats += "predword-%s".format(pred.word)
    feats += "predlemma-%s".format(pred.lemma)
    feats += "predtag-%s".format(pred.pos)

      feats += "argword-%s".format(arg.word)
      feats += "arglemma-%s".format(arg.lemma)
      feats += "argtag-%s".format(arg.pos)
//
//
      feats += "predword-%s-argword-%s".format(pred.word, arg.word)
      feats += "predlemma-%s-arglemma-%s".format(pred.lemma, arg.lemma)
      feats += "predtag-%s-argtag-%s".format(pred.pos, arg.pos)

      feats += "pred-%s-argtag-%s".format(pred.word, arg.pos)
      feats += "predtag-%s-argword-%s".format(pred.pos, arg.word)
      feats += "predword-%s-argword-%s-predtag-%s-argtag-%s".format(pred.word, arg.word, pred.pos, arg.pos)
      feats += "predmorph-%s-argmorph-%s".format(pred.morph, arg.morph)
//    }
//    if (mode >= 2) {
      feats += "predtag-%s-argtag-dist-%s-%d".format(pred.pos, arg.pos, dist)
      feats += "predtag-%s-argtag-dir-%s-%s".format(pred.pos, arg.pos, dir)
      feats += "predtag-dist-dir-%s-%d-%s".format(pred.pos, dist, dir)
      feats += "argtag-dist-dir-%s-%d-%s".format(arg.pos, dist, dir)
      feats += "predtag-%s-argtag-%s-dist-dir-%d-%s".format(pred.pos, arg.pos, dist, dir)

//    }

    if (aidx > 1) {
      feats += "prevword-word-%s-%s".format(tokens(aidx-1).word, arg.word)
      feats += "prevtag-word-%s-%s".format(tokens(aidx-1).pos, arg.word)
      feats += "prevtag-tag-%s-%s".format(tokens(aidx-1).pos, arg.pos)
    }
    else {
      feats += "[start]"
    }
    if (aidx < tokens.size-1) {
      feats += "postword-word-%s-%s".format(tokens(aidx+1).word, arg.word)
      feats += "posttag-word-%s-%s".format(tokens(aidx+1).pos, arg.word)
      feats += "posttag-tag-%s-%s".format(tokens(aidx+1).pos, arg.pos)
    }
    else {
      feats += "[end]"
    }

    if (dist <= 5) feats += "[in-between-tags]-%s".format(tokens.slice(pidx, aidx).map(_.pos).mkString("-"))

/*    if (mode >= 3) {
      val m1s = pred.morph.split("\\|")
      val m2s = arg.morph.split("\\|")
      for (m1 <- m1s; m2 <- m2s) {
        feats += "pmorph-%s-amorph-%s".format(m1, m2)
      }
    }*/
    return feats.map(_.replaceAll("=", "-")).toArray
  }


  def connectFeatures(tokens: Array[SRLToken], cidx: Int, pidx: Int): Array[String] = {
    val feats = new ArrayBuffer[String]
    val parent = tokens(pidx)
    val child  = tokens(cidx)
    val dist = Math.abs(cidx - pidx)
    val dir  = if (cidx > pidx) "R" else "L"
    feats += "sli"
    feats += "sli-%s-%s-%s-%s".format(parent.word, child.word, parent.pos, child.pos)
    feats += "sli-%s-%s".format(parent.word, child.word)
    feats += "sli-%s-%s".format(parent.pos, child.pos)
    feats += "sli-%s-%s".format(parent.word, child.pos)
    feats += "sli-%s-%s".format(parent.pos, child.word)
    feats += "sli-%s-%s-%s".format(parent.word, child.word, dir)
    feats += "sli-%s-%s-%s".format(parent.pos, child.pos, dir)
    feats += "sli-%s-%s-%s".format(parent.word, child.pos, dir)
    feats += "sli-%s-%s-%s".format(parent.pos, child.word, dir)
    feats += "sli-%s-%s-%d".format(parent.word, child.pos, dist)
    feats += "sli-%s-%s-%d".format(parent.pos, child.word, dist)
    feats += "sli-%s-%s-%d".format(parent.pos, child.pos, dist)
    feats += "sli-%s-%s-%d-%s".format(parent.pos, child.pos, dist, dir)
    feats += "sli-%d".format(dist)
    feats += "sli-%s".format(dir)
    feats += "sli%s-%d".format(dir, dist)
    if (dist <= 5) feats += "[connect-in-between-tags]-%s".format(tokens.slice(pidx, cidx).map(_.pos).mkString("-"))

    /*
    for (m1 <- parent.morph.split("\\|"); m2 <- child.morph.split("\\|")) {
      feats += "sli-pmorph-%s-amorph-%s".format(m1, m2)
    }*/
    return feats.map(_.replaceAll("=", "-")).toArray
  }

  def connectFeatures(tokens: Array[SRLToken], pidx: Int, cidx: Int, gcidx: Int): Array[String] = {
    val feats = new ArrayBuffer[String]
    val parent = tokens(pidx)
    val child  = tokens(cidx)
    val gchild = tokens(gcidx)
    val all = Array(pidx, cidx, gcidx).sortBy(_ * -1)
    val width = Math.abs(all(0) - all(2))
    val dist1 = Math.abs(pidx - cidx)
    val dist2 = Math.abs(cidx - gcidx)
    val dir1  = if (pidx < cidx) "R" else "L"
    val dir2  = if (cidx < gcidx) "R" else "L"
    feats += "3sli"
/*
    feats += "3sli-%s-%s-%s-%s".format(parent.word, child.word, parent.pos, child.pos)
    feats += "3sli-%s-%s".format(parent.word, child.word)
    feats += "3sli-%s-%s".format(parent.pos, child.pos)
    feats += "3sli-%s-%s".format(parent.word, child.pos)
    feats += "3sli-%s-%s".format(parent.pos, child.word)
    feats += "3sli-%s-%s-%s".format(parent.word, child.word, dir1)
    feats += "3sli-%s-%s-%s".format(parent.pos, child.pos, dir1)
    feats += "3sli-%s-%s-%s".format(parent.word, child.pos, dir1)
    feats += "3sli-%s-%s-%s".format(parent.pos, child.word, dir1)
    feats += "3sli-%s-%s-%d".format(parent.word, child.pos, dist1)
    feats += "3sli-%s-%s-%d".format(parent.pos, child.word, dist1)
    feats += "3sli-%s-%s-%d".format(parent.pos, child.pos, dist1)
    feats += "3sli-%s-%s-%d-%s".format(parent.pos, child.pos, dist1, dir1)
    feats += "3sli-%d-%d".format(dist1)
    feats += "3sli-%s".format(dir1)
    feats += "3sli%s-%d".format(dir1, dist1)
*/

    feats += "3sli-words-%s-%s-%s".format(parent.word, child.word, gchild.word)
    feats += "3sli-tags-%s-%s-%s".format(parent.pos, child.pos, gchild.pos)
    feats += "3sli-tags-dir-%s-%s-%s-%s-%s".format(parent.pos, child.pos, gchild.pos, dir1, dir2)
    feats += "3sli-dirs-dists-%s-%s-%d-%d".format(dir1, dir2, dist1, dist2)
    feats += "3sli-dirs-%s-%s".format(dir1, dir2)
    feats += "3sli-width-%d".format(width)
    /*
    for (m1 <- parent.morph.split("\\|"); m2 <- child.morph.split("\\|")) {
      feats += "sli-pmorph-%s-amorph-%s".format(m1, m2)
    }*/
    return feats.map(_.replaceAll("=", "-")).toArray
  }

  def morphDependency(otokens: Array[SRLToken], ohead: Int, odep: Int, morph: Boolean = false): Array[String] = {

    val feats = new ArrayBuffer[String]

    val stoken = new SRLToken("LEFFT_W", "LEFT_L", "LEFT_P", "LEFT_CP")
    val etoken = new SRLToken("RIGHT_W", "RIGHT_L", "RIGHT_P", "RIGHT_CP")
    val tokens = Array(stoken) ++ otokens ++ Array(etoken)
    val head = ohead + 1
    val dep  = odep + 1

    val dir  = if (dep > head) "R" else "L"
    val dist = Math.abs(head - dep - 1)
    val htoken = tokens(head)
    val dtoken = tokens(dep)

    val small = if (dep < head) dep else head
    val large = if (dep > head) dep else head

    feats += "1,3:%s,%s;".format(htoken.word, htoken.pos)
    feats += "1,3:%s,%s;&%s%d".format(htoken.word, htoken.pos, dir, dist)
    feats += "1,3:%s,%s;%s,%s".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos)
    feats += "1,3:%s,%s;%s,%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos, dir, dist)
    feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.word)
    feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dir, dist)
    feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.pos)
    feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.pos, dir, dist)

    feats += "2:%s;".format(htoken.lemma)
    feats += "2:%s;&%s%d".format(htoken.lemma, dir, dist)
    feats += "2:%s;1:%s".format(htoken.lemma, dtoken.word, dir, dist)
    feats += "2:%s;1:%s:&%s%d".format(htoken.lemma, dtoken.word, dir, dist)
    feats += "2:%s;1,3:%s,%s".format(htoken.lemma, dtoken.lemma, dtoken.pos)
    feats += "2:%s;1,3:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.pos, dir, dist)
//    feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
//    feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
    feats += "2:%s;2:%s".format(htoken.lemma, dtoken.lemma)
    feats += "2:%s;2:%s&%s%d".format(htoken.lemma, dtoken.lemma, dir, dist)
    feats += "2:%s;3:%s".format(htoken.lemma, dtoken.pos)
    feats += "2:%s;3:%s&%s%d".format(htoken.lemma, dtoken.pos, dir, dist)
//    feats += "2:%s;4:%s".format(htoken.lemma, dtoken.cpos)
//    feats += "2:%s;4:%s&%s%d".format(htoken.lemma, dtoken.cpos, dir, dist)

    feats += "adj3:%s,%s,%s,%s,%s".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos)
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "")
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "", dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "")
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "", dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos)
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
    feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
    feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
    feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)

    if (morph) {
      feats += "[dp-morph]-%s-X-%s".format(htoken.morph, dtoken.morph)
      val m1s = htoken.morph.split("\\|")
      val m2s = dtoken.morph.split("\\|")
      for (m1 <- m1s; m2 <- m2s) {
        feats += "S-H-%sxD-%s".format(m1, m2)
      }
    }

    return feats.map(_.replaceAll("=", "-")).toArray
  }

}
























  /*
    def dependencyFeatures(tokens: Array[SRLToken], cidx: Int, pidx: Int): Array[String] = {
    val feats = new ArrayBuffer[String]
    val parent = tokens(pidx)
    val child  = tokens(cidx)
    val dist = Math.abs(cidx - pidx)
    val dir  = if (cidx > pidx) "R" else "L"
    feats += "dp%s-%s".format(parent.word, child.word)
    feats += "dp%s-%s".format(parent.pos, child.pos)
    feats += "dp%s-%s".format(parent.word, child.pos)
    feats += "dp%s-%s".format(parent.pos, child.word)
    feats += "dp%s-%s-%s".format(parent.word, child.word, dir)
    feats += "dp%s-%s-%s".format(parent.pos, child.pos, dir)
    feats += "dp%s-%s-%s".format(parent.word, child.pos, dir)
    feats += "dp%s-%s-%s".format(parent.pos, child.word, dir)
    feats += "dp%d".format(dist)
    feats += "dp%s".format(dir)
    feats += "dp%s-%d".format(dir, dist)
    return feats.map(_.replaceAll("=", "-")).toArray
  }
   */




//    if (params.CONSTITUENT_SYNTAX) {
//      for ( width <- 2 to slen; start <- 0 to (slen - width); split <- (start+1) until (start+width)) {
//        val end = start + width
//        val potname = "clink(%d,%d,%d)".format(start, end, split)
//        ex.potentials += new Potential(1.0, potname, false)
//        ex.features(potname) = connectFeatures(tokens, start, end).map{f => new Feature(index.index("clink-" + f), 1.0, 0)}
//      }
//    }




/*
                    Array(new Feature(index.index("%s-%s-%d".format(sense + lemma, role, on)), 1.0, 0),
                                               new Feature(index.index("%s-%s-%d".format(sense + word, on)), 1.0, 0),
                                               new Feature(index.index("%s-%s-%d".format(sense + tag, on)), 1.0, 0),
                                               new Feature(index.index("%s-%s-%d".format(sense + morph, on)), 1.0, 0),
                                               new Feature(index.index("%s-%d".format(lemma, on)), 1.0, 0),
                                               new Feature(index.index("%s-%d".format(lemma, on)), 1.0, 0),
                                               new Feature(index.index("%s-%d".format(role, on)), 1.0, 0))
*/


/*
         if (params.MODEL_VALENCY && vtype == 1) {
      roles.slice(0, params.NUM_VALENCY_ROLES).zipWithIndex.foreach { case(role, roleCount) =>
        Array(0,1).foreach { on =>
          senses.zipWithIndex.foreach { case(sense, senseCount) =>
          val potname = "senseHasArg(%d,%d,%d,%d)".format(i, roleCount, on, senseCount)
          val hasRole = datum.hasSense(i, sense) && !datum.roles(i).contains(role)
          ex.potentials += new Potential(1.0, potname, labelCorrect &&
                                         ((datum.hasSense(i, sense) && datum.roles(i).contains(role) && on == 1) ||
                                          (datum.hasSense(i, sense) && !datum.roles(i).contains(role) && on == 0)))
          ex.features(potname) = if (on == 1) {
            Array(new Feature(index.index("%s-%s-on".format(lemma + sense, role)), 1.0, 0))
          }
          else {
            Array(new Feature(index.index("%s-%s-off".format(lemma + sense, role)), 1.0, 0))
          }
        }
      }
    }
  }
*/







//      val label = if (datum.hasArg(i, j) && heads(j) == i && labelHidden) "+" else ""
//      out.write("sslink(%d,%d)\t%s%s\n".format(i, j, label, connectFeatures(tokens, i, j).mkString(" ")))


//  out.write("un(%d,%d)\t%s%s\n".format(i, j, label, "X_DUMMY"))

//        val feats = morphDependency(tokens, i, j).mkString(" ")
//        out.write("un(%d,%d)\t%s%s\n".format(i, j, label, feats))
//        val potname = "un(%d,%d)".format(i, j)
//        ex.potentials += new Potential(1.0, potname, label)


/*
    for (t <- tokens) {
      feats += "[sense-unigram]-%s-%s".format(token.lemma, t.word)
    }
    feats += "[morph-string]-%s".format(token.morph)
    for (f <- token.morph.split("\\|")) {
      feats += "[morph-feat]-%s".format(f)
    }

    for (offset <- idx-window to idx+window if (offset > 0 && offset < slen && offset != idx)) {
      val otoken = tokens(offset)
      val ocap = if (capitalized(otoken.word)) "UC" else "LC"
      feats += "[offset-%d-word]-%s".format(offset, otoken.word)
      feats += "[offset-%d-tag]-%s".format(offset, otoken.pos)
      feats += "[offset-%d-word-tag]-%s-%s".format(offset, otoken.word, otoken.pos)
      feats += "[offset-%d-cap]-%s".format(offset, ocap)
      feats += "[offset-%d-pos-opos]-%s-%s".format(offset, token.pos, otoken.pos)
      feats += "[offset-%d-word-opos]-%s-%s".format(offset, token.word, otoken.pos)
      feats += "[offset-%d-word-oword]-%s-%s".format(offset, token.word, otoken.word)
      feats += "[offset-%d-pos-opos]-%s-%s".format(offset, cap, ocap)
    }

*/

//ex.potentials += new Potential(1.0, potname, !found && alabel == "+" && labelCorrect)
//          out.write("hasArg(%d,%d)\t%s%s\n".format(i, j, alabel, afeatures.mkString(" ")))
//          out.write("sense(%d,0)\t%ssense-%s\n".format(i, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
//  out.write("sense(%d,%d)\t%ssense-%s\n".format(i, senseCount, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
//  out.write("hasLabel(%d,%d,%d)\t%s%s\n".format(i, j, k, if (labelCorrect) "+" else "", builder.toString.trim))


/*
          val builder = new StringBuilder()
          for (f <- afeatures) builder.append(" " + roles(k) + "_" + f)
          if (datum.hasArgLabel(i, j, roles(k))) {
            builder.clear
            found = true
          }
          else {
            out.write("hasLabel(%d,%d,%d)\t%s\n".format(i, j, k, builder.toString.trim))
          }
        }
        if (!found && alabel == "+" && labelCorrect) {
          out.write("hasLabel(%d,%d,%d)\t+0\n".format(i, j, roles.size))
        }
        else {
          out.write("hasLabel(%d,%d,%d)\t0\n".format(i, j, roles.size))
        }
      }
    }
  ex  // new PotentialExample(attributes, potentials, featureMap)
}
*/



/*
    println("Found datum:")
    println(datum)
    if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
    val slen = datum.slen
    out.write("@slen\t%d\n".format(slen))
    out.write("@words\t%s\n".format(datum.words.mkString(" ")))
    out.write("@bigram\tfalse\n")
    rout.write("@slen\t%d\n".format(slen))
    rout.write("@words\t%s\n".format(datum.words.mkString(" ")))
    rout.write("@bigram\tfalse\n")
    datum.words.zipWithIndex.foreach { case(word, widx) =>
      val feats = unigramFeatures(datum, widx+1, useMorph=false, useSyntax=false)
      val tags = if (dict.contains(word)) dict.tags(word).toArray else alltags
      tags.zipWithIndex.foreach { case(tag, tidx) =>
        //				dict.getOrElse(word, alltags).toArray.zipWithIndex.foreach { case(tag, tidx) =>
        val builder = new StringBuilder()
        for (f <- feats) builder.append(" " + tag + "_" + f)
        val isCorrect = correct(datum, widx+1, params.MODE) == tag //datum.postag(widx+1) == tag
        val ll = if (useIndices) tidx else tag
        out.write("ulabel(%d,%s)\t%s%s\n".format(widx+1, tag, if (isCorrect) "+" else "", builder.toString().trim))
        rout.write("ulabel(%d,%d)\t%s%s\n".format(widx+1, tidx, if (isCorrect) "+" else "", builder.toString().trim))
      }
    }
    out.write("\n")
    rout.write("\n")
  }
  out.close()
  rout.close()
}
*/


/*
  def extractFeatures(trainFile: String, trainFeatureFile: String, roles: Array[String], senseDict: HashMap[String, Array[String]], params: SRLParams) = {
    val useIndices = false
    val in = trainFile
    val out = new FileWriter(trainFeatureFile)
    val rout = new FileWriter(trainFeatureFile + ".bpdp")
    val util = new ChunkReader(in)
    util.zipWithIndex.foreach { case(chunk, i) =>
      val datum = SRLDatum.constructFromCoNLL(chunk.split("\n"))
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = datum.slen
      val gpreds = datum.predicates
      out.write("@slen\t%d\n".format(slen))
      out.write("@maxdist\t%d\n".format(1000))
      out.write("@roles\t%s\n".format(roles.mkString(" ") + " A-DUMMY"))
      out.write("@gpreds\t0 %s\n".format(gpreds.mkString(" ")))

      extractSRLFeatures(datum, roles, senseDict, out, labelCorrect=true, prune=false, maxdist=1000,
                         srlmode=1, sensemode=1, argfeats=true)

      extractSyntacticFeatures(datum, out, labelHidden=true, mode=1)

      extractConnectionFeatures(datum, out, labelHidden=true, gpreds=gpreds, abound=1000)

        out.write("\n")
      rout.write("\n")
    }
    out.close()
    rout.close()
  }
*/


/*


		def morphDependency(otokens: Array[SRLToken], ohead: Int, odep: Int, morph: Boolean = true): Array[String] = {
			val feats = new ArrayBuffer[String]

			val stoken = new SRLToken("LEFFT_W", "LEFT_L", "LEFT_P", "LEFT_CP")
			val etoken = new SRLToken("RIGHT_W", "RIGHT_L", "RIGHT_P", "RIGHT_CP")
			val tokens = Array(stoken) ++ otokens ++ Array(etoken)
			val head = ohead + 1
			val dep  = odep + 1

			val dir  = if (dep > head) "R" else "L"
			val dist = Math.abs(head - dep - 1)
			val htoken = tokens(head)
			val dtoken = tokens(dep)

			val small = if (dep < head) dep else head
			val large = if (dep > head) dep else head

			feats += "1,3:%s,%s;".format(htoken.word, htoken.pos)
			feats += "1,3:%s,%s;&%s%d".format(htoken.word, htoken.pos, dir, dist)
			feats += "1,3:%s,%s;%s,%s".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos)
			feats += "1,3:%s,%s;%s,%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos, dir, dist)
			feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.word)
			feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dir, dist)
			feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.pos)
			feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.pos, dir, dist)

			feats += "2:%s;".format(htoken.lemma)
			feats += "2:%s;&%s%d".format(htoken.lemma, dir, dist)
			feats += "2:%s;1:%s".format(htoken.lemma, dtoken.word, dir, dist)
			feats += "2:%s;1:%s:&%s%d".format(htoken.lemma, dtoken.word, dir, dist)
			feats += "2:%s;1,3:%s,%s".format(htoken.lemma, dtoken.lemma, dtoken.pos)
			feats += "2:%s;1,3:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.pos, dir, dist)
			feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
			feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
			feats += "2:%s;2:%s".format(htoken.lemma, dtoken.lemma)
			feats += "2:%s;2:%s&%s%d".format(htoken.lemma, dtoken.lemma, dir, dist)
			feats += "2:%s;3:%s".format(htoken.lemma, dtoken.pos)
			feats += "2:%s;3:%s&%s%d".format(htoken.lemma, dtoken.pos, dir, dist)
			feats += "2:%s;4:%s".format(htoken.lemma, dtoken.cpos)
			feats += "2:%s;4:%s&%s%d".format(htoken.lemma, dtoken.cpos, dir, dist)

			feats += "adj3:%s,%s,%s,%s,%s".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "")
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "", dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "")
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "", dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)

			if (morph) {
				feats += "P-%sX-A-%s".format(htoken.morph, dtoken.morph)
				val m1s = htoken.morph.split("\\|")
				val m2s = htoken.morph.split("\\|")
				for (m1 <- m1s; m2 <- m2s) {
					feats += "S-H-%sxD-%s".format(m1, m2)
				}
			}

			return feats.map(_.replaceAll("=", "-")).toArray
		}

	def argFeatures(idx: Int, tokens: Array[SRLToken], mode: Int = 2): Array[String] = {
			val feats = new ArrayBuffer[String]
			val token = tokens(idx)
			val cap = if (capitalized(token.word)) "iiaUC" else "iiaLC"
			feats += "isArg"
			feats += "ia-word-%s".format(token.word)
			feats += "ia-pos-%s".format(token.pos)
			feats += "ia-word%s-iapos-%s".format(token.word, token.pos)
			feats += "ia-cap-%s".format(cap)
			feats += "ia-cap-%s-%s".format(cap, token.pos)

			if (mode == 2) {
				if (idx > 1) {
					val ptoken = tokens(idx-1)
					val pcap = if (capitalized(ptoken.word)) "ipUC" else "ipLC"
					feats += "iak-%s".format(ptoken.word)
					feats += "iak-%s".format(ptoken.pos)
					feats += "iak-%s-%s".format(ptoken.word, ptoken.pos)
					feats == pcap
					feats += "%s-iak%s".format(token.pos,  ptoken.pos)
					feats += "%s-iak%s".format(token.word, ptoken.pos)
					feats += "%s-iak%s".format(token.word, ptoken.word)
					feats += "%s-iak%s".format(cap, pcap)
				}
				if (idx < tokens.size-1) {
					val atoken = tokens(idx+1)
					val acap = if (capitalized(atoken.word)) "iaUC" else "iaLC"
					feats += "iaa-%s".format(atoken.word)
					feats += "iaa-%s".format(atoken.pos)
					feats += "iaa-%s-%s".format(atoken.word, atoken.pos)
					feats += acap
					feats += "%s-iaa%s".format(token.pos,  atoken.pos)
					feats += "%s-iaa%s".format(token.word, atoken.pos)
					feats += "%s-iaa%s".format(token.word, atoken.word)
					feats += "%s-iaa%s".format(cap, acap)
				}
			}
			return feats.map(_.replaceAll("=", "-")).toArray
		}



//  Seems to hurt performance
//			feats += "p-morph-%s".format(token.morph)
//			for (f <- token.morph.split("\\|")) {
//				feats += "morph-s-%s".format(f)
//			}


//			if (mode == 2) {  // This only seems to hurt performance as well
				val w = 3
				val lb = if (idx-w < 0) 0 else idx-w
				val ub = if (idx+w > slen-1) slen-1 else idx+w
				for (i <- lb to ub) {
					feats += "ctxt-%s".format(tokens(idx).word)
				}


				if (idx > 1) {
					val ptoken = tokens(idx-1)
					val pcap = if (capitalized(ptoken.word)) "pUC" else "pLC"
					feats += "p-%s".format(ptoken.word)
					feats += "p-%s".format(ptoken.pos)
					feats += "p-%s-%s".format(ptoken.word, ptoken.pos)
					feats == pcap
					feats += "%s-p%s".format(token.pos,  ptoken.pos)
					feats += "%s-p%s".format(token.word, ptoken.pos)
					feats += "%s-p%s".format(token.word, ptoken.word)
					feats += "%s-p%s".format(cap, pcap)
				}
				if (idx < tokens.size-1) {
					val atoken = tokens(idx+1)
					val acap = if (capitalized(atoken.word)) "aUC" else "aLC"
					feats += "a-%s".format(atoken.word)
					feats += "a-%s".format(atoken.pos)
					feats += "a-%s-%s".format(atoken.word, atoken.pos)
					feats += acap
					feats += "%s-a%s".format(token.pos,  atoken.pos)
					feats += "%s-a%s".format(token.word, atoken.pos)
					feats += "%s-a%s".format(token.word, atoken.word)
					feats += "%s-a%s".format(cap, acap)
				}
//			}
*/


/*
    def predicateFeatures(idx: Int, tokens: Array[SRLToken], mode: Int = 2): Array[String] = {
    val feats = new ArrayBuffer[String]
    val token = tokens(idx)
    val cap = if (capitalized(token.word)) "UC" else "LC"
    feats += "pred"
    feats += "ps-word-%s".format(token.word)
    feats += "ps-pos-%s".format(token.pos)
    feats += "ps-word%s-pspos-%s".format(token.word, token.pos)
    feats += "pcap-%s".format(cap)

    if (mode == 2) {
      if (idx > 1) {
        val ptoken = tokens(idx-1)
        val pcap = if (capitalized(ptoken.word)) "pUC" else "pLC"
        feats += "p-%s".format(ptoken.word)
        feats += "p-%s".format(ptoken.pos)
        feats += "p-%s-%s".format(ptoken.word, ptoken.pos)
        feats == pcap
        feats += "%s-p%s".format(token.pos,  ptoken.pos)
        feats += "%s-p%s".format(token.word, ptoken.pos)
        feats += "%s-p%s".format(token.word, ptoken.word)
        feats += "%s-p%s".format(cap, pcap)
      }
      if (idx < tokens.size-1) {
        val atoken = tokens(idx+1)
        val acap = if (capitalized(atoken.word)) "aUC" else "aLC"
        feats += "a-%s".format(atoken.word)
        feats += "a-%s".format(atoken.pos)
        feats += "a-%s-%s".format(atoken.word, atoken.pos)
        feats += acap
        feats += "%s-a%s".format(token.pos,  atoken.pos)
        feats += "%s-a%s".format(token.word, atoken.pos)
        feats += "%s-a%s".format(token.word, atoken.word)
        feats += "%s-a%s".format(cap, acap)
      }
    }
    return feats.map(_.replaceAll("=", "-")).toArray
  }
  */

/*
package narad.nlp.srl
import java.io._

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import narad.util.ArgParser
import narad.io.util.SRLReader

object SRLFeaturizer {
	val dummyToken = SRLToken("ROOT", "ROOT_LEMMA", "ROOT_POS", "ROOT_CPOS")

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename  = options.getString("--srl.file")
		val format    = options.getString("--format", "CoNLL09")
		val verbose   = options.getBoolean("--verbose", false)
		val printInterval  = options.getInt("--print.interval", 50)
		val prune  = options.getBoolean("--prune", true)
		val feats  = options.getString("--mode", "r11s1c1")
				val roles = readLines(options.getString("--arg.file", "srl.args"))
			val maxdist = 1000 // readLines(options.getString("--dist.file", "srl.dist"))(0).toInt
		//		val maxsuffix = readLines(options.getString("--suffix.file", "srl.suffixes"))(0)
				val senseFile = new File(options.getString("--sense.file", "srl.senses"))
				val senseDict = new HashMap[String, Array[String]]
				for (line <- io.Source.fromFile(senseFile).getLines()) {val cols = line.split("\t"); senseDict(cols(0)) = cols(1).split(" ")}

				val skipSyntax    = options.getBoolean("--skip.syntax", false)
				val predictSenses = options.getBoolean("--predict.senses", true)


		//		val filterExamples = options.getBoolean("--filter.examples", false)
		var i = 1
		val startTime = System.nanoTime
		for (datum <- SRLReader.iterator(options)) {
			if (i % printInterval == 0) System.err.println("  example %d...".format(i))
			try {
				featurize(datum, feats, senseDict, roles, maxdist, prune, i)
			}
			catch {
				case e: Exception => {
					System.err.println("Error trying to featurize sentence %d:\n  %s".format(i, datum.forms.mkString(" ")))
					System.err.println(e.getStackTrace.mkString("\n"))
					System.exit(1)
				}
			}
			i += 1
		}
		val elapsed = (System.nanoTime - startTime)/1000000000.0
		System.err.println("Elapsed time: " + elapsed + " seconds.")
	}

	// r = roles, s = syntax, c = connect, capitals mean add "+" to correct examples
	def featurize(datum: SRLDatum, feats: String = "r11s1c1", senseDict: HashMap[String, Array[String]],
								roles: Array[String], maxdist: Int, prune: Boolean = true, i: Int) = {
		val lfeats = feats.toLowerCase
		val srl     = feats.contains("r") || feats.contains("R")
		val syntax  = feats.contains("s") || feats.contains("S")
		val connect = feats.contains("c") || feats.contains("C")
		val labelSRL = feats.contains("R")
		val labelSyntax = feats.contains("S")
		val labelConnect = feats.contains("C")
		val srlmode     = if (srl)     lfeats.substring(lfeats.indexOf("r")+1, lfeats.indexOf("r")+2).toInt else 1
		val sensemode   = if (srl)     lfeats.substring(lfeats.indexOf("r")+2, lfeats.indexOf("r")+3).toInt else 1
		val syntaxmode  = if (syntax)  lfeats.substring(lfeats.indexOf("s")+1, lfeats.indexOf("s")+2).toInt else 1
		val connectmode = if (connect) lfeats.substring(lfeats.indexOf("c")+1, lfeats.indexOf("c")+2).toInt else 1

		val tokens = Array(dummyToken) ++ datum.tokens
		val validDatum = datum.predicates.size > 0

		val slen = datum.slen
		println("@example\t%d".format(i))
		println("@slen\t%d".format(slen))
		println("@words\t%s".format(tokens.tail.map(_.word).mkString(" ")))
		println("@lemmas\t%s".format(datum.lemmas.mkString(" ")))
		println("@tags\t%s".format(tokens.tail.map(_.pos).mkString(" ")))
		println("@roles\t%s".format(roles.mkString(" ") + " A-DUMMY"))
		println("@gpreds\t0 %s".format(datum.predicates.mkString(" ")))
		println("@maxdist\t%s".format(maxdist))
		println("DUMMY\tXXXXX")

		if (validDatum) {
			if (srl)
				extractSRLFeatures(datum, roles, senseDict, labelSRL, prune, maxdist, srlmode)
			if (syntax)
				extractSyntacticFeatures(datum, labelSyntax, syntaxmode)
			if (connect)
				extractConnectionFeatures(datum, labelConnect, datum.predicates.toArray, maxdist)
		}
		println
	}

	def extractSRLFeatures(datum: SRLDatum, roles: Array[String], senseDict: HashMap[String, Array[String]],
		labelCorrect: Boolean, prune: Boolean = true, maxdist: Int, srlmode: Int = 1, sensemode: Int = 1, argfeats: Boolean = false) = {
		val slen = datum.slen
		val tokens = Array(dummyToken) ++ datum.tokens ++ Array(dummyToken)
		for (i <- 1 to slen if datum.hasPred(i)) {
			val lemma = datum.lemma(i)
			val senseFeats = SRLFeatures.senseFeatures(i, tokens)
			val senses = senseDict.getOrElse(lemma, Array[String](lemma + "_UNK"))
			if (senseDict.contains(lemma)) {
				var senseCount = 0
				if (senseDict.contains(datum.lemmas(i-1))) {
					for (sense <- senses) {
						val senseLabel = if (datum.hasSense(i, sense) && labelCorrect) "+" else ""
						println("sense(%d,%d)\t%ssense-%s".format(i, senseCount, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
						senseCount += 1
					}
				}
				else {
					val sense = senses(0)
					val senseLabel = if (labelCorrect) "+" else ""
					println("sense(%d,0)\t%ssense-%s".format(i, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
				}
			}
/*
			// Predicate Features
//			println("pred(%d)\t+pred-%S".format(i, datum.words(i-1)))
			val senseFeats = SRLFeatures.senseFeatures(i, tokens, sensemode)
			val senses = senseDict.getOrElse(datum.lemmas(i-1), Array[String](datum.lemmas(i-1) + "." + maxsuffix))
			var senseCount = 0
			if (senseDict.contains(datum.lemmas(i-1))) {
				for (sense <- senses) {
					val senseLabel = if (datum.hasSense(i, sense) && labelCorrect) "+" else ""
					println("sense(%d,%d)\t%ssense-%s".format(i, senseCount, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
					senseCount += 1
				}
			}
			else {
				val sense = senses(0)
				val senseLabel = if (labelCorrect) "+" else ""
				println("sense(%d,0)\t%spred-%s".format(i, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))
			}
*/
			// Argument Features
			if (srlmode > 0) {
							val abound = if (prune) maxdist else slen
							for (j <- 1 to slen if Math.abs(i-j) <= abound) {
								val afeatures = SRLFeatures.argumentFeatures(j, i, tokens, srlmode)
								val alabel = if (datum.hasArg(i, j) && labelCorrect) "+" else ""
								println("hasArg(%d,%d)\t%s%s".format(i, j, alabel, afeatures.mkString(" ")))
								var found = false
								for (k <- 0 until roles.size) {
									val builder = new StringBuilder()
									for (f <- afeatures) builder.append(" " + roles(k) + "_" + f)

									if (datum.hasArgLabel(i, j, roles(k)) && labelCorrect) {
										println("hasLabel(%d,%d,%d)\t+%s".format(i, j, k, builder.toString.trim))
				//						println("hasLabel(%d,%d,%d)\t+%s".format(i, j, k, afeatures.map("%s-%s".format(roles(k), _)).mkString(" ")))
										found = true
									}
									else {
										println("hasLabel(%d,%d,%d)\t%s".format(i, j, k, builder.toString.trim))
									}
								}
								// Add in a dummy label in case we have pruned away this arg's real label
								if (!found && alabel == "+" && labelCorrect) {
									println("hasLabel(%d,%d,%d)\t+A-DUMMY-F".format(i, j, roles.size))
								}
								else {
									println("hasLabel(%d,%d,%d)\tA-DUMMY-F".format(i, j, roles.size))
								}
							}
			}
		}
		if (argfeats) {
			for (i <- 1 to slen) {
				var isArg = false
				for (j <- 1 to slen) {
					if (datum.hasArg(j,i)) isArg = true
				}
				val isArgLabel = if (isArg && labelCorrect) "+" else ""
				println("isArg(%d)\t%s%s".format(i, isArgLabel, SRLFeatures.argFeatures(i, tokens).mkString(" ")))
			}
		}
	}



		def extractConnectionFeatures(datum: SRLDatum, labelHidden: Boolean = true, gpreds: Array[Int], abound: Int) = {
			val slen = datum.slen
			val words = datum.forms
			val tags  = datum.postags
			val tokens = Array(dummyToken) ++ datum.tokens
			val heads = Array(-1) ++ datum.heads //Array(-1) ++ lines.map(_.split("\t")(8).toInt)
			for (i <- 0 to slen; j <- 1 to slen if i != j && datum.hasPred(i) && Math.abs(i-j) <= abound) {
				val label = if (datum.hasArg(i, j) && heads(j) == i && labelHidden) "+" else ""
				println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
			}
		}

		def extractSyntacticFeatures(datum: SRLDatum, labelHidden: Boolean = true, mode: Int = 1) = {
			val slen = datum.slen
			val tokens = Array(dummyToken) ++ datum.tokens
			val heads = Array(-1) ++ datum.heads
			val skip = mode == 0
			for (i <- 0 to slen; j <- 1 to slen if i != j) {
				val label = if (heads(j) == i && labelHidden) "+" else ""
				if (skip) {
					println("un(%d,%d)\t%s%s".format(i, j, label, "X_DUMMY"))
				}
				else {
					val feats = SRLFeatures.morphDependency(tokens, i, j).mkString(" ")
					println("un(%d,%d)\t%s%s".format(i, j, label, feats))
				}
			}
		}

		def readLines(filename: String): Array[String] = {
			val lines = new ArrayBuffer[String]
			var src = io.Source.fromFile(new File(filename))
			try {
			  src.getLines.foreach(l => lines += l)
			}
			finally src match { case b: scala.io.BufferedSource => b.close }
			return lines.toArray
		}
	}












































/*
for (i <- gpreds; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && datum.hasArg(i, j) && labelHidden) "+" else ""
println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
}
}
*/

/*
@slen   22
@words  Rolls - Royce Motor Cars Inc. said it expects its U.S. sales to remain steady at about 1,200 cars in 1990 .
@tags   NNP HYPH NNP NNP NNS NNP VBD PRP VBZ PRP$ NNP NNS TO VB JJ IN CD NN NNS IN CD .
@roles  A1 A3 A0 AM-LOC AM-MNR AM-TMP
@gpreds 0 5 7 9 12 14 19
DUMMY   1
pred(5) 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21



println("@slen\t%d".format(slen))
println("@words\t%s".format(tokens.tail.map(_.word).mkString(" ")))
println("@tags\t%s".format(tokens.tail.map(_.pos).mkString(" ")))
println("@roles\t%s".format(args.mkString(" ")))
println("@gpreds\t0 %s".format(gpreds.mkString(" ")))
println("DUMMY\tXXXXX")

if (srl) {
for (i <- 0 until slen) {
if (datum.hasPred(i))
val plabel = if (datum.hasPred(i)) "+" else ""
println("pred(%d)\t%s%s".format(i, plabel, SRLFeatures.predicateFeatures(i, tokens).mkString(" ")))
for (j <- 1 to slen) {
val afeatures = SRLFeatures.argumentFeatures(j, i, tokens, srlmode)
val alabel = if (datum.hasArg(i, j)) "+" else ""
println("hasArg(%d,%d)\t%s%s".format(i, j, alabel, afeatures.mkString(" ")))

for (k <- 0 until args.size) {
val llabel = if (datum.hasLabel(i, j, args(k))) "+" else ""
println("hasLabel(%d,%d,%d)\t%s%s".format(i, j, k, llabel, afeatures.map("%s-%s".format(args(k), _)).mkString(" ")))
}
}
}
}
if (syntax) extractSyntacticFeatures(datum, !(srl && !labelHidden), skipSyntax)
if (connect) extractConnectionFeatures(datum, labelHidden, gpreds)
println
}

def extractConnectionFeatures(datum: SRLDatum, labelHidden: Boolean = true, gpreds: Array[Int]) = {
val slen = datum.size
val words = datum.words
val tags  = datum.tags
val tokens = Array(dummyToken) ++ datum.tokens
val heads = Array(-1) ++ datum.heads //Array(-1) ++ lines.map(_.split("\t")(8).toInt)
for (i <- gpreds; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && datum.hasArg(i, j) && labelHidden) "+" else ""
println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
}
}

def extractSyntacticFeatures(datum: SRLDatum, labelHidden: Boolean = true, skip: Boolean) = {
val slen = datum.size
val tokens = Array(dummyToken) ++ datum.tokens
val heads = Array(-1) ++ datum.heads //lines.map(_.split("\t")(8).toInt)
for (i <- 0 to slen; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && labelHidden) "+" else ""
if (skip) {
println("un(%d,%d)\t%s%s".format(i, j, label, "X_DUMMY")) //dependencyFeatures(tokens, i, j).mkString(" ")))
}
else {
//				println("un(%d,%d)\t%s%s".format(i, j, label, dependencyFeatures(tokens, i, j).mkString(" "))) //dependencyFeatures(tokens, i, j).mkString(" ")))
println("un(%d,%d)\t%s%s".format(i, j, label, narad.nlp.parse.McDonaldFeatures.extract(tokens, i, j).mkString(" "))) //dependencyFeatures(tokens, i, j).mkString(" ")))
}
}
}

def main(args: Array[String]) = {
val options = new narad.util.ArgParser(args)
val filename  = options.getString("--srl.file")
val format    = options.getString("--format", "CoNLL09")
for (datum <- SRLReader.iterator(options)) {
featurize(datum, options)
}
}
}



val feats	    = options.getString("--feats", "srl")
val labelHidden = options.getBoolean("--label.hidden", false)
val prune = options.getBoolean("--prune", false)
val mode   = options.getString("--mode", "train")
val skipSyntax = options.getBoolean("--skip.syntax", false)
assert(format == "CoNLL09" || format == "CoNLL08", "Invalid SRL format: " + format)

val labels = if (mode == "train")
findLabels(filename, format=format)
else
io.Source.fromFile(labelFile).getLines.toArray

val predtags = if (mode == "train")
findPredTags(filename)
else
io.Source.fromFile(tagFile).getLines.toArray

var count = 0
for (chunk <- ChunkReader.read(filename)) {
if (count % 10 == 0) System.err.println("Extracting features for sentence %d".format(count))
featurize(SRLDatum.constructFromCoNLL(chunk.split("\n"), format), options)
count += 1
}
}
}
*/


// The 0 in the gpreds is just to circumvent a problem where if only
// a single index is valid for a sentence, facidx in bpdp does not update
// the list of gpreds for that example.  The dummy potential is used in
// situations where heavy pruning removes all other potentials and would
// otherwise crash the util.


/*
def featurize(lines: Array[String], labels: Array[String], predtags: Array[String],
srl: Boolean = false, syntax: Boolean = false, connect: Boolean = false,
labelHidden: Boolean = true, prune: Boolean = false, bigram: Int = 0,
format: String, skipSyntax: Boolean = false, mode: Int = 1) = {
*/

*/