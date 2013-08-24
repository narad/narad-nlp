package narad.io.tree

import narad.util.ArgParser

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/10/13
 * Time: 9:45 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class TreebankReaderOptions {

  def BINARIZE: Boolean

  def BINARIZE_MODE: String

  def COARSEN_LABELS: Boolean

  def DEFAULT_LABEL: String

  def FIX_ROOT: Boolean

  def REMOVE_NONES: Boolean

  def REMOVE_TOP: Boolean

  def REMOVE_UNARY_CHAINS: Boolean

  def UNIFY_NONTERMS: Boolean
}

class DefaultTreebankReaderOptions extends TreebankReaderOptions {

  def BINARIZE: Boolean = false

  def BINARIZE_MODE = "RIGHT_0MARKOV"

  def COARSEN_LABELS: Boolean = true

  def DEFAULT_LABEL: String = "TOP"

  def FIX_ROOT: Boolean = false

  def REMOVE_NONES: Boolean = true

  def REMOVE_TOP: Boolean = false

  def REMOVE_UNARY_CHAINS: Boolean = false

  def UNIFY_NONTERMS: Boolean = false
}

class OntoNotesTreebankReaderOptions extends DefaultTreebankReaderOptions {

  override def REMOVE_TOP: Boolean = true
}

object TreebankReaderOptions {

  def fromCommandLine(options: ArgParser) = new TreebankReaderOptions {

    def BINARIZE = options.getBoolean("--binarize", false)

    def BINARIZE_MODE = options.getString("--binarize.mode", "RIGHT_0MARKOV")

    def COARSEN_LABELS = options.getBoolean("--coarsen.labels", true)

    def DEFAULT_LABEL = options.getString("--default.label", "TOP")

    def FIX_ROOT = options.getBoolean("--fix.root", false)

    def REMOVE_NONES = options.getBoolean("--remove.nones", true)

    def REMOVE_TOP = options.getBoolean("--remove.top", false)

    def REMOVE_UNARY_CHAINS = options.getBoolean("--remove.unary.chains", false)

    def UNIFY_NONTERMS = options.getBoolean("--unify.nonterms", false)
  }
}