package narad.io.disfluency

import narad.io.tree.DefaultTreebankReaderOptions

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/23/13
 * Time: 4:08 PM
 */
class DisfluencyTreebankReaderOptions extends DefaultTreebankReaderOptions {

  override def DEFAULT_LABEL = "TOP"

  override def COARSEN_LABELS = true

}