package narad.io.re;
/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/9/14
 * Time: 6:29 AM
 */
class ReProto {

    public static void main(String[] args) throws Exception {
        System.out.println("Reading <" + args[0] + ">");
    }
}



/*
def fromProtobufInputStream(in: InputStream,  os : PrintStream, tos : PrintStream) : DocumentProtos.Relation = {     //todo:check type

    val msg : DocumentProtos.Relation = DocumentProtos.Relation.parseDelimitedFrom(in)
    if (msg == null) return null

    val srcid = msg.getSourceGuid
    val destid = msg.getDestGuid
    val relType = msg.getRelType
    os.print(srcid + "\t" + destid + "\tREL$" + relType  )      //to distinguish relation labels from other features
    tos.println(srcid + "\t" + destid) //todo: get entity name

    for (index <- 0 until msg.getMentionCount) {
      val mentionMsg = msg.getMention(index)
      for(index <- 0 until mentionMsg.getFeatureCount) os.print("\t" + mentionMsg.getFeature(index))
      val sentence = mentionMsg.getSentence
      tos.println(sentence)
    }
    os.println
    msg
  }

  */