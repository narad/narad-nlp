package narad.nlp.io
import collection.JavaConversions._
import narad.nlp.ling.Sentence
import edu.stanford.nlp.process.DocumentPreprocessor
import java.io.StringReader
import scala.collection.mutable.ArrayBuffer

object SentenceReader {
	
	def read(paragraph: String): Array[String] = {
		val sentences = new ArrayBuffer[String]
		val reader = new StringReader(paragraph)
		val dp = new DocumentPreprocessor(reader)
		var pos = 0
		for (sentence <- dp.iterator) {
			val text = sentence.map(_.word).mkString(" ")
			sentences += text //new Sentence()
		}		
		return sentences.toArray
	}
	
	def main(args: Array[String]) = {
		read("Here is some text. And here is more text.")
	}
}
	

			
/*		
		List<String> sentenceList = new LinkedList<String>();
		Iterator<List<HasWord>> it = dp.iterator();
		while (it.hasNext()) {
		   StringBuilder sentenceSb = new StringBuilder();
		   List<HasWord> sentence = it.next();
		   for (HasWord token : sentence) {
		      if(sentenceSb.length()>1) {
		         sentenceSb.append(" ");
		      }
		      sentenceSb.append(token);
		   }
		   sentenceList.add(sentenceSb.toString());
		}

		for(String sentence:sentenceList) {
		   System.out.println(sentence);
		}
*/
