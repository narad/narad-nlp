package narad.nlp.parse
import narad.nlp.srl.{SRLToken => RToken, SRLDatum}
import scala.collection.mutable.ArrayBuffer

object McDonaldFeatures {
	
	def extract(otokens: Array[RToken], ohead: Int, odep: Int): Array[String] = {
		val feats = new ArrayBuffer[String]

		val stoken = new RToken("LEFFT_W", "LEFT_L", "LEFT_P", "LEFT_CP")
		val etoken = new RToken("RIGHT_W", "RIGHT_L", "RIGHT_P", "RIGHT_CP")
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
		// Then same with the opposing (coarse or fine) part of speech
		feats += "1,4:%s,%s;".format(htoken.word, htoken.cpos)
		feats += "1,4:%s,%s;&%s%d".format(htoken.word, htoken.cpos, dir, dist)
		feats += "1,4:%s,%s;%s,%s".format(htoken.word, htoken.cpos, dtoken.word, dtoken.cpos)
		feats += "1,4:%s,%s;%s,%s;&%s%d".format(htoken.word, htoken.cpos, dtoken.word, dtoken.cpos, dir, dist)
		feats += "1,4:%s,%s;%s".format(htoken.word, htoken.cpos, dtoken.word)
		feats += "1,4:%s,%s;%s;&%s%d".format(htoken.word, htoken.cpos, dtoken.word, dir, dist)
		feats += "1,4:%s,%s;%s".format(htoken.word, htoken.cpos, dtoken.cpos)
		feats += "1,4:%s,%s;%s;&%s%d".format(htoken.word, htoken.cpos, dtoken.cpos, dir, dist)
		
		// More..
		feats += "1:%s;".format(htoken.word)
		feats += "1:%s;&%s%d".format(htoken.word, dir, dist)
		feats += "1:%s;1,3:%s,%s".format(htoken.word, dtoken.word, dtoken.pos)
		feats += "1:%s;1,3:%s,%s;&%s%d".format(htoken.word, dtoken.word, dtoken.pos, dir, dist)
		feats += "1:%s;1,4:%s,%s;&%s%d".format(htoken.word, dtoken.word, dtoken.cpos, dir, dist)
		feats += "1:%s;1,4:%s,%s;&%s%d".format(htoken.word, dtoken.word, dtoken.cpos, dir, dist)
		feats += "1:%s;1:%s".format(htoken.word, dtoken.word)
		feats += "1:%s;1:%s&%s%d".format(htoken.word, dtoken.word, dir, dist)
		feats += "1:%s;2:%s".format(htoken.word, dtoken.lemma)
		feats += "1:%s;2:%s&%s%d".format(htoken.word, dtoken.lemma, dir, dist)
		feats += "1:%s;3:%s".format(htoken.word, dtoken.pos)
		feats += "1:%s;3:%s&%s%d".format(htoken.word, dtoken.pos, dir, dist)
		feats += "1:%s;4:%s".format(htoken.word, dtoken.cpos)
		feats += "1:%s;4:%s&%s%d".format(htoken.word, dtoken.cpos, dir, dist)
		
		
		// WITH HEAD LEMMAS
		feats += "2,3:%s,%s;".format(htoken.lemma, htoken.pos)
		feats += "2,3:%s,%s;&%s%d".format(htoken.lemma, htoken.pos, dir, dist)
		feats += "2,3:%s,%s;%s,%s".format(htoken.lemma, htoken.pos, dtoken.lemma, dtoken.pos)
		feats += "2,3:%s,%s;%s,%s;&%s%d".format(htoken.lemma, htoken.pos, dtoken.lemma, dtoken.pos, dir, dist)
		feats += "2,3:%s,%s;%s".format(htoken.lemma, htoken.pos, dtoken.lemma)
		feats += "2,3:%s,%s;%s;&%s%d".format(htoken.lemma, htoken.pos, dtoken.lemma, dir, dist)
		feats += "2,3:%s,%s;%s".format(htoken.lemma, htoken.pos, dtoken.pos)
		feats += "2,3:%s,%s;%s;&%s%d".format(htoken.lemma, htoken.pos, dtoken.pos, dir, dist)
		// Then same with the opposing (coarse or fine) part of speech
		feats += "2,4:%s,%s;".format(htoken.lemma, htoken.cpos)
		feats += "2,4:%s,%s;&%s%d".format(htoken.lemma, htoken.cpos, dir, dist)
		feats += "2,4:%s,%s;%s,%s".format(htoken.lemma, htoken.cpos, dtoken.lemma, dtoken.cpos)
		feats += "2,4:%s,%s;%s,%s;&%s%d".format(htoken.lemma, htoken.cpos, dtoken.lemma, dtoken.cpos, dir, dist)
		feats += "2,4:%s,%s;%s".format(htoken.lemma, htoken.cpos, dtoken.lemma)
		feats += "2,4:%s,%s;%s;&%s%d".format(htoken.lemma, htoken.cpos, dtoken.lemma, dir, dist)
		feats += "2,4:%s,%s;%s".format(htoken.lemma, htoken.cpos, dtoken.cpos)
		feats += "2,4:%s,%s;%s;&%s%d".format(htoken.lemma, htoken.cpos, dtoken.cpos, dir, dist)	
		// Backoffs	

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
		
		feats += "3:%s".format(htoken.pos)
		feats += "3:%s;&%s%d".format(htoken.pos, dir, dist)
		feats += "3:%s;1,3:%s,%s".format(htoken.pos, dtoken.word, dtoken.pos, dir, dist)
		feats += "3:%s;1,3:%s,%s&%s%d".format(htoken.pos, dtoken.word, dtoken.pos, dir, dist)
		feats += "3:%s;1:%s".format(htoken.pos, dtoken.word)
		feats += "3:%s;1:%s&%s%d".format(htoken.pos, dtoken.word, dir, dist)
		feats += "3:%s;2,3:%s,%s".format(htoken.pos, dtoken.lemma, dtoken.pos)
		feats += "3:%s;2,3:%s,%s&%s%d".format(htoken.pos, dtoken.lemma, dtoken.pos, dir, dist)
		feats += "3:%s;2:%s".format(htoken.pos, dtoken.lemma)
		feats += "3:%s;2:%s&%s%d".format(htoken.pos, dtoken.lemma, dir, dist)
		feats += "3:%s;3:%s".format(htoken.pos, dtoken.pos)
		feats += "3:%s;3:%s&%s%d".format(htoken.pos, dtoken.pos, dir, dist)

		feats += "4:%s".format(htoken.cpos)
		feats += "4:%s;&%s%d".format(htoken.cpos, dir, dist)
		feats += "4:%s;1,4:%s,%s".format(htoken.cpos, dtoken.word, dtoken.pos, dir, dist)
		feats += "4:%s;1,4:%s,%s&%s%d".format(htoken.cpos, dtoken.word, dtoken.pos, dir, dist)
		feats += "4:%s;1:%s".format(htoken.cpos, dtoken.word)
		feats += "4:%s;1:%s&%s%d".format(htoken.cpos, dtoken.word, dir, dist)
		feats += "4:%s;2,4:%s,%s".format(htoken.cpos, dtoken.lemma, dtoken.pos)
		feats += "4:%s;2,4:%s,%s&%s%d".format(htoken.cpos, dtoken.lemma, dtoken.pos, dir, dist)
		feats += "4:%s;2:%s".format(htoken.cpos, dtoken.lemma)
		feats += "4:%s;2:%s&%s%d".format(htoken.cpos, dtoken.lemma, dir, dist)
		feats += "4:%s;4:%s".format(htoken.cpos, dtoken.pos)
		feats += "4:%s;4:%s&%s%d".format(htoken.cpos, dtoken.pos, dir, dist)
		
		feats += ";1,3:%s,%s".format(dtoken.word, dtoken.pos)
		feats += ";1,3:%s,%s&%s%d".format(dtoken.word, dtoken.pos, dir, dist)
		feats += ";1,4:%s,%s".format(dtoken.word, dtoken.cpos)
		feats += ";1,4:%s,%s&%s%d".format(dtoken.word, dtoken.cpos, dir, dist)
		feats += ";1:%s".format(dtoken.word)
		feats += ";1:%s&%s%d".format(dtoken.word, dir, dist)
		feats += ";2,3:%s,%s".format(dtoken.lemma, dtoken.pos)
		feats += ";2,3:%s,%s&%s%d".format(dtoken.lemma, dtoken.pos, dir, dist)
		feats += ";2,4:%s,%s".format(dtoken.lemma, dtoken.cpos)
		feats += ";2,4:%s,%s&%s%d".format(dtoken.lemma, dtoken.cpos, dir, dist)
		feats += ";2:%s".format(dtoken.lemma)
		feats += ";2:%s&%s%d".format(dtoken.lemma, dir, dist)
		feats += ";3:%s".format(dtoken.pos)
		feats += ";3:%s&%s%d".format(dtoken.pos, dir, dist)
		feats += ";4:%s".format(dtoken.cpos)
		feats += ";4:%s&%s%d".format(dtoken.cpos, dir, dist)
		

		
		val partition = if (dist % 2 != 0) (dist+1) / 2 else dist / 2
		for (cut <- 1 to Math.min(partition, 3)) { //Math.min(Array(partition, 2))) {
			if (partition == cut) {
				feats += "tw3,%s,%s,%s".format(tokens(small).pos, tokens(large).pos, tokens(small+cut).pos)
				feats += "tw3,%s,%s,%s&%s%d".format(tokens(small).pos, tokens(large).pos, tokens(small+1).pos, dir, dist)				
			}			
			feats += "tw3,%s,%s,%s".format(tokens(small).pos, tokens(large).pos, tokens(large-1).pos)
			feats += "tw3,%s,%s,%s&%s%d".format(tokens(small).pos, tokens(large).pos, tokens(large-1).pos, dir, dist)
		}

		for (cut <- 1 to partition) { //Math.min(Array(partition, 2))) {
			if (partition == cut) {
				feats += "tw4,%s,%s,%s".format(tokens(small).cpos, tokens(large).cpos, tokens(small+cut).cpos)
				feats += "tw4,%s,%s,%s&%s%d".format(tokens(small).cpos, tokens(large).cpos, tokens(small+1).cpos, dir, dist)				
			}			
			feats += "tw4,%s,%s,%s".format(tokens(small).cpos, tokens(large).cpos, tokens(large-1).cpos)
			feats += "tw4,%s,%s,%s&%s%d".format(tokens(small).cpos, tokens(large).cpos, tokens(large-1).cpos, dir, dist)
		}
			
/*	
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
			feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
			feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
*/

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

// COARSE TAGS
			feats += "adj4:%s,%s,%s,%s,%s".format("", "", tokens(small+1).cpos, tokens(large-1).cpos, tokens(large).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format("", "", tokens(small+1).cpos, tokens(large-1).cpos, tokens(large).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format("", tokens(small).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos)
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s".format("", tokens(small).cpos, "", tokens(large-1).cpos, tokens(large).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, "", tokens(large-1).cpos, tokens(large).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s".format("", tokens(small).cpos, tokens(small+1).cpos, "", tokens(large).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, tokens(small+1).cpos, "", tokens(large).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format("", tokens(small).cpos, tokens(small+1).cpos, "", tokens(large).cpos, tokens(large+1).cpos)
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, tokens(small+1).cpos, "", tokens(large).cpos, tokens(large+1).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format("", tokens(small).cpos, tokens(small+1).cpos, tokens(large-1).cpos, "", "")
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, tokens(small+1).cpos, tokens(large-1).cpos, "", "", dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format("", tokens(small).cpos, tokens(small+1).cpos, tokens(large-1).cpos, tokens(large).cpos, "")
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).cpos, tokens(small+1).cpos, tokens(large-1).cpos, tokens(large).cpos, "", dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s".format(tokens(small-1).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format(tokens(small-1).cpos, tokens(small).cpos, "", "", "", tokens(large+1).cpos)
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).cpos, tokens(small).cpos, "", "", "", tokens(large+1).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s".format(tokens(small-1).cpos, tokens(small).cpos, "", "", tokens(large).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).cpos, tokens(small).cpos, "", "", tokens(large).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s,%s".format(tokens(small-1).cpos, tokens(small).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos)
			feats += "adj4:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).cpos, tokens(small).cpos, "", "", tokens(large).cpos, tokens(large+1).cpos, dir, dist)
			feats += "adj4:%s,%s,%s,%s,%s".format(tokens(small-1).cpos, tokens(small).cpos, "", tokens(large-1).cpos, tokens(large).cpos)
			feats += "adj4:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).cpos, tokens(small).cpos, "", tokens(large-1).cpos, tokens(large).cpos, dir, dist)
		
		return feats.toArray
	}
	
	def main(args: Array[String]) {
		val text1 = """1	The	the	DT	CDT	The	the	DT	2	NMOD	_	_	_	_	_
		2	economy	economy	NN	NN	economy	economy	NN	4	NMOD	_	A1	_	_	_
		3	's	's	POS	POS	's	's	POS	2	SUFFIX	_	_	_	_	_
		4	temperature	temperature	NN	NN	temperature	temperature	NN	5	SBJ	temperature.01	A2	A1	_	_
		5	Will	will	MD	CMD	will	will	MD	0	ROOT	_	_	AM-MOD	_	_
		6	be	be	VB	VB	be	be	VB	5	VC	_	_	_	_	_
		7	taken	take	VBN	VBN	taken	take	VBN	6	VC	take.01	_	_	_	_
		8	from	from	IN	IN	from	from	IN	7	CLR	_	_	A2	_	_
		9	several	several	JJ	JJ	several	several	DT	11	NMOD	_	_	_	_	_
		10	vantage	vantage	NN	NN	vantage	vantage	NN	11	NMOD	_	_	_	A1	_
		11	points	point	NNS	NNS	points	point	NNS	8	PMOD	point.02	_	_	_	_
		12	this	this	DT	DT	this	this	DT	13	NMOD	_	_	_	_	_
		13	week	week	NN	NN	week	week	NN	7	TMP	_	_	AM-TMP	_	_
		14	,	,	,	,	,	,	,	7	P	_	_	_	_	_
		15	with	with	IN	IN	with	with	IN	7	ADV	_	_	AM-ADV	_	_
		16	readings	reading	NNS	NNS	readings	reading	NNS	15	PMOD	reading.01	_	_	_	_
		17	on	on	IN	IN	on	on	IN	16	NMOD	_	_	_	_	A1
		18	trade	trade	NN	NN	trade	trade	NN	17	PMOD	_	_	_	_	_
		19	,	,	,	,	,	,	,	18	P	_	_	_	_	_
		20	output	output	NN	NN	output	output	NN	18	COORD	_	_	_	_	_
		21	,	,	,	,	,	,	,	20	P	_	_	_	_	_
		22	housing	housing	NN	NN	housing	housing	NN	20	COORD	_	_	_	_	_
		23	and	and	CC	CC	and	and	CC	22	COORD	_	_	_	_	_
		24	inflation	inflation	NN	NN	inflation	inflation	NN	23	CONJ	_	_	_	_	_
		25	.	.	.	.	.	.	.	5	P	_	_	_	_	_"""
		val text2 = """1	The	the	DT	DT	The	the	DT	2	NMOD	_	_	_	_	_
		2	economy	economy	NN	NN	economy	economy	NN	4	NMOD	_	A1	_	_	_
		3	's	's	POS	POS	's	's	POS	2	SUFFIX	_	_	_	_	_
		4	temperature	temperature	NN	NN	temperature	temperature	NN	5	SBJ	temperature.01	A2	A1	_	_
		5	will	will	MD	MD	will	will	MD	0	ROOT	_	_	AM-MOD	_	_
		6	be	be	VB	VB	be	be	VB	5	VC	_	_	_	_	_
		7	taken	take	VBN	VBN	taken	take	VBN	6	VC	take.01	_	_	_	_
		8	from	from	IN	IN	from	from	IN	7	CLR	_	_	A2	_	_
		9	several	several	JJ	JJ	several	several	DT	11	NMOD	_	_	_	_	_
		10	vantage	vantage	NN	NN	vantage	vantage	NN	11	NMOD	_	_	_	A1	_
		11	points	point	NNS	NNS	points	point	NNS	8	PMOD	point.02	_	_	_	_
		12	this	this	DT	DT	this	this	DT	13	NMOD	_	_	_	_	_
		13	week	week	NN	NN	week	week	NN	7	TMP	_	_	AM-TMP	_	_
		14	,	,	,	,	,	,	,	7	P	_	_	_	_	_
		15	with	with	IN	IN	with	with	IN	7	ADV	_	_	AM-ADV	_	_
		16	readings	reading	NNS	NNS	readings	reading	NNS	15	PMOD	reading.01	_	_	_	_
		17	on	on	IN	IN	on	on	IN	16	NMOD	_	_	_	_	A1
		18	trade	trade	NN	NN	trade	trade	NN	17	PMOD	_	_	_	_	_
		19	,	,	,	,	,	,	,	18	P	_	_	_	_	_
		20	output	output	NN	NN	output	output	NN	18	COORD	_	_	_	_	_
		21	,	,	,	,	,	,	,	20	P	_	_	_	_	_
		22	housing	housing	NN	NN	housing	housing	NN	20	COORD	_	_	_	_	_
		23	and	and	CC	CC	and	and	CC	22	COORD	_	_	_	_	_
		24	inflation	inflation	NN	NN	inflation	inflation	NN	23	CONJ	_	_	_	_	_
		25	.	.	.	.	.	.	.	5	P	_	_	_	_	_"""
		val lines = text1.split("\n").map(_.trim)
//		val trimmed = lines map { s => s.reverse.dropWhile ( c => c == ' ').reverse.mkString(System.getProperty("line.seperator"))
		
		val datum = SRLDatum.constructFromCoNLL(lines, format="CoNLL08")
		val tokens = Array(RToken("ROOT", "ROOT_LEMMA", "ROOT_POS", "ROOT_CPOS")) ++ datum.tokens
//		println(tokens.mkString("\n"))
		println(extract(tokens, 5, 1).mkString("\n"))		
	}
	
//	def linearFeatures()
}


/*
 1:<root>;1,3:The,DT 1:<root>;1,3:The,DT&R0 1:<root>;1,4:The,DT 1:<root>;1,4:The,DT&R0 1:<root>;1:The 1:<root>;1:The&R0 1:<root>;2:the 1:<root>;2:the&R0 1:<root>;3:DT 1:<root>;3:DT&R0 1:<root>;4:DT 1:<root>;4:DT&R0 3:<root-CPOS>; 3:<root-CPOS>;&R0 3:<root-CPOS>;1,3:The,DT 3:<root-CPOS>;1,3:The,DT&R0 3:<root-CPOS>;1:The 3:<root-CPOS>;1:The&R0 3:<root-CPOS>;2,3:the,DT 3:<root-CPOS>;2,3:the,DT&R0 3:<root-CPOS>;2:the 3:<root-CPOS>;2:the&R0 3:<root-CPOS>;3:DT 3:<root-CPOS>;3:DT&R0 4:<root-POS>; 4:<root-POS>;&R0 4:<root-POS>;1,4:The,DT 4:<root-POS>;1,4:The,DT&R0 4:<root-POS>;1:The 4:<root-POS>;1:The&R0 4:<root-POS>;2,4:the,DT 4:<root-POS>;2,4:the,DT&R0 4:<root-POS>;2:the 4:<root-POS>;2:the&R0 4:<root-POS>;4:DT 4:<root-POS>;4:DT&R0 ;1,3:The,DT ;1,3:The,DT&R0 ;1,4:The,DT ;1,4:The,DT&R0 ;1:The ;1:The&R0 ;2,3:the,DT ;2,3:the,DT&R0 ;2,4:the,DT ;2,4:the,DT&R0 ;2:the ;2:the&R0 ;3:DT ;3:DT&R0 ;4:DT ;4:DT&R0 adj3:,,MID,MID,DT, adj3:,,MID,MID,DT,&R0 adj3:,<root-CPOS>,,,DT,NN adj3:,<root-CPOS>,,,DT,NN&R0 adj3:,<root-CPOS>,,MID,DT, adj3:,<root-CPOS>,,MID,DT,&R0 adj3:,<root-CPOS>,MID,,DT, adj3:,<root-CPOS>,MID,,DT,&R0 adj3:,<root-CPOS>,MID,,DT,NN adj3:,<root-CPOS>,MID,,DT,NN&R0 adj3:,<root-CPOS>,MID,MID,, adj3:,<root-CPOS>,MID,MID,,&R0 adj3:,<root-CPOS>,MID,MID,DT, adj3:,<root-CPOS>,MID,MID,DT,&R0 adj3:STR,,,,DT,NN adj3:STR,,,,DT,NN&R0 adj3:STR,<root-CPOS>,,,,NN adj3:STR,<root-CPOS>,,,,NN&R0 adj3:STR,<root-CPOS>,,,DT, adj3:STR,<root-CPOS>,,,DT,&R0 adj3:STR,<root-CPOS>,,,DT,NN adj3:STR,<root-CPOS>,,,DT,NN&R0 adj3:STR,<root-CPOS>,,MID,DT, adj3:STR,<root-CPOS>,,MID,DT,&R0 adj4:,,MID,MID,DT, adj4:,,MID,MID,DT,&R0 adj4:,<root-POS>,,,DT,NN adj4:,<root-POS>,,,DT,NN&R0 adj4:,<root-POS>,,MID,DT, adj4:,<root-POS>,,MID,DT,&R0 adj4:,<root-POS>,MID,,DT, adj4:,<root-POS>,MID,,DT,&R0 adj4:,<root-POS>,MID,,DT,NN adj4:,<root-POS>,MID,,DT,NN&R0 adj4:,<root-POS>,MID,MID,, adj4:,<root-POS>,MID,MID,,&R0 adj4:,<root-POS>,MID,MID,DT, adj4:,<root-POS>,MID,MID,DT,&R0 adj4:STR,,,,DT,NN adj4:STR,,,,DT,NN&R0 adj4:STR,<root-POS>,,,,NN adj4:STR,<root-POS>,,,,NN&R0 adj4:STR,<root-POS>,,,DT, adj4:STR,<root-POS>,,,DT,&R0 adj4:STR,<root-POS>,,,DT,NN adj4:STR,<root-POS>,,,DT,NN&R0 adj4:STR,<root-POS>,,MID,DT, adj4:STR,<root-POS>,,MID,DT,&R0
un(2,1)  1:economy;1:The 1:economy;1:The&L0  :economy;2:the 1:economy;2:the&L0 1:economy;3:DT 1:economy;3:DT&L0 1:economy;4:DT 1:economy;4:DT&L0 3:NN; 3:NN;&L0 3:NN;1,3:The,DT 3:NN;1,3:The,DT&L0 3:NN;1:The 3:NN;1:The&L0 3:NN;2,3:the,DT 3:NN;2,3:the,DT&L0 3:NN;2:the 3:NN;2:the&L0 3:NN;3:DT 3:NN;3:DT&L0 4:NN; 4:NN;&L0 4:NN;1,4:The,DT 4:NN;1,4:The,DT&L0 4:NN;1:The 4:NN;1:The&L0 4:NN;2,4:the,DT 4:NN;2,4:the,DT&L0 4:NN;2:the 4:NN;2:the&L0 4:NN;4:DT 4:NN;4:DT&L0 ;1,3:The,DT ;1,3:The,DT&L0 ;1,4:The,DT ;1,4:The,DT&L0 ;1:The ;1:The&L0 ;2,3:the,DT ;2,3:the,DT&L0 ;2,4:the,DT ;2,4:the,DT&L0 ;2:the ;2:the&L0 ;3:DT ;3:DT&L0 ;4:DT ;4:DT&L0 adj3:,,MID,MID,NN, adj3:,,MID,MID,NN,&L0 adj3:,DT,,,NN,POS adj3:,DT,,,NN,POS&L0 adj3:,DT,,MID,NN, adj3:,DT,,MID,NN,&L0 adj3:,DT,MID,,NN, adj3:,DT,MID,,NN,&L0 adj3:,DT,MID,,NN,POS adj3:,DT,MID,,NN,POS&L0 adj3:,DT,MID,MID,, adj3:,DT,MID,MID,,&L0 adj3:,DT,MID,MID,NN, adj3:,DT,MID,MID,NN,&L0 adj3:<root-CPOS>,,,,NN,POS adj3:<root-CPOS>,,,,NN,POS&L0 adj3:<root-CPOS>,DT,,,,POS adj3:<root-CPOS>,DT,,,,POS&L0 adj3:<root-CPOS>,DT,,,NN, adj3:<root-CPOS>,DT,,,NN,&L0 adj3:<root-CPOS>,DT,,,NN,POS adj3:<root-CPOS>,DT,,,NN,POS&L0 adj3:<root-CPOS>,DT,,MID,NN, adj3:<root-CPOS>,DT,,MID,NN,&L0 adj4:,,MID,MID,NN, adj4:,,MID,MID,NN,&L0 adj4:,DT,,,NN,POS adj4:,DT,,,NN,POS&L0 adj4:,DT,,MID,NN, adj4:,DT,,MID,NN,&L0 adj4:,DT,MID,,NN, adj4:,DT,MID,,NN,&L0 adj4:,DT,MID,,NN,POS adj4:,DT,MID,,NN,POS&L0 adj4:,DT,MID,MID,, adj4:,DT,MID,MID,,&L0 adj4:,DT,MID,MID,NN, adj4:,DT,MID,MID,NN,&L0 adj4:<root-POS>,,,,NN,POS adj4:<root-POS>,,,,NN,POS&L0 adj4:<root-POS>,DT,,,,POS adj4:<root-POS>,DT,,,,POS&L0 adj4:<root-POS>,DT,,,NN, adj4:<root-POS>,DT,,,NN,&L0 adj4:<root-POS>,DT,,,NN,POS adj4:<root-POS>,DT,,,NN,POS&L0 adj4:<root-POS>,DT,,MID,NN, adj4:<root-POS>,DT,,MID,NN,&L0
un(8,3)  1:from; 1:from;&L4 1:from;1,3:'s,POS 1:from;1,3:'s,POS&L4 1:from;1,4:'s,POS 1:from;1,4:'s,POS&L4 1:from;1:'s 1:from;1:'s&L4 1:from;3:POS 1:from;3:POS&L4 1:from;4:POS 1:from;4:POS&L4 3:IN; 3:IN;&L4 3:IN;1,3:'s,POS 3:IN;1,3:'s,POS&L4 3:IN;1:'s 3:IN;1:'s&L4 3:IN;3:POS 3:IN;3:POS&L4 4:IN; 4:IN;&L4 4:IN;1,4:'s,POS 4:IN;1,4:'s,POS&L4 4:IN;1:'s 4:IN;1:'s&L4 4:IN;4:POS 4:IN;4:POS&L4 ;1,3:'s,POS ;1,3:'s,POS&L4 ;1,4:'s,POS ;1,4:'s,POS&L4 ;1:'s ;1:'s&L4 ;3:POS ;3:POS&L4 ;4:POS ;4:POS&L4 adj3:,,NN,VBN,IN, adj3:,,NN,VBN,IN,&L4 adj3:,POS,,,IN,JJ adj3:,POS,,,IN,JJ&L4 adj3:,POS,,VBN,IN, adj3:,POS,,VBN,IN,&L4 adj3:,POS,NN,,IN, adj3:,POS,NN,,IN,&L4 adj3:,POS,NN,,IN,JJ adj3:,POS,NN,,IN,JJ&L4 adj3:,POS,NN,VBN,, adj3:,POS,NN,VBN,,&L4 adj3:,POS,NN,VBN,IN, adj3:,POS,NN,VBN,IN,&L4 adj3:NN,,,,IN,JJ adj3:NN,,,,IN,JJ&L4 adj3:NN,POS,,,,JJ adj3:NN,POS,,,,JJ&L4 adj3:NN,POS,,,IN, adj3:NN,POS,,,IN,&L4 adj3:NN,POS,,,IN,JJ adj3:NN,POS,,,IN,JJ&L4 adj3:NN,POS,,VBN,IN, adj3:NN,POS,,VBN,IN,&L4 adj4:,,NN,VBN,IN, adj4:,,NN,VBN,IN,&L4 adj4:,POS,,,IN,JJ adj4:,POS,,,IN,JJ&L4 adj4:,POS,,VBN,IN, adj4:,POS,,VBN,IN,&L4 adj4:,POS,NN,,IN, adj4:,POS,NN,,IN,&L4 adj4:,POS,NN,,IN,JJ adj4:,POS,NN,,IN,JJ&L4 adj4:,POS,NN,VBN,, adj4:,POS,NN,VBN,,&L4 adj4:,POS,NN,VBN,IN, adj4:,POS,NN,VBN,IN,&L4 adj4:NN,,,,IN,JJ adj4:NN,,,,IN,JJ&L4 adj4:NN,POS,,,,JJ adj4:NN,POS,,,,JJ&L4 adj4:NN,POS,,,IN, adj4:NN,POS,,,IN,&L4 adj4:NN,POS,,,IN,JJ adj4:NN,POS,,,IN,JJ&L4 adj4:NN,POS,,VBN,IN, adj4:NN,POS,,VBN,IN,&L4 tw3,POS,IN,MD tw3,POS,IN,MD&L4 tw3,POS,IN,NN tw3,POS,IN,NN&L4 tw3,POS,IN,VB tw3,POS,IN,VB&L4 tw3,POS,IN,VBN tw3,POS,IN,VBN&L4 tw4,POS,IN,MD tw4,POS,IN,MD&L4 tw4,POS,IN,NN tw4,POS,IN,NN&L4 tw4,POS,IN,VB tw4,POS,IN,VB&L4 tw4,POS,IN,VBN tw4,POS,IN,VBN&L4


un(5,1)	
1,3:Will,MD;
1,3:Will,MD;&L3
1,3:Will,MD;1,3:The,DT
1,3:Will,MD;1,3:The,DT&L3
1,3:Will,MD;1:The
1,3:Will,MD;1:The&L3
1,3:Will,MD;3:DT
1,3:Will,MD;3:DT&L3
1,4:Will,CMD;
1,4:Will,CMD;&L3
1,4:Will,CMD;1,4:The,CDT
1,4:Will,CMD;1,4:The,CDT&L3
1,4:Will,CMD;1:The
1,4:Will,CMD;1:The&L3
1,4:Will,CMD;4:CDT
1,4:Will,CMD;4:CDT&L3
1:Will;
1:Will;&L3
1:Will;1,3:The,DT
1:Will;1,3:The,DT&L3
1:Will;1,4:The,CDT
1:Will;1,4:The,CDT&L3
1:Will;1:The
1:Will;1:The&L3
1:Will;2:the
1:Will;2:the&L3
1:Will;3:DT
1:Will;3:DT&L3
1:Will;4:CDT
1:Will;4:CDT&L3
2,3:will,MD;
2,3:will,MD;&L3
2,3:will,MD;2,3:the,DT
2,3:will,MD;2,3:the,DT&L3
2,3:will,MD;2:the
2,3:will,MD;2:the&L3
2,3:will,MD;3:DT
2,3:will,MD;3:DT&L3
2,4:will,CMD;
2,4:will,CMD;&L3
2,4:will,CMD;2,4:the,CDT
2,4:will,CMD;2,4:the,CDT&L3
2,4:will,CMD;2:the
2,4:will,CMD;2:the&L3
2,4:will,CMD;4:CDT
2,4:will,CMD;4:CDT&L3
2:will;
2:will;&L3
2:will;1:The
2:will;1:The&L3
2:will;2,3:the,DT
2:will;2,3:the,DT&L3
2:will;2,4:the,CDT
2:will;2,4:the,CDT&L3
2:will;2:the
2:will;2:the&L3
2:will;3:DT
2:will;3:DT&L3
2:will;4:CDT
2:will;4:CDT&L3
3:MD;
3:MD;&L3
3:MD;1,3:The,DT
3:MD;1,3:The,DT&L3
3:MD;1:The
3:MD;1:The&L3
3:MD;2,3:the,DT
3:MD;2,3:the,DT&L3
3:MD;2:the
3:MD;2:the&L3
3:MD;3:DT
3:MD;3:DT&L3
4:CMD;
4:CMD;&L3
4:CMD;1,4:The,CDT
4:CMD;1,4:The,CDT&L3
4:CMD;1:The
4:CMD;1:The&L3
4:CMD;2,4:the,CDT
4:CMD;2,4:the,CDT&L3
4:CMD;2:the
4:CMD;2:the&L3
4:CMD;4:CDT
4:CMD;4:CDT&L3
;1,3:The,DT
;1,3:The,DT&L3
;1,4:The,CDT
;1,4:The,CDT&L3
;1:The
;1:The&L3
;2,3:the,DT
;2,3:the,DT&L3
;2,4:the,CDT
;2,4:the,CDT&L3
;2:the
;2:the&L3
;3:DT
;3:DT&L3
;4:CDT
;4:CDT&L3
adj3:,,NN,NN,MD,
adj3:,,NN,NN,MD,&L3
adj3:,DT,,,MD,VB
adj3:,DT,,,MD,VB&L3
adj3:,DT,,NN,MD,
adj3:,DT,,NN,MD,&L3
adj3:,DT,NN,,MD,
adj3:,DT,NN,,MD,&L3
adj3:,DT,NN,,MD,VB
adj3:,DT,NN,,MD,VB&L3
adj3:,DT,NN,NN,,
adj3:,DT,NN,NN,,&L3
adj3:,DT,NN,NN,MD,
adj3:,DT,NN,NN,MD,&L3
adj3:<root-CPOS>,,,,MD,VB
adj3:<root-CPOS>,,,,MD,VB&L3
adj3:<root-CPOS>,DT,,,,VB
adj3:<root-CPOS>,DT,,,,VB&L3
adj3:<root-CPOS>,DT,,,MD,
adj3:<root-CPOS>,DT,,,MD,&L3
adj3:<root-CPOS>,DT,,,MD,VB
adj3:<root-CPOS>,DT,,,MD,VB&L3
adj3:<root-CPOS>,DT,,NN,MD,
adj3:<root-CPOS>,DT,,NN,MD,&L3
adj4:,,NN,NN,CMD,
adj4:,,NN,NN,CMD,&L3
adj4:,CDT,,,CMD,VB
adj4:,CDT,,,CMD,VB&L3
adj4:,CDT,,NN,CMD,
adj4:,CDT,,NN,CMD,&L3
adj4:,CDT,NN,,CMD,
adj4:,CDT,NN,,CMD,&L3
adj4:,CDT,NN,,CMD,VB
adj4:,CDT,NN,,CMD,VB&L3
adj4:,CDT,NN,NN,,
adj4:,CDT,NN,NN,,&L3
adj4:,CDT,NN,NN,CMD,
adj4:,CDT,NN,NN,CMD,&L3
adj4:<root-POS>,,,,CMD,VB
adj4:<root-POS>,,,,CMD,VB&L3
adj4:<root-POS>,CDT,,,,VB
adj4:<root-POS>,CDT,,,,VB&L3
adj4:<root-POS>,CDT,,,CMD,
adj4:<root-POS>,CDT,,,CMD,&L3
adj4:<root-POS>,CDT,,,CMD,VB
adj4:<root-POS>,CDT,,,CMD,VB&L3
adj4:<root-POS>,CDT,,NN,CMD,
adj4:<root-POS>,CDT,,NN,CMD,&L3
tw3,DT,MD,NN=2
tw3,DT,MD,NN&L3=2
tw3,DT,MD,POS
tw3,DT,MD,POS&L3
tw4,CDT,CMD,NN=2
tw4,CDT,CMD,NN&L3=2
tw4,CDT,CMD,POS
tw4,CDT,CMD,POS&L3







un(5,1) 
1,3:will,MD; 
1,3:will,MD;&L3 
1,3:will,MD;1,3:The,DT 
1,3:will,MD;1,3:The,DT&L3 
1,3:will,MD;1:The 
1,3:will,MD;1:The&L3 
1,3:will,MD;3:DT
1,3:will,MD;3:DT&L3
1,4:will,MD;
1,4:will,MD;&L3
1,4:will,MD;1,4:The,DT
1,4:will,MD;1,4:The,DT&L3
1,4:will,MD;1:The
1,4:will,MD;1:The&L3
1,4:will,MD;4:DT
1,4:will,MD;4:DT&L3
1:will;
1:will;&L3
1:will;1,3:The,DT
1:will;1,3:The,DT&L3
1:will;1,4:The,DT
1:will;1,4:The,DT&L3
1:will;1:The
1:will;1:The&L3
1:will;2:the
1:will;2:the&L3
1:will;3:DT
1:will;3:DT&L3
1:will;4:DT
1:will;4:DT&L3
3:MD;
3:MD;&L3
3:MD;1,3:The,DT
3:MD;1,3:The,DT&L3
3:MD;1:The
3:MD;1:The&L3
3:MD;2,3:the,DT
3:MD;2,3:the,DT&L3
3:MD;2:the
3:MD;2:the&L3
3:MD;3:DT
3:MD;3:DT&L3
4:MD;
4:MD;&L3
4:MD;1,4:The,DT
4:MD;1,4:The,DT&L3
4:MD;1:The
4:MD;1:The&L3
4:MD;2,4:the,DT
4:MD;2,4:the,DT&L3
4:MD;2:the
4:MD;2:the&L3
4:MD;4:DT
4:MD;4:DT&L3
;1,3:The,DT
;1,3:The,DT&L3
;1,4:The,DT
;1,4:The,DT&L3
;1:The
;1:The&L3
;2,3:the,DT
;2,3:the,DT&L3
;2,4:the,DT
;2,4:the,DT&L3
;2:the
;2:the&L3
;3:DT
;3:DT&L3
;4:DT
;4:DT&L3
adj3:,,NN,NN,MD,
adj3:,,NN,NN,MD,&L3
adj3:,DT,,,MD,VB
adj3:,DT,,,MD,VB&L3
adj3:,DT,,NN,MD,
adj3:,DT,,NN,MD,&L3
adj3:,DT,NN,,MD,
adj3:,DT,NN,,MD,&L3
adj3:,DT,NN,,MD,VB
adj3:,DT,NN,,MD,VB&L3
adj3:,DT,NN,NN,,
adj3:,DT,NN,NN,,&L3
adj3:,DT,NN,NN,MD,
adj3:,DT,NN,NN,MD,&L3
adj3:<root-CPOS>,,,,MD,VB
adj3:<root-CPOS>,,,,MD,VB&L3
adj3:<root-CPOS>,DT,,,,VB
adj3:<root-CPOS>,DT,,,,VB&L3
adj3:<root-CPOS>,DT,,,MD,
adj3:<root-CPOS>,DT,,,MD,&L3
adj3:<root-CPOS>,DT,,,MD,VB
adj3:<root-CPOS>,DT,,,MD,VB&L3
adj3:<root-CPOS>,DT,,NN,MD,
adj3:<root-CPOS>,DT,,NN,MD,&L3
adj4:,,NN,NN,MD,
adj4:,,NN,NN,MD,&L3
adj4:,DT,,,MD,VB
adj4:,DT,,,MD,VB&L3
adj4:,DT,,NN,MD,
adj4:,DT,,NN,MD,&L3
adj4:,DT,NN,,MD,
adj4:,DT,NN,,MD,&L3
adj4:,DT,NN,,MD,VB
adj4:,DT,NN,,MD,VB&L3
adj4:,DT,NN,NN,,
adj4:,DT,NN,NN,,&L3
adj4:,DT,NN,NN,MD,
adj4:,DT,NN,NN,MD,&L3
adj4:<root-POS>,,,,MD,VB
adj4:<root-POS>,,,,MD,VB&L3
adj4:<root-POS>,DT,,,,VB
adj4:<root-POS>,DT,,,,VB&L3
adj4:<root-POS>,DT,,,MD,
adj4:<root-POS>,DT,,,MD,&L3
adj4:<root-POS>,DT,,,MD,VB
adj4:<root-POS>,DT,,,MD,VB&L3
adj4:<root-POS>,DT,,NN,MD,
adj4:<root-POS>,DT,,NN,MD,&L3
tw3,DT,MD,NN=2
tw3,DT,MD,NN&L3=2
tw3,DT,MD,POS
tw3,DT,MD,POS&L3
tw4,DT,MD,NN=2
tw4,DT,MD,NN&L3=2
tw4,DT,MD,POS
tw4,DT,MD,POS&L3
*/































