package narad.util.visualize
import scala.collection.mutable.ArrayBuffer
import java.io.File

class HeatMap(grid: Array[Array[Double]], xlabels: Array[String], ylabels: Array[String]) {

	def writeToFile(rFile: File, imageFilename: String="heatmap") = {
		val slen = grid.size
		val ab = new ArrayBuffer[String]
		ab += "#!/usr/bin/env Rscript"
		ab += "library(gplots)"
		ab += ""
		ab += "pdf(\"%s.pdf\")".format(imageFilename)
		//		ab += "hm <- read.csv(f, sep=\",\")"
		ab += "hm = matrix(c("
		for (i <- 0 until grid.size) {
			if (i < grid.size-1) ab += (grid(i).mkString(", ") + ",")
			else ab += (grid(i).mkString(", ") + "), ")
		}
//		for (row <- grid) ab += (row.mkString(", ") + ",")
		ab += "nrow=%d, ncol=%d, byrow=TRUE)".format(grid.size, grid(0).size)
		//		ab += "row.names(hm) <- hm$HEAD"
		ab += "hm <- round(hm, 2)"
//		ab += "hm <- hm[,-1]"
		//		ab += "hm <- apply(t(hm),2,rev) "
		ab += "hm_matrix <- data.matrix(hm)"
		if (xlabels.size > 0) {
			ab += "rownames(hm_matrix) <- c(%s)".format(xlabels.map("\"" + _ + "\"").mkString(","))
			ab += "colnames(hm_matrix) <- c(%s)".format(ylabels.map("\"" + _ + "\"").mkString(","))
		}

		//		ab += "hm_hm <- heatmap(hm_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale=\"column\", margins=c(5,10))"

		ab += "hm_hm <- heatmap.2(hm_matrix,"
		ab += "Rowv=FALSE,"
		ab += "Colv=FALSE,"
		ab += "col = colorRampPalette(c(\"whitesmoke\",\"red2\"))(32),"
		ab += "dendrogram=\"none\","
		ab += "ylab = \"Tag i-1\","
		ab += "xlab = \"Tag i\","
		ab += "cellnote=matrix(nrow=%d, ncol=%d),".format(slen, slen)
		ab += "notecol=\"black\","
		ab += "trace=\"none\","		
		ab += "margins=c(5,5),"
		ab += "colsep=c(1:62),"
		ab += "rowsep=c(1:62),"
		ab += "sepwidth=c(0.05,0.05),"
		ab += "sepcolor=\"white\","
		ab += "key = FALSE)"

		//		lwid = c(0.05,0.05),
		//		  lhei = c(0.05,0.05))


		ab += "dev.off()"

		var out = new java.io.PrintWriter(rFile)
		out.write(ab.mkString("\n"))
		out.close()
	}
}
