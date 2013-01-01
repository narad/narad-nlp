package narad.util
import java.io.FileWriter
import narad.io.reader.ChunkReader

object ChunkPartitioner {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val input = options.getString("--input")
		val ratio = options.getInt("--ratio", 0)
		val bins  = options.getInt("--bins", 0)
		if (ratio > 0) {
			val filename1 = options.getString("--output.1", "out1.txt")
			val filename2 = options.getString("--output.2", "out2.txt")
			val fw1 = new FileWriter(filename1)
			val fw2 = new FileWriter(filename2)
			var i = 1
			for (chunk <- ChunkReader.read(input)) {
				if (i % ratio == 0) {
					fw2.write(chunk + "\n\n")
				}
				else {
					fw1.write(chunk + "\n\n")
				}
				i += 1
			}
			fw1.close()
			fw2.close()			
		}
		else if (bins > 0) {
			val writers = new Array[FileWriter](bins)
			for (i <- 1 to bins) {
				writers(i-1) = new FileWriter(input + "." + i)
			}
			var i = 1
			for (chunk <- ChunkReader.read(input)) {
				writers(i-1).write(chunk + "\n\n")
				if (i % bins == 0) i = 1 else i += 1
			}
			for (i <- 1 to bins) writers(i - 1).close()
		}
	}
}