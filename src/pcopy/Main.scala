/**
 * Copy JPEG pictures and videos from digital camera card into given directory.

 * 
 * Existing files are not overridden (existing empty ones might be deleted).
 * 
 * Required: Scala 2.8
 * 
 * If you are on Windows and classes root is current directory and H:\ is where 
 * card is inserted then command line would be
 * <pre>java -cp scala-library.jar;. pcopy.Main  H:\ D:\HomeMedia\a720is</pre>
 * 
 * Example command line if you are using some Unix
 * <pre>java -cp scala-library.jar:. pcopy.Main /media/Kingston /media/disk/HomeMedia/a720is</pre>
 * 
 * @version 0.5
 * @author Petr Gladkikh
 */ 
package pcopy
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.nio.channels.FileChannel
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object Main {
	
	val directoryDateFormat = new java.text.SimpleDateFormat("yyyy_MM_dd")
	
  	def listRecursive(dirName : File, fileFilter : File=>Boolean) : Seq[File] = {
		val nameList = dirName.listFiles()
		if (null == nameList) {
			throw new RuntimeException("Can not get contents of " + dirName + " directory");
		}
		val (dirs, files) = nameList partition {x => x.isDirectory}
		files ++ (dirs filter (fileFilter) flatMap {name => listRecursive(name, fileFilter)}) 
  	}
  
  	def makeDirName(file : File) : String = {
		directoryDateFormat.format(new java.util.Date(file.lastModified))
  	}
  
  	/**
  	 * Make target file name for given source file. trgDir is base of photo storage location.
  	 */
  	def rebase(sourceFile : File, srcDir : File, trgDir : File) : (File, File) = {  		
 		assert(sourceFile.getPath startsWith srcDir.getPath) // just in case
  		val targetFileName = new File(new File(trgDir, makeDirName(sourceFile)), sourceFile.getName)
  		(sourceFile, targetFileName)
  	}
	
	/**
	 * Copy file.
	 * 
	 * Code adapted from http://www.javalobby.org/java/forums/t17036.html
	 */
	def copyFile(sourceFile : File, destFile : File) {
		if (!destFile.getParentFile.exists && !destFile.getParentFile.mkdirs) {
			throw new RuntimeException(
					"Can not create target directory for " + destFile.getCanonicalPath );
		}
		var source : FileChannel = null
		var destination : FileChannel = null
		try {
			// TODO Use some wrapper library around NIO 
			source = new FileInputStream(sourceFile).getChannel();
			try {
				if (!destFile.createNewFile()) {
					throw new RuntimeException(
							"Target file already exists. Name " + destFile.getCanonicalPath );
				}
				destination = new FileOutputStream(destFile).getChannel();				 
				val size = source.size
				var count = 0L;
				while (count < size) 
					 count += destination.transferFrom(source, count, size - count)
			} catch {
				case e : IOException => { destFile.delete(); throw e; }				
			} 		
		} finally {
			if (source != null) {
				source.close();
			}
			if (destination != null) {
				destination.close();
			}
		}
	}
  	
  	def pcopy(sourceDir : File, targetDir : File) : Seq[String] = {	    
	    def dirFilter(dir : File) = !dir.getName.startsWith(".")
	    def fileFilter(file : File) = Seq("jpg", "mov", "thm", "avi", "gp") exists file.getName.toLowerCase.endsWith
	    val errors = new ListBuffer[String] 
	    listRecursive(sourceDir, dirFilter) filter fileFilter foreach { fileName =>
	    	val (fromFile, toFile) = rebase(fileName, sourceDir, targetDir)
	    	if (fromFile.lastModified > System.currentTimeMillis) {
	    		// Detects some cases when camera's clock is set incorrectly.	    		
	    		errors += "Skipped: Modification time of file " + fromFile + " is in future. Check your clock settings."
	    	} else {
		    	if (toFile.exists) {
		    		if (!toFile.isFile) 
		    			errors += "Warning: Target already exists and is not a file " + toFile.getCanonicalPath
					else if (0 == toFile.length) 
						errors += "Warning: Target exists and is empty " + toFile.getCanonicalPath    			
		    		// else just skip
		    	} else {
		    		println(fromFile.getCanonicalPath + " -> " + toFile.getCanonicalPath)
		    		copyFile(fromFile, toFile)
		    	}
	    	}
	    }
	    errors
  	}
  	
  	def main(args : Array[String]) {
  		if (args.length != 2) {  			
  			println("Usage:\n\tpcopy source_dir target_dir")
  		} else {
  	  		val sourceDir = new File(args(0))
  		    val targetDir = new File(args(1))
  		    println("Source dir " + sourceDir.getCanonicalPath)
  		    println("Target dir " + targetDir.getCanonicalPath)
  		    println()
  		    val errors = pcopy(sourceDir, targetDir)
  		    println()
  		    // Print errors at end of output so they are more prominent in long list
  		    errors foreach { msg => println(msg) }
  		    println("Done.")
  		}
  	}
}
