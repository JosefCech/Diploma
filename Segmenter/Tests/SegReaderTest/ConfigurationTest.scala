package SegReaderTest

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import segmenter._
import scala.xml._
import common._
import java.io.File

@RunWith(classOf[JUnitRunner])
class ConfigurationTest extends FunSuite {
  test("test exitst elements") {
   print( (Configuration.root \ "PdtData" \ "@Folder" ))
   print(Configuration.PdtDataFolder)
   Assertions.expect("../GoldenData/Morf")(Configuration.PdtDataFolder)
   Assertions.expect("../GoldenData/Seg")(Configuration.SegDataFolder)
   Assertions.expect("../GoldenData/Seg")(Configuration.DataFolder)
   Assertions.expect("../GoldenData/Output")(Configuration.OutputGoldenFolder)
   Assertions.expect("../GoldenData/Output")(Configuration.OutputOthersFolder)
   1
  }
 
  test("test exist folders") {
    
   def folderMorf  = new File(Configuration.PdtDataFolder).exists
   def folderSeg = new File(Configuration.SegDataFolder).exists
   def folderGoldenOutput = new File(Configuration.OutputGoldenFolder).exists
   def folderOthersOutput = new File(Configuration.OutputOthersFolder).exists
   
   
   Assertions.expect(true)(folderMorf)
   Assertions.expect(true)(folderSeg)
   Assertions.expect(true)(folderGoldenOutput)
   Assertions.expect(true)(folderOthersOutput)  
   1
  }
}