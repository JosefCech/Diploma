package DataObjects

import LevelEstimate.LevelAnalyzedSentence
import Xml.Writer
import Xml.XmlWritable

object Estimation {

def writeSentence(analyzed : EstimateSentence, dataSet : String, estimation : String) : Unit = {
	val filePath = segmenter.Configuration.ResultFolder(dataSet) + "/" + estimation +"/" +analyzed.getIdent + ".anx";
	Writer.Write(filePath, analyzed);
 }
}