package common.file

import scala.io.Source

def readFileLines(path: String) = Source.fromFile(path).getLines