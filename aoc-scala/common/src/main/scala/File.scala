package common.file

import scala.io.Source

def readFileLines(path: String) = readFile(path).split("\n")

def readFile(path: String) = Source.fromFile(path).mkString
