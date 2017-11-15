package edu.towson.CIC.COSC455.afabia2.project1


object Compiler {

  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var theEnd      : Boolean = false//test
  var testText : Boolean = false//test


  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    Scanner.start(fileContents)

    while (!theEnd ) {
      Scanner.getNextToken()
      Parser.gittex()
      SemanticAnalyzer.convert()
      if(currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
      System.exit(1)
      }

    }


  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println(args.length)
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println(args(0))
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }

}
