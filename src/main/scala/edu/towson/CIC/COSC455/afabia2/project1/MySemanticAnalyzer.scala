package edu.towson.CIC.COSC455.afabia2.project1
import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}

import scala.collection.mutable

class MySemanticAnalyzer {

  var semanticTree = new scala.collection.mutable.Stack[String]
  var newStack = new scala.collection.mutable.Stack[String]
  var getText : String = ""
  var special: String = ""

  def convert() = {
    semanticTree = Compiler.Parser.parseT.reverse
    getText = semanticTree.pop()
    //println(getText) //check if gets \begin

   while(semanticTree.nonEmpty) {
     if (getText.equalsIgnoreCase(CONSTANTS.DOCB)) {
       newStack.push("<html>\n")
       getText = semanticTree.pop()
     }
     else if (getText.equalsIgnoreCase(CONSTANTS.TITLEB)) {
       newStack.push("<head>\n")
       newStack.push("<title>\n")
       getText = semanticTree.pop()
       while (!CONSTANTS.beginTag.contains(getText)) {
         newStack.push(getText + " ")
         getText = semanticTree.pop()
       }

       newStack.push("</title>\n")
       newStack.push("</head>\n")
       getText = semanticTree.pop()
     }
     else if (getText.equalsIgnoreCase(CONSTANTS.HEADING)) {
       newStack.push("<h1>\n")
       newStack.push(semanticTree.pop() + " ")
       getText = semanticTree.pop()
       while (!CONSTANTS.validTags.contains(getText)) {
         newStack.push(getText + " ")
         getText = semanticTree.pop()
       }
       newStack.push("</h1>\n")
     }
     else if (getText.equalsIgnoreCase(CONSTANTS.PARAB)) {
       newStack.push("<p>")
       getText = semanticTree.pop()
       //while (!CONSTANTS.beginTag.contains(getText)) {
         //newStack.push(getText + " ")
         //getText = semanticTree.pop()
       //}

     }
     else if (getText.equalsIgnoreCase(CONSTANTS.PARAE)) {
       newStack.push("</p>\n")
       getText = semanticTree.pop()
     }
     else if (getText.equalsIgnoreCase(CONSTANTS.BOLD)) {
       newStack.push("<b>\n")
       getText = semanticTree.pop()
       while (!CONSTANTS.validTags.contains(getText)) {
         newStack.push(getText + " ")
         getText = semanticTree.pop()
       }
       newStack.push("</b>\n")
       getText = semanticTree.pop()
     }
     else if (getText.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
       newStack.push("<li>\n")
       getText = semanticTree.pop()
       while (!CONSTANTS.validTags.contains(getText)) {
         newStack.push(getText + " ")
         getText = semanticTree.pop()
       }

     }
     else if (getText.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
       newStack.push("<br>\n")
       getText = semanticTree.pop()
     }
       /*
       *should create a link but did not have enough time
       *
        */
     else if(getText.equalsIgnoreCase(CONSTANTS.LINKB)){
       special = ""
       getText = semanticTree.pop()
       while (CONSTANTS.beginTag.contains(getText)){
         newStack.push(getText + " ")
         getText = semanticTree.pop()

       }

       special = semanticTree.pop()
       newStack.push("<<a href = \">")
       newStack.push(special)

       }
     else if (getText.equalsIgnoreCase(CONSTANTS.DOCE)) {
       newStack.push("</html>\n")
     }
     else if (!CONSTANTS.validTags.contains(getText) && !CONSTANTS.beginTag.contains(getText)) {
       newStack.push(getText + " ")
       getText = semanticTree.pop()
     }

   }

  createHTMLFile()


  }
  def createHTMLFile() : Unit ={
    val printOut = newStack.reverse.mkString
    //println(printOut)
    val printNew = new PrintWriter(new File("TextAlvaro.html"))
    printNew.write(printOut)
    printNew.close()
    openHTMLFileInBrowser("TextAlvaro.html")


  }


  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }

}