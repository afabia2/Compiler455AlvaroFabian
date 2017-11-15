package edu.towson.CIC.COSC455.afabia2.project1

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  /*
  using a stack to build a tree.
  compares current token to variables in CONSTANTS
  if match push it to tree
  and gets the next token
  else it prints an error
  also checks for ends
   */
  var parseT = new scala.collection.mutable.Stack[String]
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()


      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseT.push(Compiler.currentToken)
      }
      else {
        println("SYNTAX ERROR - End of the document was expected when " + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR - Start of Document was expected when " + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }
  /*
         it gets the text in syntax
         uses a boolean
 */

  def genText(): Unit =
  {
    if (Compiler.testText)
    {
      parseT.push(Compiler.currentToken)
      Compiler.testText = false
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("SYNTAX ERROR -  this was given " + Compiler.currentToken + " should have been text")
      System.exit(1)
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
else it prints an error
also checks for ends
 */

  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      genText()

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("SYNTAX ERROR - end of Title was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }

    }
    else {
      println("SYNTAX ERROR - Title Variable was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }
  /*
  using a stack to build a tree.
  compares current token to variables in CONSTANTS
  if match push it to tree
  and gets the next token
  else it prints an error
  also checks for ends
   */
  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //variable defined
      //innertext
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }

      innerText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR - End of the paragraph was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }

    }
    else{
      println("SYNTAX ERROR - Start of the paragraph was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }
  /*
  helper
   */

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      bold()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      link()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      variableUse()
    }
    else if (Compiler.testText)
    {
      genText()
    }
  }
  /*
  helper
  it would call it self
   */

  override def innerText(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))
    {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM))
    {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
    {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
    {
      newline()
      innerText()
    }
    else if (Compiler.testText)
    {
      genText()
      innerText()
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
else it prints an error
also checks for ends
 */

  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parseT.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          genText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parseT.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else{
            println("SYNTAX ERROR - Address end of Link was expected when '" + Compiler.currentToken + "' was found.")
            System.exit(1)
          }
        }
        else{
          println("SYNTAX ERROR - Address Begin of Link was expected when '" + Compiler.currentToken + "' was found.")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR - end of Bracket for Link was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
    }

  }
  /*
  main method it calls the needed methods
   */

  override def body(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      newline()
      body()
    }
    else if (Compiler.currentToken == CONSTANTS.USEB || Compiler.currentToken == CONSTANTS.HEADING ||
      Compiler.currentToken == CONSTANTS.BOLD || Compiler.currentToken == CONSTANTS.LISTITEM || Compiler.currentToken == CONSTANTS.IMAGEB ||
      Compiler.currentToken == CONSTANTS.LINKB || Compiler.currentToken == CONSTANTS.NEWLINE || Compiler.testText)
    {
      innerText()
      body()
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
does not need to happen
 */

  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()

    }
    else{
      println("SYNTAX ERROR - Bold Token was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
does not need to happen
*/

  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }

  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
else it prints an error
also checks for ends
*/

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        genText()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseT.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          variableDefine()
        }
        else{
          println("SYNTAX ERROR - end of Bracket for Variable Defined was expected when '" + Compiler.currentToken + "' was found.")
        }
      }
      else{
        println("SYNTAX ERROR - Equal sign expected when '" + Compiler.currentToken + "' was found.")
      }
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
else it prints an error
also checks for ends
*/

  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parseT.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          genText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parseT.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
        }
        else{
          println("SYNTAX ERROR - Address Begin was expected when '" + Compiler.currentToken + "' was found.")
          System.exit(1)
        }
      }
      else{
        println("SYNTAX ERROR - Address bracket Token was expected when '" + Compiler.currentToken + "' was found.")
        System.exit(1)
      }
    }


  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
else it prints an error
also checks for ends
*/

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parseT.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("SYNTAX ERROR - end of Bracket for Variable Use was expected when '" + Compiler.currentToken + "' was found.")
      }
    }
  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
does not need to happen
*/
  override def heading(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
    }

  }
  /*
using a stack to build a tree.
compares current token to variables in CONSTANTS
if match push it to tree
and gets the next token
does not need to happen
*/
  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      parseT.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      genText()
    }

  }
}