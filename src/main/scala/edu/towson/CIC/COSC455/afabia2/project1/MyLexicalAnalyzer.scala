package edu.towson.CIC.COSC455.afabia2.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var source : List[Char] = Nil
  private var lexeme : String = ""
  private var nextChar : Char = ' '

  val tag = CONSTANTS.whiteSpace


  private val lexems : List[String] = List(
    CONSTANTS.DOCB,CONSTANTS.DOCE, CONSTANTS.TITLEB, CONSTANTS.BRACKETE,
    CONSTANTS.HEADING, CONSTANTS.PARAB, CONSTANTS.PARAE, CONSTANTS.BOLD,
    CONSTANTS.LISTITEM, CONSTANTS.NEWLINE, CONSTANTS.LINKB,
    CONSTANTS.ADDRESSB, CONSTANTS.ADDRESSE, CONSTANTS.IMAGEB, CONSTANTS.DEFB,
    CONSTANTS.EQSIGN, CONSTANTS.USEB
  )
  /*
  I convert the file passed into a list
  easier to work with
   */

  def start(line: String): Unit = {
    source = line.toList
  }
  /*
        checks for empty in source file
 */

  def isEmpty() : Boolean = {
    if(source.isEmpty)
      true
    else
      false
  }
  /*
        checks if its a space
        in each next char
 */
  def isSpace(c: Char): Boolean ={
    c == ' ' | c == '\n' | c == '\t' | c == '\r'
  }
  /*
      keeps looping until is not a space
 */
  def getNonBlank() : Unit =
  {
    while(isSpace(nextChar))
      getChar()
  }
  /*
        adds nextchar to lexeme
 */

  override def addChar(): Unit = {
    lexeme += nextChar
  }
  /*
    checks if is not empty and gets the next char
 */

  override def getChar(): Unit = {
    if(!source.isEmpty){
      nextChar = source.head
      source = source.tail
      nextChar
    }
    else{
      println("Lexical Error: " + nextChar + "is not correct")
      System.exit(1)
    }
  }
  /*
      it gets the next token
 */

  override def getNextToken(): Unit = {
    lexeme = ""
    getNonBlank()

    processText(nextChar)
    println("Current token " + Compiler.currentToken)//test
  }
  /*
      it checks if its a tag if its not then it will get add to lemexe
      and get the next char
      will process text

 */
  def getNormalText(): Unit = {
    getChar()
    while(!tag.contains(nextChar)){
      addChar()
      getChar()
    }
  }
  /*
      looks if it matches the good lexeme
 */

  override def lookup(Candidate: String): Boolean = {
    if(lexems.contains(Candidate)){
      true
    }
    else
      {
        println("Lexical Error: " + nextChar + "is not correct")
        System.exit(1)
        false
      }
  }
/*
in this method nextChar will be checked if it has one of the special char if it does it will add to lexeme
and will check if it is one of the lexeme in this language else it will return an error.
if it is, the token will be added to current token in compiler.
*/

  def processText(chr : Char) =
  {
    chr match {
      case '\\' =>
      {
        addChar()
        while (!isSpace(nextChar) && nextChar != '[')
        {
          if (lexeme.equalsIgnoreCase(CONSTANTS.DOCE))
            Compiler.theEnd = true

          getChar()

          if (!isSpace(nextChar))
          {
            addChar()
          }
        }
        if (lookup(lexeme.toUpperCase()))
        {
          Compiler.currentToken = lexeme.toString
        }
        else
          println("Lexical Error: " + Compiler.currentToken + " was not a legitimate token.")

        if (nextChar == '[')
          getChar()

      }
      case '#' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case '*' =>
      {
        addChar()
        getChar()
        if (nextChar == '*')
        {
          addChar()
          lookup(lexeme)
          Compiler.currentToken = lexeme.toString
          getChar()
        }
        else
        {
          lookup(lexeme)
          Compiler.currentToken = lexeme.toString
        }
      }
      case '+' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case '[' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case '=' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case '!' =>
      {
        addChar()
        getChar()
        if (nextChar == '[')
        {
          addChar()
          getChar()
        }

        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
      }
      case '(' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case ')' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme.toString
        getChar()
      }
      case ']' =>
      {
        addChar()
        lookup(lexeme)
        Compiler.currentToken = lexeme
        getChar()
      }

      case _ =>
      {
        addChar()
        getNormalText()

        Compiler.currentToken = lexeme
        Compiler.testText  = true
      }
    }
  }

}
