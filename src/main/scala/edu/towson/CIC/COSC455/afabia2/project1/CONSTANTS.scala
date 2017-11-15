package edu.towson.CIC.COSC455.afabia2.project1

object CONSTANTS {

  val DOCB : String = "\\BEGIN"
  val DOCE : String = "\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val BOLD : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = "\\\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["
  val REQTEXT : List[String] = List("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  val TEXT : List[String] = List("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
    "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
    "0","1","2","3","4","5","6","7","8","9",",",".","?","_","/", """ """)
  val whiteSpace : List[Char] = List('\\', '#', '*', '[' , '!', ']', '+', '(', ')', '=')
  //val textInput : List[Char] = List(':','.',',')
  val validTags: List[String] = List(LISTITEM,NEWLINE,LINKB,ADDRESSB,IMAGEB,DEFB,USEB,HEADING,BOLD)
  val beginTag = List(DOCB, TITLEB, HEADING, PARAB, LISTITEM, LINKB, IMAGEB, DEFB,
    USEB)



}
