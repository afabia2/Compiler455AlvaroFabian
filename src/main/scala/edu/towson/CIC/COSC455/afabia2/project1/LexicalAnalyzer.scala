package edu.towson.CIC.COSC455.afabia2.project1

trait LexicalAnalyzer {

    def addChar() : Unit
    def getChar() : Unit
    def getNextToken() : Unit
    def lookup(C: String) : Boolean

}
