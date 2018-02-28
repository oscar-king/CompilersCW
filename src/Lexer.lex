import java_cup.runtime.*;

%%

%class Lexer
%unicode
%cup
%line
%column

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn, yytext());
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }
%}

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [\t\f]|" "

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}
TraditionalComment   = "/*" ~"*/" | "/#" "#/"

// Comment can be the last line of the file, without line terminator.
EndOfLineComment     = "#" {InputCharacter}* {LineTerminator}?
CommentContent       = ( [^*] | \# + [^/#] )*

//Base units
Letter = [a-zA-Z]
Digit = [0-9]
Punctuation = [!\"#\$%&\'()\*\+\,\-\.\/:;<=>\?@\[\]\\\^_`{¦}\~]
Character = '{Letter}' | '{Punctuation}' | '{Digit}'

Identifier = ({Letter})({Letter}|{Digit}|"_")*
Boolean = T|F

PosInteger = [1-9]({Digit}|[_])*{Digit}
Integer = [1-9]([0-9]|[_])*|0
Float = {Integer}"."{PosInteger}
Rat = {Integer}"/"{PosInteger}

%state STRING

%%

/* keywords */
<YYINITIAL> {


    //Primitive Data Types
    "char"               { return symbol(sym.CHAR); }
    "bool"               { return symbol(sym.BOOL); }
    "int"                { return symbol(sym.INT); }
    "rat"                { return symbol(sym.RAT); }
    "float"              { return symbol(sym.FLOAT); }

    //Aggregate Data Types
    "dict"               { return symbol(sym.DICT); }
    "seq"                { return symbol(sym.SEQ); }
    "set"                { return symbol(sym.SET); }

    //Other Data Types
    "thread"             { return symbol(sym.THREAD); }
    "top"                { return symbol(sym.TOP); }

    //Other Keywords
    "tdef"               { return symbol(sym.TDEF); }
    "fdef"               { return symbol(sym.FDEF); }
    "function"           { return symbol(sym.FUNC); }
    "return"             { return symbol(sym.RETURN); }
    "main"               { return symbol(sym.MAIN); }
    "alias"              { return symbol(sym.ALIAS); }

    //Control Flow Start
    "if"                 { return symbol(sym.IF); }
    "elif"               { return symbol(sym.ELIF); }
    "else"               { return symbol(sym.ELSE); }
    "while"              { return symbol(sym.WHILE); }
    "forall"             { return symbol(sym.FORALL); }
    "in"                 { return symbol(sym.IN); }
    "do"                 { return symbol(sym.DO); }
    "break"              { return symbol(sym.BREAK); }
    "then"               { return symbol(sym.THEN); }

    //Control Flow End
    "fi"                  { return symbol(sym.FI); }
    "od"                  { return symbol(sym.OD); }

    //IO
    "read"               { return symbol(sym.READ); }
    "print"              { return symbol(sym.PRINT); }

    //Assignment
    ":="                 { return symbol(sym.ASSIGN); }
    "->"                 { return symbol(sym.ARROW); }


    //Boolean
    "!"                   { return symbol(sym.NOT); }
    "&&"                  { return symbol(sym.AND); }
    "||"                  { return symbol(sym.OR); }
    "T"                   { return symbol(sym.TRUE); }
    "F"                   { return symbol(sym.FALSE); }

    //Set
    "&"                   { return symbol(sym.INTERSECTION); }
    "|"                   { return symbol(sym.PIPE); }
    "\\"                   { return symbol(sym.DIFFERENCE); }

    //Sequence
    "::"                  { return symbol(sym.CONCAT); }

    //Comparison
    "!="                  { return symbol(sym.NOTEQ); }
    "=="                  { return symbol(sym.EQ); }
    "<"                   { return symbol(sym.LT); }
    ">"                   { return symbol(sym.GT); }
    "<="                  { return symbol(sym.LTEQ); }
    ">="                  { return symbol(sym.GTEQ); }

    //Numeric
    "+"                            { return symbol(sym.PLUS); }
    "-"                            { return symbol(sym.MINUS); }
    "*"                            { return symbol(sym.MULT); }
    "/"                            { return symbol(sym.DIV); }
    "^"                            { return symbol(sym.EXPO); }

    //Other symbols
    "("                            { return symbol(sym.LPAREN); }
    ")"                            { return symbol(sym.RPAREN); }
    "["                            { return symbol(sym.LBRACK); }
    "]"                            { return symbol(sym.RBRACK); }
    "{"                            { return symbol(sym.LBRACE); }
    "}"                            { return symbol(sym.RBRACE); }
    ";"                            { return symbol(sym.SEMI); }
    ":"                            { return symbol(sym.COLON); }
    ","                            { return symbol(sym.COMMA); }
    "."                            { return symbol(sym.DOT); }

    /* literals */
    {Identifier}                   { return symbol(sym.IDENTIFIER, yytext()); }
    {Character}                    { return symbol(sym.CHAR_LIT, yytext()); }
    {Integer}                      { return symbol(sym.INT_LIT, Integer.parseInt(yytext())); }
    {Float}                        { return symbol(sym.FLOAT_LIT, Float.parseFloat(yytext())); }
    {Rat}                          { return symbol(sym.RAT_LIT, yytext()); }
    \"                             { string.setLength(0); yybegin(STRING); }
    {Comment}                      { /* ignore */ }
    {WhiteSpace}                   { /* ignore */ }
}

<STRING> {
  \"                             { yybegin(YYINITIAL); return symbol(sym.STRING_LIT, string.toString()); }
  [^\n\r\"\\\f]*                 { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }
  \\r                            { string.append('\r'); }
  \\f                            { string.append('\f'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
}

/* error fallback */
[^]                              { throw new Error("Illegal character <"+ yytext()+">"); }