/* The following code was generated by JFlex 1.6.1 */

import java_cup.runtime.*;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>src/Lexer.lex</tt>
 */
class Lexer implements java_cup.runtime.Scanner {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int STRING = 2;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1, 1
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\3\1\2\1\0\1\70\1\1\22\0\1\3\1\50\1\67"+
    "\1\6\2\11\1\51\1\12\1\57\1\60\1\5\1\55\1\66\1\46"+
    "\1\17\1\4\1\10\11\16\1\44\1\65\1\54\1\45\1\47\2\11"+
    "\5\7\1\15\15\7\1\14\6\7\1\61\1\53\1\62\1\56\1\13"+
    "\1\11\1\22\1\24\1\20\1\33\1\35\1\32\1\7\1\21\1\27"+
    "\1\7\1\43\1\26\1\41\1\30\1\25\1\37\1\36\1\23\1\34"+
    "\1\31\1\40\1\7\1\42\3\7\1\63\1\52\1\64\1\11\uff7e\0"+
    "\1\11\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\22\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\1\0\1\1\1\2\2\3\1\4\1\5\1\3\1\6"+
    "\2\2\1\7\1\10\1\2\1\11\16\6\1\12\1\2"+
    "\1\13\1\14\1\15\1\16\1\17\1\20\1\21\1\22"+
    "\1\23\1\24\1\25\1\26\1\27\1\30\1\31\1\32"+
    "\1\33\1\34\1\1\1\35\1\36\5\0\1\37\1\0"+
    "\6\6\1\40\1\41\1\42\5\6\1\43\2\6\1\44"+
    "\6\6\1\45\1\46\1\47\1\50\1\51\1\52\1\53"+
    "\1\54\1\55\1\56\1\57\1\60\1\61\1\62\4\0"+
    "\1\63\2\6\1\64\4\6\1\65\2\6\1\66\6\6"+
    "\1\67\1\70\5\6\1\71\1\72\1\73\1\6\1\74"+
    "\2\6\1\75\1\6\1\76\1\77\2\6\1\100\1\6"+
    "\1\101\1\102\1\103\1\6\1\104\1\6\1\105\1\6"+
    "\1\106\2\6\1\107\1\6\1\110\1\111\1\112\1\113"+
    "\1\114\2\6\1\115";

  private static int [] zzUnpackAction() {
    int [] result = new int[162];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\71\0\162\0\253\0\162\0\344\0\162\0\u011d"+
    "\0\u0156\0\u018f\0\u01c8\0\u0156\0\u0156\0\u0201\0\162\0\u023a"+
    "\0\u0273\0\u02ac\0\u02e5\0\u031e\0\u0357\0\u0390\0\u03c9\0\u0402"+
    "\0\u043b\0\u0474\0\u04ad\0\u04e6\0\u051f\0\u0558\0\u0591\0\u05ca"+
    "\0\u0603\0\u063c\0\u0675\0\u06ae\0\162\0\u06e7\0\162\0\162"+
    "\0\162\0\162\0\162\0\162\0\162\0\162\0\162\0\162"+
    "\0\162\0\u0720\0\u0759\0\162\0\u0792\0\u07cb\0\u0804\0\u083d"+
    "\0\u0876\0\u08af\0\u0201\0\u08e8\0\u0921\0\u095a\0\u0993\0\u09cc"+
    "\0\u0a05\0\u0156\0\u0a3e\0\u0156\0\u0a77\0\u0ab0\0\u0ae9\0\u0b22"+
    "\0\u0b5b\0\u0156\0\u0b94\0\u0bcd\0\u0156\0\u0c06\0\u0c3f\0\u0c78"+
    "\0\u0cb1\0\u0cea\0\u0d23\0\162\0\162\0\162\0\162\0\162"+
    "\0\162\0\162\0\162\0\162\0\162\0\162\0\162\0\162"+
    "\0\162\0\u0d5c\0\u0d95\0\u0dce\0\u0e07\0\162\0\u0e40\0\u0e79"+
    "\0\u0156\0\u0eb2\0\u0eeb\0\u0f24\0\u0f5d\0\u0156\0\u0f96\0\u0fcf"+
    "\0\u0156\0\u1008\0\u1041\0\u107a\0\u10b3\0\u10ec\0\u1125\0\u0156"+
    "\0\u0156\0\u115e\0\u1197\0\u11d0\0\u1209\0\u1242\0\u0dce\0\u0e07"+
    "\0\u0156\0\u127b\0\u0156\0\u12b4\0\u12ed\0\u0156\0\u1326\0\u0156"+
    "\0\u0156\0\u135f\0\u1398\0\u0156\0\u13d1\0\u0156\0\u0156\0\u0156"+
    "\0\u140a\0\u0156\0\u1443\0\u0156\0\u147c\0\u0156\0\u14b5\0\u14ee"+
    "\0\u0156\0\u1527\0\u0156\0\u0156\0\u0156\0\u0156\0\u0156\0\u1560"+
    "\0\u1599\0\u0156";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[162];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\3\1\4\2\5\1\6\1\7\1\10\1\11\1\12"+
    "\1\3\1\13\1\3\1\14\1\15\1\16\1\17\1\20"+
    "\1\11\1\21\1\22\1\23\1\24\1\11\1\25\1\11"+
    "\1\26\1\27\1\30\1\31\1\32\1\11\1\33\1\11"+
    "\1\34\1\35\1\11\1\36\1\37\1\40\1\41\1\42"+
    "\1\43\1\44\1\45\1\46\1\47\1\50\1\51\1\52"+
    "\1\53\1\54\1\55\1\56\1\57\1\60\1\61\1\5"+
    "\1\62\2\3\50\62\1\63\13\62\1\64\1\3\73\0"+
    "\1\5\73\0\1\65\1\66\62\0\1\10\1\4\1\5"+
    "\66\10\7\0\2\11\2\0\4\11\1\0\24\11\31\0"+
    "\1\67\12\0\1\70\55\0\46\71\1\0\15\71\11\0"+
    "\1\72\2\0\1\73\2\0\1\72\61\0\2\11\2\0"+
    "\4\11\1\0\1\11\1\74\22\11\34\0\2\11\2\0"+
    "\4\11\1\0\6\11\1\75\15\11\34\0\2\11\2\0"+
    "\4\11\1\0\2\11\1\76\12\11\1\77\6\11\34\0"+
    "\2\11\2\0\4\11\1\0\3\11\1\100\1\11\1\101"+
    "\16\11\34\0\2\11\2\0\4\11\1\0\13\11\1\102"+
    "\10\11\34\0\2\11\2\0\4\11\1\0\10\11\1\103"+
    "\1\11\1\104\11\11\34\0\2\11\2\0\4\11\1\0"+
    "\1\11\1\105\3\11\1\106\5\11\1\107\10\11\34\0"+
    "\2\11\2\0\4\11\1\0\5\11\1\110\1\111\1\112"+
    "\3\11\1\113\4\11\1\114\3\11\34\0\2\11\2\0"+
    "\4\11\1\0\5\11\1\115\1\11\1\116\14\11\34\0"+
    "\2\11\2\0\4\11\1\0\15\11\1\117\6\11\34\0"+
    "\2\11\2\0\4\11\1\0\6\11\1\120\15\11\34\0"+
    "\2\11\2\0\4\11\1\0\3\11\1\121\20\11\34\0"+
    "\2\11\2\0\4\11\1\0\2\11\1\122\21\11\34\0"+
    "\2\11\2\0\4\11\1\0\1\11\1\123\22\11\71\0"+
    "\1\124\1\125\70\0\1\126\72\0\1\127\66\0\1\130"+
    "\70\0\1\131\74\0\1\132\71\0\1\133\63\0\1\134"+
    "\23\0\1\62\2\0\50\62\1\0\13\62\25\0\1\135"+
    "\4\0\1\136\1\137\1\140\34\0\1\141\1\0\5\65"+
    "\1\142\63\65\6\0\1\143\100\0\1\144\70\0\1\145"+
    "\64\0\1\146\62\0\1\67\3\0\1\72\2\0\1\73"+
    "\2\0\1\72\1\70\60\0\2\11\2\0\4\11\1\0"+
    "\2\11\1\147\21\11\34\0\2\11\2\0\4\11\1\0"+
    "\7\11\1\150\14\11\34\0\2\11\2\0\4\11\1\0"+
    "\11\11\1\151\12\11\34\0\2\11\2\0\4\11\1\0"+
    "\2\11\1\152\6\11\1\153\12\11\34\0\2\11\2\0"+
    "\4\11\1\0\15\11\1\154\6\11\34\0\2\11\2\0"+
    "\4\11\1\0\5\11\1\155\16\11\34\0\2\11\2\0"+
    "\4\11\1\0\11\11\1\156\12\11\34\0\2\11\2\0"+
    "\4\11\1\0\3\11\1\157\11\11\1\160\6\11\34\0"+
    "\2\11\2\0\4\11\1\0\17\11\1\161\4\11\34\0"+
    "\2\11\2\0\4\11\1\0\15\11\1\162\6\11\34\0"+
    "\2\11\2\0\4\11\1\0\3\11\1\163\20\11\34\0"+
    "\2\11\2\0\4\11\1\0\5\11\1\164\16\11\34\0"+
    "\2\11\2\0\4\11\1\0\15\11\1\165\6\11\34\0"+
    "\2\11\2\0\4\11\1\0\10\11\1\166\13\11\34\0"+
    "\2\11\2\0\4\11\1\0\1\167\23\11\34\0\2\11"+
    "\2\0\4\11\1\0\11\11\1\170\4\11\1\171\5\11"+
    "\34\0\2\11\2\0\4\11\1\0\7\11\1\172\4\11"+
    "\1\173\7\11\34\0\2\11\2\0\4\11\1\0\7\11"+
    "\1\174\14\11\34\0\2\11\2\0\4\11\1\0\7\11"+
    "\1\175\14\11\34\0\2\11\2\0\4\11\1\0\7\11"+
    "\1\176\14\11\25\0\4\65\1\5\1\142\63\65\4\0"+
    "\1\5\74\0\1\177\2\0\1\144\2\0\1\177\62\0"+
    "\1\200\2\0\1\145\2\0\1\200\61\0\2\11\2\0"+
    "\4\11\1\0\3\11\1\201\20\11\34\0\2\11\2\0"+
    "\4\11\1\0\2\11\1\202\21\11\34\0\2\11\2\0"+
    "\4\11\1\0\13\11\1\203\10\11\34\0\2\11\2\0"+
    "\4\11\1\0\20\11\1\204\3\11\34\0\2\11\2\0"+
    "\4\11\1\0\2\11\1\205\21\11\34\0\2\11\2\0"+
    "\4\11\1\0\6\11\1\206\15\11\34\0\2\11\2\0"+
    "\4\11\1\0\15\11\1\207\6\11\34\0\2\11\2\0"+
    "\4\11\1\0\10\11\1\210\13\11\34\0\2\11\2\0"+
    "\4\11\1\0\12\11\1\211\11\11\34\0\2\11\2\0"+
    "\4\11\1\0\2\11\1\212\21\11\34\0\2\11\2\0"+
    "\4\11\1\0\2\11\1\213\21\11\34\0\2\11\2\0"+
    "\4\11\1\0\12\11\1\214\11\11\34\0\2\11\2\0"+
    "\4\11\1\0\1\215\23\11\34\0\2\11\2\0\4\11"+
    "\1\0\11\11\1\216\12\11\34\0\2\11\2\0\4\11"+
    "\1\0\12\11\1\217\11\11\34\0\2\11\2\0\4\11"+
    "\1\0\15\11\1\220\6\11\34\0\2\11\2\0\4\11"+
    "\1\0\10\11\1\221\13\11\34\0\2\11\2\0\4\11"+
    "\1\0\10\11\1\222\13\11\34\0\2\11\2\0\4\11"+
    "\1\0\6\11\1\223\15\11\34\0\2\11\2\0\4\11"+
    "\1\0\14\11\1\224\7\11\34\0\2\11\2\0\4\11"+
    "\1\0\3\11\1\225\20\11\34\0\2\11\2\0\4\11"+
    "\1\0\23\11\1\226\34\0\2\11\2\0\4\11\1\0"+
    "\2\11\1\227\21\11\34\0\2\11\2\0\4\11\1\0"+
    "\6\11\1\230\15\11\34\0\2\11\2\0\4\11\1\0"+
    "\11\11\1\231\12\11\34\0\2\11\2\0\4\11\1\0"+
    "\11\11\1\232\12\11\34\0\2\11\2\0\4\11\1\0"+
    "\11\11\1\233\12\11\34\0\2\11\2\0\4\11\1\0"+
    "\15\11\1\234\6\11\34\0\2\11\2\0\4\11\1\0"+
    "\10\11\1\235\13\11\34\0\2\11\2\0\4\11\1\0"+
    "\13\11\1\236\10\11\34\0\2\11\2\0\4\11\1\0"+
    "\6\11\1\237\15\11\34\0\2\11\2\0\4\11\1\0"+
    "\7\11\1\240\14\11\34\0\2\11\2\0\4\11\1\0"+
    "\5\11\1\241\16\11\34\0\2\11\2\0\4\11\1\0"+
    "\10\11\1\242\13\11\25\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[5586];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\1\0\1\1\1\11\1\1\1\11\1\1\1\11\7\1"+
    "\1\11\25\1\1\11\1\1\13\11\2\1\1\11\5\0"+
    "\1\1\1\0\30\1\16\11\4\0\1\11\74\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[162];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn, yytext());
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  Lexer(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 176) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Contains user EOF-code, which will be executed exactly once,
   * when the end of file is reached
   */
  private void zzDoEOF() throws java.io.IOException {
    if (!zzEOFDone) {
      zzEOFDone = true;
      yyclose();
    }
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public java_cup.runtime.Symbol next_token() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn += zzCharCount;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
            zzDoEOF();
          { return new java_cup.runtime.Symbol(sym.EOF); }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { string.append( yytext() );
            }
          case 78: break;
          case 2: 
            { throw new Error("Illegal character <"+ yytext()+">");
            }
          case 79: break;
          case 3: 
            { /* ignore */
            }
          case 80: break;
          case 4: 
            { return symbol(sym.DIV);
            }
          case 81: break;
          case 5: 
            { return symbol(sym.MULT);
            }
          case 82: break;
          case 6: 
            { return symbol(sym.IDENTIFIER, yytext());
            }
          case 83: break;
          case 7: 
            { return symbol(sym.TRUE);
            }
          case 84: break;
          case 8: 
            { return symbol(sym.FALSE);
            }
          case 85: break;
          case 9: 
            { return symbol(sym.DOT);
            }
          case 86: break;
          case 10: 
            { return symbol(sym.COLON);
            }
          case 87: break;
          case 11: 
            { return symbol(sym.MINUS);
            }
          case 88: break;
          case 12: 
            { return symbol(sym.GT);
            }
          case 89: break;
          case 13: 
            { return symbol(sym.NOT);
            }
          case 90: break;
          case 14: 
            { return symbol(sym.INTERSECTION);
            }
          case 91: break;
          case 15: 
            { return symbol(sym.PIPE);
            }
          case 92: break;
          case 16: 
            { return symbol(sym.DIFFERENCE);
            }
          case 93: break;
          case 17: 
            { return symbol(sym.LT);
            }
          case 94: break;
          case 18: 
            { return symbol(sym.PLUS);
            }
          case 95: break;
          case 19: 
            { return symbol(sym.EXPO);
            }
          case 96: break;
          case 20: 
            { return symbol(sym.LPAREN);
            }
          case 97: break;
          case 21: 
            { return symbol(sym.RPAREN);
            }
          case 98: break;
          case 22: 
            { return symbol(sym.LBRACK);
            }
          case 99: break;
          case 23: 
            { return symbol(sym.RBRACK);
            }
          case 100: break;
          case 24: 
            { return symbol(sym.LBRACE);
            }
          case 101: break;
          case 25: 
            { return symbol(sym.RBRACE);
            }
          case 102: break;
          case 26: 
            { return symbol(sym.SEMI);
            }
          case 103: break;
          case 27: 
            { return symbol(sym.COMMA);
            }
          case 104: break;
          case 28: 
            { string.setLength(0); yybegin(STRING);
            }
          case 105: break;
          case 29: 
            { string.append('\\');
            }
          case 106: break;
          case 30: 
            { yybegin(YYINITIAL); return symbol(sym.STRING_LIT, string.toString());
            }
          case 107: break;
          case 31: 
            { return symbol(sym.INT_LIT, Integer.parseInt(yytext()));
            }
          case 108: break;
          case 32: 
            { return symbol(sym.OD);
            }
          case 109: break;
          case 33: 
            { return symbol(sym.IN);
            }
          case 110: break;
          case 34: 
            { return symbol(sym.IF);
            }
          case 111: break;
          case 35: 
            { return symbol(sym.FI);
            }
          case 112: break;
          case 36: 
            { return symbol(sym.DO);
            }
          case 113: break;
          case 37: 
            { return symbol(sym.CONCAT);
            }
          case 114: break;
          case 38: 
            { return symbol(sym.ASSIGN);
            }
          case 115: break;
          case 39: 
            { return symbol(sym.EQ);
            }
          case 116: break;
          case 40: 
            { return symbol(sym.ARROW);
            }
          case 117: break;
          case 41: 
            { return symbol(sym.GTEQ);
            }
          case 118: break;
          case 42: 
            { return symbol(sym.NOTEQ);
            }
          case 119: break;
          case 43: 
            { return symbol(sym.AND);
            }
          case 120: break;
          case 44: 
            { return symbol(sym.OR);
            }
          case 121: break;
          case 45: 
            { return symbol(sym.LTEQ);
            }
          case 122: break;
          case 46: 
            { string.append('\r');
            }
          case 123: break;
          case 47: 
            { string.append('\n');
            }
          case 124: break;
          case 48: 
            { string.append('\t');
            }
          case 125: break;
          case 49: 
            { string.append('\f');
            }
          case 126: break;
          case 50: 
            { string.append('\"');
            }
          case 127: break;
          case 51: 
            { return symbol(sym.CHAR_LIT, yytext());
            }
          case 128: break;
          case 52: 
            { return symbol(sym.RAT);
            }
          case 129: break;
          case 53: 
            { return symbol(sym.INT);
            }
          case 130: break;
          case 54: 
            { return symbol(sym.TOP);
            }
          case 131: break;
          case 55: 
            { return symbol(sym.SET);
            }
          case 132: break;
          case 56: 
            { return symbol(sym.SEQ);
            }
          case 133: break;
          case 57: 
            { return symbol(sym.RAT_LIT, yytext());
            }
          case 134: break;
          case 58: 
            { return symbol(sym.FLOAT_LIT, Float.parseFloat(yytext()));
            }
          case 135: break;
          case 59: 
            { return symbol(sym.CHAR);
            }
          case 136: break;
          case 60: 
            { return symbol(sym.READ);
            }
          case 137: break;
          case 61: 
            { return symbol(sym.BOOL);
            }
          case 138: break;
          case 62: 
            { return symbol(sym.THEN);
            }
          case 139: break;
          case 63: 
            { return symbol(sym.TDEF);
            }
          case 140: break;
          case 64: 
            { return symbol(sym.FDEF);
            }
          case 141: break;
          case 65: 
            { return symbol(sym.DICT);
            }
          case 142: break;
          case 66: 
            { return symbol(sym.ELIF);
            }
          case 143: break;
          case 67: 
            { return symbol(sym.ELSE);
            }
          case 144: break;
          case 68: 
            { return symbol(sym.MAIN);
            }
          case 145: break;
          case 69: 
            { return symbol(sym.ALIAS);
            }
          case 146: break;
          case 70: 
            { return symbol(sym.BREAK);
            }
          case 147: break;
          case 71: 
            { return symbol(sym.FLOAT);
            }
          case 148: break;
          case 72: 
            { return symbol(sym.PRINT);
            }
          case 149: break;
          case 73: 
            { return symbol(sym.WHILE);
            }
          case 150: break;
          case 74: 
            { return symbol(sym.RETURN);
            }
          case 151: break;
          case 75: 
            { return symbol(sym.THREAD);
            }
          case 152: break;
          case 76: 
            { return symbol(sym.FORALL);
            }
          case 153: break;
          case 77: 
            { return symbol(sym.FUNC);
            }
          case 154: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
