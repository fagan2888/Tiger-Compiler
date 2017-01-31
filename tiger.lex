type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val nestedComments = ref 0
val buildString = ref ""
val stringStart = ref 0

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%s COMMENT STRING;
%%

<INITIAL>[ |\t]+ => (continue());
<INITIAL>\n         => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>type     => (Tokens.TYPE (yypos, yypos+4));
<INITIAL>var      => (Tokens.VAR (yypos, yypos+3));
<INITIAL>function => (Tokens.FUNCTION (yypos, yypos+8));
<INITIAL>break    => (Tokens.BREAK (yypos, yypos+5));
<INITIAL>of       => (Tokens.OF (yypos, yypos+2));
<INITIAL>end      => (Tokens.END (yypos, yypos+3));
<INITIAL>in       => (Tokens.IN (yypos, yypos+2));
<INITIAL>nil      => (Tokens.NIL (yypos, yypos+3));
<INITIAL>let      => (Tokens.LET (yypos, yypos+3));
<INITIAL>do       => (Tokens.DO (yypos, yypos+2));
<INITIAL>to       => (Tokens.TO (yypos, yypos+2));
<INITIAL>for      => (Tokens.FOR (yypos, yypos+3));
<INITIAL>while    => (Tokens.WHILE (yypos, yypos+5));
<INITIAL>else     => (Tokens.ELSE (yypos, yypos+4));
<INITIAL>then     => (Tokens.THEN (yypos, yypos+4));
<INITIAL>if       => (Tokens.IF (yypos, yypos+2));
<INITIAL>array    => (Tokens.ARRAY (yypos, yypos+5));
<INITIAL>":="     => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL>"|"      => (Tokens.OR (yypos, yypos+1));
<INITIAL>"&"      => (Tokens.AND (yypos, yypos+1));
<INITIAL>">="     => (Tokens.GE (yypos, yypos+2));
<INITIAL>">"      => (Tokens.GT (yypos, yypos+1));
<INITIAL>"<="     => (Tokens.LE (yypos, yypos+2));
<INITIAL>"<"      => (Tokens.LT (yypos, yypos+1));
<INITIAL>"<>"     => (Tokens.NEQ (yypos, yypos+2));
<INITIAL>"="      => (Tokens.EQ (yypos, yypos+1));
<INITIAL>"/"      => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL>"*"      => (Tokens.TIMES (yypos, yypos+1));
<INITIAL>"-"      => (Tokens.MINUS (yypos, yypos+1));
<INITIAL>"+"      => (Tokens.PLUS (yypos, yypos+1));
<INITIAL>"."      => (Tokens.DOT (yypos, yypos+1));
<INITIAL>"}"      => (Tokens.RBRACE (yypos, yypos+1));
<INITIAL>"{"      => (Tokens.LBRACE (yypos, yypos+1));
<INITIAL>"]"      => (Tokens.RBRACK (yypos, yypos+1));
<INITIAL>"["      => (Tokens.LBRACK (yypos, yypos+1));
<INITIAL>")"      => (Tokens.RPAREN (yypos, yypos+1));
<INITIAL>"("      => (Tokens.LPAREN (yypos, yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL>":"      => (Tokens.COLON (yypos, yypos+1));
<INITIAL>","      => (Tokens.COMMA (yypos, yypos+1));

<INITIAL>[0-9]+                => (Tokens.INT ((Option.valOf (Int.fromString yytext)), yypos, yypos+(String.size yytext)));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID (yytext, yypos, yypos+(String.size yytext)));

<INITIAL>"/*" => (nestedComments := 1; YYBEGIN COMMENT; continue());
<INITIAL>"*/" => (ErrorMsg.error yypos ("end comment with no start comment"); continue());
<COMMENT>"/*" => (nestedComments := !nestedComments+1; continue());
<COMMENT>"*/" => (nestedComments := !nestedComments-1; if (!nestedComments)=0 then YYBEGIN INITIAL else YYBEGIN COMMENT; continue());
<COMMENT>\n   => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>.    => (continue());

<INITIAL>\"               => (buildString := ""; stringStart := yypos; YYBEGIN STRING; continue());
<STRING>\"                => (YYBEGIN INITIAL; Tokens.STRING(!buildString, !stringStart, yypos+1));
<STRING>\n                => (buildString := (!buildString) ^ "\n"; lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>.                 => (buildString := (!buildString) ^ yytext; continue());
<STRING>\\[0-9][0-9][0-9] => (if (Option.valOf (Int.fromString (String.substring (yytext,1,(String.size yytext)-1))))>255 then (ErrorMsg.error yypos ("illegal ascii char " ^ yytext)) else (buildString := (!buildString) ^ (Char.toString (Char.chr (Option.valOf (Int.fromString (String.substring (yytext,1,(String.size yytext)-1))))))); continue());
<STRING>\\[abfnrtv\"\\]   => (buildString := (!buildString) ^ (String.str (Option.valOf (Char.fromString yytext))); continue());
<STRING>\\\^[@-_]         => (buildString := (!buildString) ^ (String.str (Option.valOf (Char.fromString yytext))); continue());
<STRING>\\.               => (ErrorMsg.error yypos ("illegal escape sequence " ^ yytext); continue());

. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
