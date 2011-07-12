%{
#include "t_lexer.h"
#include "ylcode.h"

#include "cdk/lex/yysupport.h"
%}

%option prefix="yy"
%option header-file="/tmp/yylex.h"
%option bison-locations
%option reentrant
%option tables-file="yyscanner.tables"
%option ecs full 8bit
%option stack
%option nounput noinput nounistd
%option nodefault
%option never-interactive

%x COMMENT

/* ------------------- definitions -------------------- */
/* Whitespace */
WS	[ \t\v\n\r\f]

/* newline */
NL            "\n"

/* anything but newline */
NOTNL         .

/* any of 256 source characters */
ANY           ({NOTNL}|{NL})

/* backslash */
BACKSL        "\\"

/* beginnging of line (must be start of a pattern) */
BOL           ^

/* end of line (would like EOF to qualify also, but flex doesn't allow it */
EOL           {NL}

/* letter or underscore */
LETTER        [A-Za-z_$]

/* letter or underscore or digit */
ALNUM         [A-Za-z_0-9$]

/* decimal digit */
DIGIT         [0-9]
HEXDIGIT      [0-9A-Fa-f]

/* sequence of decimal digits */
DIGITS        ({DIGIT}+)
/* sequence of hex digits */
HEXDIGITS     ({HEXDIGIT}+)

/* sign of a number */
SIGN          ("+"|"-")

/* integer suffix */
/* added 'LL' option for gcc/c99 long long compatibility */
ELL_SUFFIX    [lL]([lL]?)
INT_SUFFIX    ([uU]{ELL_SUFFIX}?|{ELL_SUFFIX}[uU]?)

/* floating-point suffix letter */
FLOAT_SUFFIX  [flFL]

/* normal string character: any but quote, newline, or backslash */
STRCHAR       [^\"\n\\]

/* (start of) an escape sequence */
ESCAPE        ({BACKSL}{ANY})

/* double quote */
QUOTE         [\"]

/* normal character literal character: any but single-quote, newline, or backslash */
CCCHAR        [^\'\n\\]

/* single quote */
TICK          [\']

/* space or tab */
SPTAB         [ \t]

/* preprocessor "character" -- any but escaped newline */
PPCHAR        ([^\\\n]|{BACKSL}{NOTNL})

%%
"asm"			{ Return (TK_ASM); }
"auto"			{ Return (TK_AUTO); }
"break"			{ Return (TK_BREAK); }
"bool"			{ Return (TK_BOOL); }
"case"			{ Return (TK_CASE); }
"catch"			{ Return (TK_CATCH); }
"cdecl"			{ Return (TK_CDECL); }
"char"			{ Return (TK_CHAR); }
"class"			{ Return (TK_CLASS); }
"const"			{ Return (TK_CONST); }
"const_cast"		{ Return (TK_CONST_CAST); }
"continue"		{ Return (TK_CONTINUE); }
"default"		{ Return (TK_DEFAULT); }
"delete"		{ Return (TK_DELETE); }
"do"			{ Return (TK_DO); }
"double"		{ Return (TK_DOUBLE); }
"dynamic_cast"		{ Return (TK_DYNAMIC_CAST); }
"else"			{ Return (TK_ELSE); }
"enum"			{ Return (TK_ENUM); }
"explicit"		{ Return (TK_EXPLICIT); }
"export"		{ Return (TK_EXPORT); }
"extern"		{ Return (TK_EXTERN); }
"false"			{ Return (TK_FALSE); }
"float"			{ Return (TK_FLOAT); }
"for"			{ Return (TK_FOR); }
"friend"		{ Return (TK_FRIEND); }
"goto"			{ Return (TK_GOTO); }
"if"			{ Return (TK_IF); }
"inline"		{ Return (TK_INLINE); }
"int"			{ Return (TK_INT); }
"long"			{ Return (TK_LONG); }
"mutable"		{ Return (TK_MUTABLE); }
"namespace"		{ Return (TK_NAMESPACE); }
"new"			{ Return (TK_NEW); }
"operator"		{ Return (TK_OPERATOR); }
"pascal"		{ Return (TK_PASCAL); }
"private"		{ Return (TK_PRIVATE); }
"protected"		{ Return (TK_PROTECTED); }
"public"		{ Return (TK_PUBLIC); }
"register"		{ Return (TK_REGISTER); }
"reinterpret_cast"	{ Return (TK_REINTERPRET_CAST); }
"return"		{ Return (TK_RETURN); }
"short"			{ Return (TK_SHORT); }
"signed"		{ Return (TK_SIGNED); }
"sizeof"		{ Return (TK_SIZEOF); }
"static"		{ Return (TK_STATIC); }
"static_cast"		{ Return (TK_STATIC_CAST); }
"struct"		{ Return (TK_STRUCT); }
"switch"		{ Return (TK_SWITCH); }
"template"		{ Return (TK_TEMPLATE); }
"this"			{ Return (TK_THIS); }
"throw"			{ Return (TK_THROW); }
"true"			{ Return (TK_TRUE); }
"try"			{ Return (TK_TRY); }
"typedef"		{ Return (TK_TYPEDEF); }
"typeid"		{ Return (TK_TYPEID); }
"typename"		{ Return (TK_TYPENAME); }
"union"			{ Return (TK_UNION); }
"unsigned"		{ Return (TK_UNSIGNED); }
"using"			{ Return (TK_USING); }
"virtual"		{ Return (TK_VIRTUAL); }
"void"			{ Return (TK_VOID); }
"volatile"		{ Return (TK_VOLATILE); }
"wchar_t"		{ Return (TK_WCHAR_T); }
"while"			{ Return (TK_WHILE); }

"("			{ Return (TK_LPAREN); }
")"			{ Return (TK_RPAREN); }
"["			{ Return (TK_LBRACKET); }
"]"			{ Return (TK_RBRACKET); }
"->"			{ Return (TK_ARROW); }
"::"			{ Return (TK_COLONCOLON); }
"."			{ Return (TK_DOT); }
"!"			{ Return (TK_BANG); }
"~"			{ Return (TK_TILDE); }
"+"			{ Return (TK_PLUS); }
"-"			{ Return (TK_MINUS); }
"++"			{ Return (TK_PLUSPLUS); }
"--"			{ Return (TK_MINUSMINUS); }
"&"			{ Return (TK_AND); }
"*"			{ Return (TK_STAR); }
".*"			{ Return (TK_DOTSTAR); }
"->*"			{ Return (TK_ARROWSTAR); }
"/"			{ Return (TK_SLASH); }
"%"			{ Return (TK_PERCENT); }
"<<"			{ Return (TK_LEFTSHIFT); }
">>"			{ Return (TK_RIGHTSHIFT); }
"<"			{ Return (TK_LESSTHAN); }
"<="			{ Return (TK_LESSEQ); }
">"			{ Return (TK_GREATERTHAN); }
">="			{ Return (TK_GREATEREQ); }
"=="			{ Return (TK_EQUALEQUAL); }
"!="			{ Return (TK_NOTEQUAL); }
"^"			{ Return (TK_XOR); }
"|"			{ Return (TK_OR); }
"&&"			{ Return (TK_ANDAND); }
"||"			{ Return (TK_OROR); }
"?"			{ Return (TK_QUESTION); }
":"			{ Return (TK_COLON); }
"="			{ Return (TK_EQUAL); }
"*="			{ Return (TK_STAREQUAL); }
"/="			{ Return (TK_SLASHEQUAL); }
"%="			{ Return (TK_PERCENTEQUAL); }
"+="			{ Return (TK_PLUSEQUAL); }
"-="			{ Return (TK_MINUSEQUAL); }
"&="			{ Return (TK_ANDEQUAL); }
"^="			{ Return (TK_XOREQUAL); }
"|="			{ Return (TK_OREQUAL); }
"<<="			{ Return (TK_LEFTSHIFTEQUAL); }
">>="			{ Return (TK_RIGHTSHIFTEQUAL); }
","			{ Return (TK_COMMA); }
"..."			{ Return (TK_ELLIPSIS); }
";"			{ Return (TK_SEMICOLON); }
"{"			{ Return (TK_LBRACE); }
"}"			{ Return (TK_RBRACE); }

  /* "alternative tokens" of cppstd 2.5p2 */
"<%"			{ Return (TK_LBRACE); }
"%>"			{ Return (TK_RBRACE); }
"<:"			{ Return (TK_LBRACKET); }
":>"			{ Return (TK_RBRACKET); }
  /* "%:" and "%:%:" correspond to "#" and "##", which are only for
   * the preprocessor, so I will ignore them here */
"and"			{ Return (TK_ANDAND); }
"bitor"			{ Return (TK_OR); }
"or"			{ Return (TK_OROR); }
"xor"			{ Return (TK_XOR); }
"compl"			{ Return (TK_TILDE); }
"bitand"		{ Return (TK_AND); }
"and_eq"		{ Return (TK_ANDEQUAL); }
"or_eq"			{ Return (TK_OREQUAL); }
"xor_eq"		{ Return (TK_XOREQUAL); }
"not"			{ Return (TK_BANG); }
"not_eq"		{ Return (TK_NOTEQUAL); }

	/* GNU Extensions */
"__extension__"		{ }

"__asm"			{ Return (TK_ASM); }
"__asm__"		{ Return (TK_ASM); }

"__const"		{ Return (TK_CONST); }
"__const__"		{ Return (TK_CONST); }
"__inline"		{ Return (TK_INLINE); }
"__inline__"		{ Return (TK_INLINE); }

"__volatile"		{ Return (TK_VOLATILE); }
"__volatile__"		{ Return (TK_VOLATILE); }

"__attribute"		{ Return (TK_ATTRIBUTE); }
"__attribute__"		{ Return (TK_ATTRIBUTE); }

"__signed__"		{ Return (TK_SIGNED); }

"decltype"		{ Return (TK_TYPEOF); }
"__decltype"		{ Return (TK_TYPEOF); }
"typeof"		{ Return (TK_TYPEOF); }
"__typeof"		{ Return (TK_TYPEOF); }
"__typeof__"		{ Return (TK_TYPEOF); }

"restrict"		{ Return (TK_RESTRICT); }
"__restrict"		{ Return (TK_RESTRICT); }
"__restrict__"		{ Return (TK_RESTRICT); }

"alignof"		{ Return (TK_ALIGNOF); }
"__alignof"		{ Return (TK_ALIGNOF); }
"__alignof__"		{ Return (TK_ALIGNOF); }

"_Complex"		{ Return (TK__COMPLEX); }
"__complex__"		{ Return (TK__COMPLEX); }

"_Imaginary"		{ Return (TK__IMAGINARY); }

"__real__"		{ Return (TK___REAL__); }
"__imag__"		{ Return (TK___IMAG__); }

"__func__"		{ Return (TK___FUNC__); }
"__FUNCTION__"		{ Return (TK___FUNCTION__); }
"__PRETTY_FUNCTION__"	{ Return (TK___PRETTY_FUNCTION__); }

"__builtin_constant_p"	{ Return (TK___BUILTIN_CONSTANT_P); }
"__builtin_offsetof"	{ Return (TK___BUILTIN_OFFSETOF); }
"__builtin_expect"	{ Return (TK___BUILTIN_EXPECT); }
"__builtin_va_arg"	{ Return (TK___BUILTIN_VA_ARG); }

"<?"                    { Return (TK_MIN_OP); }
">?"                    { Return (TK_MAX_OP); }


{LETTER}{ALNUM}* {
  Return (TK_NAME);
}

[1-9][0-9]*{INT_SUFFIX}?           |
[0][0-7]*{INT_SUFFIX}?             |
[0][xX][0-9A-Fa-f]+{INT_SUFFIX}?   {
  Return (TK_INT_LITERAL);
}

{DIGITS}"."{DIGITS}?([eE]{SIGN}?{DIGITS})?{FLOAT_SUFFIX}?   |
{DIGITS}"."?([eE]{SIGN}?{DIGITS})?{FLOAT_SUFFIX}?	    |
"."{DIGITS}([eE]{SIGN}?{DIGITS})?{FLOAT_SUFFIX}?	    {
  Return (TK_FLOAT_LITERAL);
}

[0][xX]{HEXDIGITS}"."{HEXDIGITS}?[pP]{SIGN}?{DIGITS}{FLOAT_SUFFIX}?   |
[0][xX]{HEXDIGITS}"."?[pP]{SIGN}?{DIGITS}{FLOAT_SUFFIX}?              |
[0][xX]"."{HEXDIGITS}[pP]{SIGN}?{DIGITS}{FLOAT_SUFFIX}?               {
  Return (TK_FLOAT_LITERAL);
}

"L"?{QUOTE}({STRCHAR}|{ESCAPE})*{QUOTE} {
  Return (TK_STRING_LITERAL);
}
  /* character literal */
"L"?{TICK}({CCCHAR}|{ESCAPE})*{TICK}   {
  Return (TK_CHAR_LITERAL);
}

{WS}+					{ }
"#".+					{ }
"//".*					{ }
<INITIAL,COMMENT>{
	"/*"				{ push_state (COMMENT); }
}
<COMMENT>{
	[^*/]+				{ }
	[*/]				{ }
	"*/"				{ pop_state (); }
}


<*>(.|\n)				{ printf (">>> in state %s\n", STRSTATE (state ())); yyerror (yylloc, 0, yytext); }
%%

int
yywrap (yyscan_t yyscanner)
{
  return yyget_extra (yyscanner)->wrap ();
}

int
t_lexer::state () const
{
  yyguts_t *yyg = (yyguts_t *)yyscanner;
  return YY_START;
}

void
t_lexer::push_state (int state)
{
  yy_push_state (state, yyscanner);
}

void
t_lexer::pop_state ()
{
  yy_pop_state (yyscanner);
}
