%{
#include "t_parser.h"
#include "ylcode.h"
#define YYSTYPE YYSTYPE
#define YYMAXDEPTH 1000000
%}

%union {
  cdk::ast::node *any;
  cdk::ast::generic_node *node;
  cdk::ast::generic_token *token;
}

%{
#include "yystate.h"

typedef t_parser parser;
#include "cdk/parse/yysupport.h"

YYSTYPE merge (YYSTYPE const &x0, YYSTYPE const &x1);
%}

%define api.pure
%locations
%debug
%error-verbose
%token-table
%glr-parser

%parse-param	{ parser *self }
%lex-param	{ parser *self }

%token TK_EOF 0				"end of file"

%token<token> TK_NAME			"identifier"
%token<token> TK_INT_LITERAL		"integer literal"
%token<token> TK_CHAR_LITERAL		"character literal"
%token<token> TK_FLOAT_LITERAL		"floating point literal"
%token<token> TK_STRING_LITERAL		"string literal"

%token TK_ASM			"asm"
%token TK_AUTO			"auto"
%token TK_BREAK			"break"
%token TK_BOOL			"bool"
%token TK_CASE			"case"
%token TK_CATCH			"catch"
%token TK_CDECL			"cdecl"
%token TK_CHAR			"char"
%token TK_CLASS			"class"
%token TK_CONST			"const"
%token TK_CONST_CAST		"const_cast"
%token TK_CONTINUE		"continue"
%token TK_DEFAULT		"default"
%token TK_DELETE		"delete"
%token TK_DO			"do"
%token TK_DOUBLE		"double"
%token TK_DYNAMIC_CAST		"dynamic_cast"
%token TK_ELSE			"else"
%token TK_ENUM			"enum"
%token TK_EXPLICIT		"explicit"
%token TK_EXPORT		"export"
%token TK_EXTERN		"extern"
%token TK_FALSE			"false"
%token TK_FLOAT			"float"
%token TK_FOR			"for"
%token TK_FRIEND		"friend"
%token TK_GOTO			"goto"
%token TK_IF			"if"
%token TK_INLINE		"inline"
%token TK_INT			"int"
%token TK_LONG			"long"
%token TK_MUTABLE		"mutable"
%token TK_NAMESPACE		"namespace"
%token TK_NEW			"new"
%token TK_OPERATOR		"operator"
%token TK_PASCAL		"pascal"
%token TK_PRIVATE		"private"
%token TK_PROTECTED		"protected"
%token TK_PUBLIC		"public"
%token TK_REGISTER		"register"
%token TK_REINTERPRET_CAST	"reinterpret_cast"
%token TK_RETURN		"return"
%token TK_SHORT			"short"
%token TK_SIGNED		"signed"
%token TK_SIZEOF		"sizeof"
%token TK_STATIC		"static"
%token TK_STATIC_CAST		"static_cast"
%token TK_STRUCT		"struct"
%token TK_SWITCH		"switch"
%token TK_TEMPLATE		"template"
%token TK_THIS			"this"
%token TK_THROW			"throw"
%token TK_TRUE			"true"
%token TK_TRY			"try"
%token TK_TYPEDEF		"typedef"
%token TK_TYPEID		"typeid"
%token TK_TYPENAME		"typename"
%token TK_UNION			"union"
%token TK_UNSIGNED		"unsigned"
%token TK_USING			"using"
%token TK_VIRTUAL		"virtual"
%token TK_VOID			"void"
%token TK_VOLATILE		"volatile"
%token TK_WCHAR_T		"wchar_t"
%token TK_WHILE			"while"

%token TK_LPAREN		"("
%token TK_RPAREN		")"
%token TK_LBRACKET		"["
%token TK_RBRACKET		"]"
%token TK_ARROW			"->"
%token TK_COLONCOLON		"::"
%token TK_DOT			"."
%token TK_BANG			"!"
%token TK_TILDE			"~"
%token TK_PLUS			"+"
%token TK_MINUS			"-"
%token TK_PLUSPLUS		"++"
%token TK_MINUSMINUS		"--"
%token TK_AND			"&"
%token TK_STAR			"*"
%token TK_DOTSTAR		".*"
%token TK_ARROWSTAR		"->*"
%token TK_SLASH			"/"
%token TK_PERCENT		"%"
%token TK_LEFTSHIFT		"<<"
%token TK_RIGHTSHIFT		">>"
%token TK_LESSTHAN		"<"
%token TK_LESSEQ		"<="
%token TK_GREATERTHAN		">"
%token TK_GREATEREQ		">="
%token TK_EQUALEQUAL		"=="
%token TK_NOTEQUAL		"!="
%token TK_XOR			"^"
%token TK_OR			"|"
%token TK_ANDAND		"&&"
%token TK_OROR			"||"
%token TK_QUESTION		"?"
%token TK_COLON			":"
%token TK_EQUAL			"="
%token TK_STAREQUAL		"*="
%token TK_SLASHEQUAL		"/="
%token TK_PERCENTEQUAL		"%="
%token TK_PLUSEQUAL		"+="
%token TK_MINUSEQUAL		"-="
%token TK_ANDEQUAL		"&="
%token TK_XOREQUAL		"^="
%token TK_OREQUAL		"|="
%token TK_LEFTSHIFTEQUAL	"<<="
%token TK_RIGHTSHIFTEQUAL	">>="
%token TK_COMMA			","
%token TK_ELLIPSIS		"..."
%token TK_SEMICOLON		";"
%token TK_LBRACE		"{"
%token TK_RBRACE		"}"

/* GNU Extensions */
%token TK_ATTRIBUTE		"__attribute__"
%token TK_RESTRICT		"restrict"
%token TK_TYPEOF		"typeof"
%token TK_ALIGNOF		"alignof"
%token TK___FUNC__		"__func__"
%token TK___FUNCTION__		"__FUNCTION__"
%token TK___PRETTY_FUNCTION__	"__PRETTY_FUNCTION__"
%token TK__COMPLEX		"_Complex"
%token TK__IMAGINARY		"_Imaginary"
%token TK___REAL__		"__real__"
%token TK___IMAG__		"__imag__"
%token TK___BUILTIN_CONSTANT_P	"__builtin_constant_p"
%token TK___BUILTIN_OFFSETOF	"__builtin_offsetof"
%token TK___BUILTIN_EXPECT	"__builtin_expect"
%token TK___BUILTIN_VA_ARG	"__builtin_va_arg"

%token TK_MIN_OP		"<?"
%token TK_MAX_OP		">?"

%token R_DOCUMENT

%nonassoc TOK_PREFER_REDUCE
%right "::"
%nonassoc "const" "volatile" "else" "[" "__attribute__" "restrict"

%left ".*" "->*"
%left "*" "/" "%"
%left "+" "-"
%left "<<" ">>"

%left "==" "!="
%left "&"
%left "^"
%left "|"
%left "&&"
%left "||"
    
%nonassoc TOK_PREFER_SHIFT

%destructor { delete $$; } <*>

%%
/****************************************************************************
 *
 *	Toplevel declarations
 *
 ****************************************************************************/
document
	: TranslationUnit
	;

Identifier
	: TK_NAME
	;

TranslationUnit
	: empty
	| TranslationUnit Declaration %dprec 1
	| TranslationUnit ";" %dprec 2
	;

PrimaryExpression
	: Literal
	| "this"
	| "(" Expression ")" %dprec 1
	| IdExpression
	;

Literal
	: TK_INT_LITERAL
	| TK_FLOAT_LITERAL
	| StringLiteral
	| TK_CHAR_LITERAL
	| "true"
	| "false"
	;

PreprocString
	: TK_STRING_LITERAL
	;

StringLiteral
	: PreprocString
	| PreprocString StringLiteral
	;

IdExpression
	: PQualifiedId
	| "::" PQualifiedId
	;

UnqualifiedId
	: Identifier
	| OperatorFunctionId
	| ConversionFunctionId
	| TemplateId
	;

PQualifiedId
	: UnqualifiedId %prec "::" %merge<merge>
	| Identifier "::" PQualifiedId
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQualifiedId %merge<merge>
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQualifiedId %merge<merge>
	;

ArgumentList
	: "(" ExpressionListOpt ")"
	;

PostfixExpression
	: PrimaryExpression
	| PostfixExpression "[" Expression "]"
	| PostfixExpression ArgumentList %merge<merge>
	| "typename" IdExpression ArgumentList
	| CtorExpressionType ArgumentList %merge<merge>
	| PostfixExpression "." NameAfterDot
	| PostfixExpression "->" NameAfterDot
	| PostfixExpression "++"
	| PostfixExpression "--"
	| CastKeyword "<" TypeId ">" "(" Expression ")"
	| "typeid" "(" Expression ")" %merge<merge>
	| "typeid" "(" TypeId ")" %merge<merge>
	;

CtorExpressionType
	: PQTypeName
	| "char"
	| "wchar_t"
	| "bool"
	| "short"
	| "int"
	| "long"
	| "signed"
	| "unsigned"
	| "float"
	| "double"
	| "void"
	;

CastKeyword
	: "dynamic_cast"
	| "static_cast"
	| "reinterpret_cast"
	| "const_cast"
	;

ExpressionList
	: AssignmentExpression %merge<merge>
	| AssignmentExpression "," ExpressionList %merge<merge>
	;

ExpressionListOpt
	: empty
	| ExpressionList
	;

UnaryExpression
	: PostfixExpression
	| "++" CastExpression
	| "--" CastExpression
	| DeleteExpression
	| "*" CastExpression
	| "&" CastExpression
	| "+" CastExpression
	| "-" CastExpression
	| "!" CastExpression
	| "~" CastExpression
	| "sizeof" UnaryExpression %merge<merge>
	| "sizeof" "(" TypeId ")" %merge<merge>
	| NewExpression
	;

ColonColonOpt
	: empty
	| "::"
	;

NewExpression
	: ColonColonOpt "new" NewPlacementOpt NewTypeId NewInitializerOpt %merge<merge>
	| ColonColonOpt "new" NewPlacementOpt "(" TypeId ")" NewInitializerOpt %merge<merge>
	;

NewPlacementOpt
	: empty
	| "(" ExpressionList ")"
	;

NewTypeId
	: TypeSpecifier NewDeclaratorOpt
	;

NewDeclaratorOpt
	: empty
	| "*" CVQualifierSeqOpt NewDeclaratorOpt
	| PtrToMemberName "*" CVQualifierSeqOpt NewDeclaratorOpt
	| DirectNewDeclarator
	;

DirectNewDeclarator
	: "[" Expression "]"
	| DirectNewDeclarator "[" ConstantExpression "]"
	;

NewInitializerOpt
	: empty
	| "(" ExpressionListOpt ")"
	;

DeleteExpression
	: ColonColonOpt "delete" CastExpression
	| ColonColonOpt "delete" "[" "]" CastExpression
	;

NameAfterDot
	: NAD1
	| "::" NAD2
	;

NAD1
	: NAD2
	| "template" Identifier "<" TemplateArgumentListOpt ">"
	| "~" Identifier
	| "~" Identifier "<" TemplateArgumentListOpt ">"
	| ConversionFunctionId
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" NAD1
	;

NAD2
	: Identifier "<" TemplateArgumentListOpt ">"
	| Identifier
	| OperatorFunctionId
	| OperatorFunctionId "<" TemplateArgumentListOpt ">"
	| "template" OperatorFunctionId "<" TemplateArgumentListOpt ">"
	| Identifier "<" TemplateArgumentListOpt ">" "::" NAD1
	| Identifier "::" NAD1
	;

CastExpression
	: UnaryExpression %merge<merge>
	| "(" TypeId ")" CastExpression %merge<merge>
	;

BinExp_high
	: CastExpression %merge<merge>
	| BinExp_high ".*" BinExp_high
	| BinExp_high "->*" BinExp_high
	| BinExp_high "*" BinExp_high %merge<merge>
	| BinExp_high "/" BinExp_high
	| BinExp_high "%" BinExp_high
	| BinExp_high "+" BinExp_high %merge<merge>
	| BinExp_high "-" BinExp_high %merge<merge>
	| BinExp_high "<<" BinExp_high
	| BinExp_high ">>" BinExp_high
	;

BinExp_mid
	: BinExp_high %merge<merge>
	| BinExp_mid "<" BinExp_high %merge<merge>
	| BinExp_mid ">" BinExp_high %merge<merge>
	| BinExp_mid "<=" BinExp_high
	| BinExp_mid ">=" BinExp_high
	;

BinaryExpression
	: BinExp_mid %merge<merge>
	| BinaryExpression "==" BinaryExpression %merge<merge>
	| BinaryExpression "!=" BinaryExpression %merge<merge>
	| BinaryExpression "&" BinaryExpression %merge<merge>
	| BinaryExpression "^" BinaryExpression %merge<merge>
	| BinaryExpression "|" BinaryExpression %merge<merge>
	| BinaryExpression "&&" BinaryExpression %merge<merge>
	| BinaryExpression "||" BinaryExpression %merge<merge>
	;

ConditionalExpression
	: BinaryExpression %merge<merge>
	| BinaryExpression "?" Expression ":" AssignmentExpression %merge<merge>
	;

AssignmentExpression
	: ConditionalExpression %merge<merge>
	| BinaryExpression AssignmentOperator AssignmentExpression %merge<merge>
	| ThrowExpression
	;

AssignmentOperator
	: "*="
	| "/="
	| "%="
	| "+="
	| "-="
	| ">>="
	| "<<="
	| "&="
	| "^="
	| "|="
	| "="
	;

Expression
	: AssignmentExpression %merge<merge>
	| Expression "," AssignmentExpression %merge<merge>
	;

ExpressionOpt
	: empty
	| Expression
	;

ConstantExpression
	: AssignmentExpression
	;

ConstantExpressionOpt
	: empty
	| ConstantExpression
	;

LabelAndColon
	: Identifier ":" %prec TK_PREFER_SHIFT
	;

Statement
	: LabelAndColon Statement
	| "case" ConstantExpression ":" Statement
	| "default" ":" Statement
	| ExpressionStatement %merge<merge>
	| CompoundStatement
	| "if" "(" Condition ")" Statement %prec TK_PREFER_SHIFT
	| "if" "(" Condition ")" Statement "else" Statement
	| "switch" "(" Condition ")" Statement
	| "while" "(" Condition ")" Statement
	| "do" Statement "while" "(" Expression ")" ";"
	| "for" "(" ForInitStatement ConditionOpt ";" ExpressionOpt ")" Statement
	| "break" ";"
	| "continue" ";"
	| "return" Expression ";"
	| "return" ";"
	| "goto" Identifier ";"
	| BlockDeclaration %merge<merge>
	| TryBlock
	| AsmDefinition
	| NamespaceDecl
	;

ExpressionStatement
	: ";"
	| Expression ";"
	;

CompoundStatement
	: CompoundStmtHelper "}"
	;

CompoundStmtHelper
	: "{" empty
	| CompoundStmtHelper Statement
	;

Condition
	: Expression %merge<merge>
	| TypeSpecifier Declarator "=" AssignmentExpression %merge<merge>
	;

ConditionOpt
	: empty
	| Condition
	;

ForInitStatement
	: ExpressionStatement %merge<merge>
	| SimpleDeclaration %merge<merge>
	;

Declaration
	: BlockDeclaration
	| FunctionDefinition
	| TemplateDeclaration
	| ExplicitInstantiation
	| LinkageSpecification
	| AsmDefinition
	| NamespaceDefinition
	| NamespaceDecl
	;

BlockDeclaration
	: SimpleDeclaration
	;

SimpleDeclaration
	: DeclSpecifier InitDeclaratorListOpt ";" %merge<merge>
	;

DeclSpecifier
	: PQTypeName UberModifierSeqOpt
	| UberModifierSeq PQTypeName UberModifierSeqOpt
	| UberTypeKeyword UberTypeAndModifierSeqOpt
	| UberModifierSeq UberTypeKeyword UberTypeAndModifierSeqOpt
	| ElaboratedOrSpecifier UberModifierSeqOpt
	| UberModifierSeq ElaboratedOrSpecifier UberModifierSeqOpt
	;

ElaboratedOrSpecifier
	: ElaboratedTypeSpecifier
	| ClassSpecifier
	| EnumSpecifier
	;

UberModifierSeq
	: UberModifier
	| UberModifierSeq UberModifier
	;

UberModifierSeqOpt
	: empty
	| UberModifierSeq
	;

UberTypeAndModifierSeqOpt
	: empty
	| UberTypeAndModifierSeqOpt UberModifier
	| UberTypeAndModifierSeqOpt UberTypeKeyword
	;

UberCVQualifierSeq
	: UberCVQualifier
	| UberCVQualifierSeq UberCVQualifier
	;

UberCVQualifierSeqOpt
	: empty
	| UberCVQualifierSeq
	;

UberTypeAndCVQualifierSeqOpt
	: empty
	| UberTypeAndCVQualifierSeqOpt UberCVQualifier
	| UberTypeAndCVQualifierSeqOpt UberTypeKeyword
	;

UberModifier
	: "auto"
	| "register"
	| "static"
	| "extern"
	| "mutable"
	| "inline"
	| "virtual"
	| "friend"
	| "typedef"
	| "const"
	| "volatile"
	;

UberCVQualifier
	: "const"
	| "volatile"
	;

UberTypeKeyword
	: "char"
	| "wchar_t"
	| "bool"
	| "short"
	| "int"
	| "long"
	| "signed"
	| "unsigned"
	| "float"
	| "double"
	| "void"
	;

ElaboratedTypeSpecifier
	: ClassKey PQTypeName
	| "enum" PQTypeName
	| "typename" PQTypeName
	;

TypeSpecifier
	: PQTypeName UberCVQualifierSeqOpt
	| UberCVQualifierSeq PQTypeName UberCVQualifierSeqOpt
	| UberTypeKeyword UberTypeAndCVQualifierSeqOpt
	| UberCVQualifierSeq UberTypeKeyword UberTypeAndCVQualifierSeqOpt
	| ElaboratedOrSpecifier UberCVQualifierSeqOpt
	| UberCVQualifierSeq ElaboratedOrSpecifier UberCVQualifierSeqOpt
	;

PQTypeName
	: PQTypeName_ncc
	| "::" PQTypeName_ncc
	;

PQTypeName_ncc
	: Identifier %prec "::"
	| TemplateId %prec "::" %merge<merge>
	| Identifier "::" PQTypeName_notfirst
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQTypeName_notfirst %merge<merge>
	;

PQTypeName_notfirst
	: PQTypeName_ncc %prec "::"
	| "template" TemplateId %prec "::"
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQTypeName_notfirst
	;

EnumSpecifier
	: "enum" "{" EnumeratorListOpt "}"
	| "enum" Identifier "{" EnumeratorListOpt "}"
	;

EnumeratorListOpt
	: empty
	| EnumeratorDefinition
	| EnumeratorDefinition "," EnumeratorListOpt
	;

EnumeratorDefinition
	: Identifier
	| Identifier "=" ConstantExpression
	;

AsmDefinition
	: "asm" "(" StringLiteral ")" ";"
	;

LinkageSpecification
	: "extern" TK_STRING_LITERAL "{" TranslationUnit "}"
	| "extern" TK_STRING_LITERAL Declaration
	;

InitDeclaratorListOpt
	: empty
	| InitDeclaratorList
	;

InitDeclaratorList
	: InitDeclarator %merge<merge>
	| InitDeclarator "," InitDeclaratorList %merge<merge>
	;

InitDeclarator
	: Declarator %merge<merge>
	| Declarator Initializer %merge<merge>
	;

Initializer
	: "=" SimpleInitializerClause
	| "(" ExpressionList ")"
	;

SimpleInitializerClause
	: AssignmentExpression
	;

InitializerClause
	: SimpleInitializerClause
	;

CompoundInitializer
	: "{" InitializerList CommaOpt "}"
	| "{" "}"
	;

CommaOpt
	: empty
	| ","
	;

InitializerList
	: InitializerClause %merge<merge>
	| InitializerList "," InitializerClause %merge<merge>
	;

Declarator
	: "*" CVQualifierSeqOpt Declarator
	| "&" CVQualifierSeqOpt Declarator
	| PtrToMemberName "*" CVQualifierSeqOpt Declarator
	| DirectDeclarator
	;

DirectDeclarator
	: IdExpression %merge<merge>
	| PQDtorName
	| DirectDeclarator
	  "(" ParameterDeclarationClause ")"
	  CVQualifierSeqOpt
	  ExceptionSpecificationOpt
	| DirectDeclarator "[" ConstantExpressionOpt "]" %merge<merge>
	| "(" Declarator ")"
	;

PQDtorName
	: "~" Identifier
	| "~" Identifier "<" TemplateArgumentListOpt ">"
	| Identifier "::" PQDtorName
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQDtorName
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQDtorName
	;

PtrToMemberName
	: IdExpression "::"
	;

CVQualifierSeqOpt
	: empty
	| CVQualifierSeq
	;

CVQualifierSeq
	: CVQualifier
	| CVQualifier CVQualifierSeq
	;

CVQualifier
	: "const"
	| "volatile"
	;

TypeId
	: TypeSpecifier AbstractDeclaratorOpt
	;

AbstractDeclaratorOpt
	: empty
	| AbstractDeclarator
	;

AbstractDeclarator
	: "*" CVQualifierSeqOpt AbstractDeclaratorOpt
	| "&" CVQualifierSeqOpt AbstractDeclaratorOpt
	| PtrToMemberName "*" CVQualifierSeqOpt AbstractDeclaratorOpt
	| DirectAbstractDeclarator
	;

DirectAbstractDeclaratorOpt
	: empty
	| DirectAbstractDeclarator
	;

DirectAbstractDeclarator
	: DirectAbstractDeclaratorOpt
	  "(" ParameterDeclarationClause ")"
	  CVQualifierSeqOpt
	  ExceptionSpecificationOpt
	| DirectAbstractDeclaratorOpt "[" ConstantExpressionOpt "]"
	| "(" AbstractDeclarator ")"
	;

ParameterDeclarationClause
	: ParameterDeclarationList
	| empty
	;

ParameterDeclarationList
	: "..."
	| ParameterDeclaration "..."
	| ParameterDeclaration %merge<merge>
	| ParameterDeclaration "," ParameterDeclarationList %merge<merge>
	;

ParameterDeclaration
	: TypeSpecifier ParameterDeclarator %merge<merge>
	| "register" TypeSpecifier ParameterDeclarator %merge<merge>
	| TypeSpecifier "register" ParameterDeclarator %merge<merge>
	;

ParameterDeclarator
	: UnqualifiedDeclarator %merge<merge>
	| UnqualifiedDeclarator "=" AssignmentExpression
	| AbstractDeclaratorOpt %merge<merge>
	| AbstractDeclaratorOpt "=" AssignmentExpression
	;

FunctionDefinition
	: DeclSpecifier FDDeclarator FunctionBody %merge<merge>
	| DeclSpecifier FDDeclarator "try" FunctionBody HandlerSeq
	| CDtorModifierSeq FDDeclarator CtorInitializerOpt FunctionBody
	| FDDeclarator CtorInitializerOpt FunctionBody %merge<merge>
	| CDtorModifierSeq FDDeclarator "try" CtorInitializerOpt FunctionBody HandlerSeq
	| FDDeclarator "try" CtorInitializerOpt FunctionBody HandlerSeq
	;

FDDeclarator
	: Declarator
	;

FunctionBody
	: CompoundStatement
	;

CtorInitializerOpt
	: empty
	| ":" MemInitializerList
	;

ClassSpecifier
	: ClassKey ClassHeadNameOpt BaseClauseOpt "{" MemberDeclarationSeqOpt "}"
	;

ClassHeadNameOpt
	: empty
	| ClassHeadName
	;

ClassHeadName
	: Identifier %prec "::"
	| TemplateId %prec "::"
	| Identifier "::" ClassHeadName
	| Identifier "<" TemplateArgumentListOpt ">" "::" ClassHeadName
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" ClassHeadName
	;

ClassKey
	: "class"
	| "struct"
	| "union"
	;

MemberDeclarationSeqOpt
	: empty
	| MemberDeclarationSeqOpt ";"
	| MemberDeclarationSeqOpt MemberDeclaration
	| MemberDeclarationSeqOpt AccessSpecifier ":"
	;

AccessSpecifier
	: "public"
	| "private"
	| "protected"
	;

MemberDeclaration
	: DeclSpecifier MemberDeclaratorList ";" %merge<merge>
	| DeclSpecifier ";" %merge<merge>
	| PQualifiedId ";" %merge<merge>
	| "using" IdExpression ";"
	| FunctionDefinition
	| CDtorProtoDecl %merge<merge>
	| TemplateDeclaration
	;

CDtorProtoDecl
	: CDtorModifierSeq MemberDeclarator ";"
	| MemberDeclarator ";"
	;

MemberDeclaratorList
	: MemberDeclarator
	| MemberDeclarator "," MemberDeclaratorList
	;

MemberDeclarator
	: Declarator
	| Declarator "=" ConstantExpression
	| IdentifierOpt ":" ConstantExpression
	;

IdentifierOpt
	: empty
	| Identifier
	;

CDtorModifier
	: "explicit"
	| "virtual"
	| "inline"
	| "friend"
	;

CDtorModifierSeq
	: CDtorModifier
	| CDtorModifierSeq CDtorModifier
	;

BaseClauseOpt
	: empty
	| ":" BaseSpecifierList
	;

BaseSpecifierList
	: BaseSpecifier
	| BaseSpecifier "," BaseSpecifierList
	;

BaseSpecifier
	: PQClassName
	| "virtual" AccessSpecifierOpt PQClassName
	| AccessSpecifier VirtualOpt PQClassName
	;

VirtualOpt
	: empty
	| "virtual"
	;

AccessSpecifierOpt
	: empty
	| AccessSpecifier
	;

PQClassName
	: PQTypeName
	;

ConversionFunctionId
	: "operator" ConversionTypeId
	;

ConversionTypeId
	: TypeSpecifier ConversionDeclaratorOpt
	;

ConversionDeclaratorOpt
	: empty %prec TK_PREFER_SHIFT
	| "*" CVQualifierSeqOpt ConversionDeclaratorOpt
	| "&" CVQualifierSeqOpt ConversionDeclaratorOpt
	| PtrToMemberName "*" CVQualifierSeqOpt ConversionDeclaratorOpt
	;

MemInitializerList
	: MemInitializer
	| MemInitializer "," MemInitializerList
	;

MemInitializer
	: MemInitializerId "(" ExpressionListOpt ")"
	;

MemInitializerId
	: PQTypeName
	;

OperatorFunctionId
	: "operator" Operator
	;

Operator
	: "new" %prec TK_PREFER_SHIFT
	| "delete" %prec TK_PREFER_SHIFT
	| "new" "[" "]"
	| "delete" "[" "]"
	| "!"
	| "~"
	| "++"
	| "--"
	| "+"
	| "-"
	| "*"
	| "/"
	| "%"
	| "<<"
	| ">>"
	| "&"
	| "^"
	| "|"
	| "="
	| "+="
	| "-="
	| "*="
	| "/="
	| "%="
	| "<<="
	| ">>="
	| "&="
	| "^="
	| "|="
	| "=="
	| "!="
	| "<"
	| ">"
	| "<="
	| ">="
	| "&&"
	| "||"
	| "->"
	| "->*"
	| "[" "]"
	| "(" ")"
	| ","
	;

TemplateDeclaration
	: TemplatePreamble FunctionDefinition
	| TemplatePreamble SimpleDeclaration %merge<merge>
	| TemplatePreamble TemplateDeclaration
	| TemplatePreamble CDtorProtoDecl %merge<merge>
	;

TemplatePreamble
	: "template" "<" TemplateParameterList ">"
	| "export" "template" "<" TemplateParameterList ">"
	| "template" "<" ">"
	| "export" "template" "<" ">"
	;

TemplateParameterList
	: ClassOrTypename IdentifierOpt DefaultTypeOpt TemplateParameterListContinuation %merge<merge>
	| ParameterDeclaration TemplateParameterListContinuation %merge<merge>
	| "template" "<" TemplateParameterList ">" "class" IdentifierOpt
	  DefaultTemplateOpt TemplateParameterListContinuation
	;

TemplateParameterListContinuation
	: empty
	| "," TemplateParameterList
	;

ClassOrTypename
	: "class"
	| "typename"
	;

DefaultTypeOpt
	: empty
	| "=" TypeId
	;

DefaultTemplateOpt
	: empty
	| "=" IdExpression
	;

TemplateArgumentListOpt
	: empty
	| TemplateArgumentList
	;

TemplateId
	: Identifier "<" TemplateArgumentListOpt ">"
	| OperatorFunctionId "<" TemplateArgumentListOpt ">"
	;

TemplateArgumentList
	: TemplateArgument
	;

TemplateArgumentListTailOpt
	: empty
	| "," TemplateArgument
	;

TemplateArgument
	: TypeId TemplateArgumentListTailOpt %merge<merge>
	| AssignmentExpression TemplateArgumentListTailOpt %merge<merge>
	;

ExplicitInstantiation
	: "template" BlockDeclaration
	| "inline" "template" BlockDeclaration
	;

TryBlock
	: "try" CompoundStatement HandlerSeq
	;

HandlerSeq
	: Handler
	| Handler HandlerSeq
	;

Handler
	: "catch" "(" HandlerParameter ")" CompoundStatement
	| "catch" "(" "..." ")" CompoundStatement
	;

HandlerParameter
	: TypeSpecifier UnqualifiedDeclarator
	| TypeSpecifier AbstractDeclaratorOpt
	;

UnqualifiedDeclarator
	: Declarator
	;

ThrowExpression
	: "throw"
	| "throw" AssignmentExpression
	;

ExceptionSpecificationOpt
	: empty
	| "throw" "(" ")"
	| "throw" "(" TypeIdList ")"
	;

TypeIdList
	: TypeId
	| TypeId "," TypeIdList
	;

NamespaceDefinition
	: "inline" "namespace" IdentifierOpt "{" TranslationUnit "}"
	| "namespace" IdentifierOpt "{" TranslationUnit "}"
	;

NamespaceDecl
	: "namespace" Identifier "=" IdExpression ";"
	| "using" IdExpression ";"
	| "using" "namespace" IdExpression ";"
	;

empty
	:
	;

/****************************************************************************
 *
 * GNU Extensions
 *
 ****************************************************************************/

PrimaryExpression
	: "(" CompoundStatement ")" %dprec 2
	;

PrimaryExpression
	: "(" TypeId ")" CompoundInitializer
	;

AssignmentExpression
	: CompoundInitializer
	;

PreprocString
	: "__func__"
	| "__FUNCTION__"
	| "__PRETTY_FUNCTION__"
	;

PostfixExpression
	: "alignof" "(" TypeId ")" %merge<merge>
	| "alignof" "(" Expression ")" %merge<merge>
	| "__builtin_constant_p" ParenthesizedExpression
	| "__builtin_offsetof" "(" TypeId "," NamesAfterDot ")"
	| "__builtin_expect" "(" Expression "," Expression ")"
	| "__builtin_va_arg" "(" Expression "," TypeId ")"
	;

NamesAfterDot
	: NameAfterDot
	| NamesAfterDot "." NameAfterDot
	| NamesAfterDot "[" Expression "]"
	;

ParenthesizedExpression
	: "(" Expression ")"
	;

ConditionalExpression
	: BinaryExpression "?" ":" AssignmentExpression
	;

Statement
	: FunctionDefinition
	;

ExplicitInstantiation
	: "extern" "template" BlockDeclaration
	;

Statement
	: "case" ConstantExpression "..." ConstantExpression ":" Statement
	;

CompoundStatement
	: CompoundStmtHelper LabeledEmptyStatementList "}"
	;

LabeledEmptyStatementList
	: LabelAndColon LabeledEmptyStatementListOpt
	| "case" ConstantExpression ":" LabeledEmptyStatementListOpt
	| "case" ConstantExpression "..." ConstantExpression ":" LabeledEmptyStatementListOpt
	| "default" ":" LabeledEmptyStatementListOpt
	;

LabeledEmptyStatementListOpt
	: empty
	| LabeledEmptyStatementList
	;

DeclSpecifier
	: TypeofTypeSpecifier UberModifierSeqOpt
	| UberModifierSeq TypeofTypeSpecifier UberModifierSeqOpt
	;

TypeSpecifier
	: TypeofTypeSpecifier UberCVQualifierSeqOpt
	| UberCVQualifierSeq TypeofTypeSpecifier UberCVQualifierSeqOpt
	;

TypeofTypeSpecifier
	: TypeofExpr %merge<merge>
	| TypeofType %merge<merge>
	;

TypeofExpr
	: "typeof" "(" Expression ")"
	;

TypeofType
	: "typeof" "(" TypeId ")"
	;

BinExp_mid
	: BinExp_mid "<?" BinExp_high
	| BinExp_mid ">?" BinExp_high
	;

Operator
	: "<?"
	| ">?"
	;

BracketedWordOpt
	: empty
	| "[" Identifier "]"
	;

ParenthesizedExpressionOpt
	: empty
	| "(" Expression ")"
	;

OpConstraint
	: BracketedWordOpt StringLiteral ParenthesizedExpressionOpt
	;

OpConstraintList
	: empty
	| OpConstraint
	| OpConstraint "," OpConstraintList
	;

OpConstraints
	: empty
	| NonemptyOpConstraints
	;

NonemptyOpConstraints
	: OpConstraints ":" OpConstraintList
	| OpConstraints "::" OpConstraintList
	;

AsmDefinition
	: "asm" CVQualifierSeq "(" StringLiteral ")" ";"
	| "asm" CVQualifierSeq "(" StringLiteral NonemptyOpConstraints ")" ";"
	| "asm" "(" StringLiteral NonemptyOpConstraints ")" ";"
	;

Declarator
	: DirectDeclarator "asm" "(" StringLiteral ")"
	| DirectDeclarator "asm" "(" StringLiteral ")" AttributeSpecifierList
	;

InitializerClause
	: Identifier ":" SimpleInitializerClause
	| DesignatorList "=" SimpleInitializerClause
	| DesignatorList SimpleInitializerClause
	;

DesignatorList
	: Designator
	| Designator DesignatorList
	;

Designator
	: "." Identifier
	| "[" ConstantExpression "]"
	| "[" ConstantExpression "..." ConstantExpression "]"
	;

CVQualifier
	: "restrict"
	;

UberModifier
	: "restrict"
	;

UberCVQualifier
	: "restrict"
	;

DirectDeclarator
	: DirectDeclarator "[" CVQualifierSeq "]"
	;

DirectAbstractDeclarator
	: DirectAbstractDeclaratorOpt "[" CVQualifierSeq "]"
	;

DeclSpecifier
	: PQTypeName BuggyGccTypeModifier UberModifierSeqOpt
	| UberModifierSeq PQTypeName BuggyGccTypeModifier UberModifierSeqOpt
	;

BuggyGccTypeModifier
	: "long"
	| "short"
	| "signed"
	| "unsigned"
	| "long" BuggyGccTypeModifier
	| "short" BuggyGccTypeModifier
	| "signed" BuggyGccTypeModifier
	| "unsigned" BuggyGccTypeModifier
	;

UnaryExpression
	: "&&" Identifier
	;

Statement
	: "goto" "*" Expression ";"
	;

UberTypeKeyword
	: "_Complex"
	| "_Imaginary"
	;

PrimaryExpression
	: "__real__" PrimaryExpression
	| "__imag__" PrimaryExpression
	;

LabelAndColon
	: Identifier ":" AttributeSpecifierList %prec TK_PREFER_REDUCE
	;

ElaboratedTypeSpecifier
	: ClassKey AttributeSpecifier PQTypeName
	;

ClassSpecifier
	: ClassKey AttributeSpecifierList ClassHeadNameOpt BaseClauseOpt "{" MemberDeclarationSeqOpt "}"
	;

ElaboratedTypeSpecifier
	: "enum" AttributeSpecifierList PQTypeName
	;

EnumSpecifier
	: "enum" AttributeSpecifierList "{" EnumeratorListOpt "}"
	| "enum" AttributeSpecifierList PQTypeName "{" EnumeratorListOpt "}"
	;

UberModifier
	: AttributeSpecifier
	;

UberCVQualifier
	: AttributeSpecifier
	;

InitDeclaratorList
	: InitDeclarator "," AttributeSpecifierList InitDeclaratorList
	;

CVQualAttrSeqOpt
	: empty
	| CVQualAttrSeq
	;

CVQualAttrSeq
	: CVQualAttr
	| CVQualAttr CVQualAttrSeq
	;

CVQualAttr
	: CVQualifier
	| AttributeSpecifier
	;

Declarator
	: "*" CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt Declarator
	| DirectDeclarator AttributeSpecifierList
	;

DirectDeclarator
	: "(" AttributeSpecifierList Declarator ")"
	;

AbstractDeclarator
	: "*" CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt AbstractDeclaratorOpt
	| DirectAbstractDeclarator AttributeSpecifierList
	;

DirectAbstractDeclarator
	: "(" AttributeSpecifierList AbstractDeclarator ")"
	;

MemberDeclarator
	: IdentifierOpt ":" ConstantExpression AttributeSpecifierList
	;

CDtorModifier
	: AttributeSpecifier
	;

NamespaceDefinition
	: "namespace" IdentifierOpt AttributeSpecifierList "{" TranslationUnit "}"
	;

AttributeWord
	: TK_NAME
	| "asm"
	| "auto"
	| "break"
	| "bool"
	| "case"
	| "catch"
	| "cdecl"
	| "char"
	| "class"
	| "const"
	| "const_cast"
	| "continue"
	| "default"
	| "delete"
	| "do"
	| "double"
	| "dynamic_cast"
	| "else"
	| "enum"
	| "explicit"
	| "export"
	| "extern"
	| "false"
	| "float"
	| "for"
	| "friend"
	| "goto"
	| "if"
	| "inline"
	| "int"
	| "long"
	| "mutable"
	| "namespace"
	| "new"
	| "operator"
	| "pascal"
	| "private"
	| "protected"
	| "public"
	| "register"
	| "reinterpret_cast"
	| "return"
	| "short"
	| "signed"
	| "sizeof"
	| "static"
	| "static_cast"
	| "struct"
	| "switch"
	| "template"
	| "this"
	| "throw"
	| "true"
	| "try"
	| "typedef"
	| "typeid"
	| "typename"
	| "union"
	| "unsigned"
	| "using"
	| "virtual"
	| "void"
	| "volatile"
	| "wchar_t"
	| "while"
	;

CommaSepExpressionListOpt
	: empty
	| ExpressionList
	;

AttributeParameters
	: CommaSepExpressionListOpt
	;

Attribute
	: empty
	| AttributeWord
	| AttributeWord "(" AttributeParameters ")"
	;

AttributeList
	: Attribute
	| Attribute "," AttributeList
	;

AttributeSpecifier
	: "__attribute__" "(" "(" AttributeList ")" ")"
	;

AttributeSpecifierList
	: AttributeSpecifier
	| AttributeSpecifier AttributeSpecifierList
	;

NamespaceDecl
	: "using" "namespace" IdExpression AttributeSpecifierList ";"
	;

%%

char const *
tokname (yySymbol yytoken)
{
  return yytname[yytoken - 255];
}
