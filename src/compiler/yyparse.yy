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
#include "src/ast/ast.h"
#include "merge.h"
using namespace ast;
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

%token<token> TK_ASM			"asm"
%token<token> TK_AUTO			"auto"
%token<token> TK_BREAK			"break"
%token<token> TK_BOOL			"bool"
%token<token> TK_CASE			"case"
%token<token> TK_CATCH			"catch"
%token<token> TK_CDECL			"cdecl"
%token<token> TK_CHAR			"char"
%token<token> TK_CLASS			"class"
%token<token> TK_CONST			"const"
%token<token> TK_CONST_CAST		"const_cast"
%token<token> TK_CONTINUE		"continue"
%token<token> TK_DEFAULT		"default"
%token<token> TK_DELETE			"delete"
%token<token> TK_DO			"do"
%token<token> TK_DOUBLE			"double"
%token<token> TK_DYNAMIC_CAST		"dynamic_cast"
%token<token> TK_ELSE			"else"
%token<token> TK_ENUM			"enum"
%token<token> TK_EXPLICIT		"explicit"
%token<token> TK_EXPORT			"export"
%token<token> TK_EXTERN			"extern"
%token<token> TK_FALSE			"false"
%token<token> TK_FLOAT			"float"
%token<token> TK_FOR			"for"
%token<token> TK_FRIEND			"friend"
%token<token> TK_GOTO			"goto"
%token<token> TK_IF			"if"
%token<token> TK_INLINE			"inline"
%token<token> TK_INT			"int"
%token<token> TK_LONG			"long"
%token<token> TK_MUTABLE		"mutable"
%token<token> TK_NAMESPACE		"namespace"
%token<token> TK_NEW			"new"
%token<token> TK_OPERATOR		"operator"
%token<token> TK_PASCAL			"pascal"
%token<token> TK_PRIVATE		"private"
%token<token> TK_PROTECTED		"protected"
%token<token> TK_PUBLIC			"public"
%token<token> TK_REGISTER		"register"
%token<token> TK_REINTERPRET_CAST	"reinterpret_cast"
%token<token> TK_RETURN			"return"
%token<token> TK_SHORT			"short"
%token<token> TK_SIGNED			"signed"
%token<token> TK_SIZEOF			"sizeof"
%token<token> TK_STATIC			"static"
%token<token> TK_STATIC_CAST		"static_cast"
%token<token> TK_STRUCT			"struct"
%token<token> TK_SWITCH			"switch"
%token<token> TK_TEMPLATE		"template"
%token<token> TK_THIS			"this"
%token<token> TK_THROW			"throw"
%token<token> TK_TRUE			"true"
%token<token> TK_TRY			"try"
%token<token> TK_TYPEDEF		"typedef"
%token<token> TK_TYPEID			"typeid"
%token<token> TK_TYPENAME		"typename"
%token<token> TK_UNION			"union"
%token<token> TK_UNSIGNED		"unsigned"
%token<token> TK_USING			"using"
%token<token> TK_VIRTUAL		"virtual"
%token<token> TK_VOID			"void"
%token<token> TK_VOLATILE		"volatile"
%token<token> TK_WCHAR_T		"wchar_t"
%token<token> TK_WHILE			"while"

%token<token> TK_LPAREN			"("
%token<token> TK_RPAREN			")"
%token<token> TK_LBRACKET		"["
%token<token> TK_RBRACKET		"]"
%token<token> TK_ARROW			"->"
%token<token> TK_COLONCOLON		"::"
%token<token> TK_DOT			"."
%token<token> TK_BANG			"!"
%token<token> TK_TILDE			"~"
%token<token> TK_PLUS			"+"
%token<token> TK_MINUS			"-"
%token<token> TK_PLUSPLUS		"++"
%token<token> TK_MINUSMINUS		"--"
%token<token> TK_AND			"&"
%token<token> TK_STAR			"*"
%token<token> TK_DOTSTAR		".*"
%token<token> TK_ARROWSTAR		"->*"
%token<token> TK_SLASH			"/"
%token<token> TK_PERCENT		"%"
%token<token> TK_LEFTSHIFT		"<<"
%token<token> TK_RIGHTSHIFT		">>"
%token<token> TK_LESSTHAN		"<"
%token<token> TK_LESSEQ			"<="
%token<token> TK_GREATERTHAN		">"
%token<token> TK_GREATEREQ		">="
%token<token> TK_EQUALEQUAL		"=="
%token<token> TK_NOTEQUAL		"!="
%token<token> TK_XOR			"^"
%token<token> TK_OR			"|"
%token<token> TK_ANDAND			"&&"
%token<token> TK_OROR			"||"
%token<token> TK_QUESTION		"?"
%token<token> TK_COLON			":"
%token<token> TK_EQUAL			"="
%token<token> TK_STAREQUAL		"*="
%token<token> TK_SLASHEQUAL		"/="
%token<token> TK_PERCENTEQUAL		"%="
%token<token> TK_PLUSEQUAL		"+="
%token<token> TK_MINUSEQUAL		"-="
%token<token> TK_ANDEQUAL		"&="
%token<token> TK_XOREQUAL		"^="
%token<token> TK_OREQUAL		"|="
%token<token> TK_LEFTSHIFTEQUAL		"<<="
%token<token> TK_RIGHTSHIFTEQUAL	">>="
%token<token> TK_COMMA			","
%token<token> TK_ELLIPSIS		"..."
%token<token> TK_SEMICOLON		";"
%token<token> TK_LBRACE			"{"
%token<token> TK_RBRACE			"}"

/* GNU Extensions */
%token<token> TK_ATTRIBUTE		"__attribute__"
%token<token> TK_RESTRICT		"restrict"
%token<token> TK_TYPEOF			"typeof"
%token<token> TK_ALIGNOF		"alignof"
%token<token> TK___FUNC__		"__func__"
%token<token> TK___FUNCTION__		"__FUNCTION__"
%token<token> TK___PRETTY_FUNCTION__	"__PRETTY_FUNCTION__"
%token<token> TK__COMPLEX		"_Complex"
%token<token> TK__IMAGINARY		"_Imaginary"
%token<token> TK___REAL__		"__real__"
%token<token> TK___IMAG__		"__imag__"
%token<token> TK___BUILTIN_CONSTANT_P	"__builtin_constant_p"
%token<token> TK___BUILTIN_OFFSETOF	"__builtin_offsetof"
%token<token> TK___BUILTIN_EXPECT	"__builtin_expect"
%token<token> TK___BUILTIN_VA_ARG	"__builtin_va_arg"

%token<token> TK_MIN_OP			"<?"
%token<token> TK_MAX_OP			">?"

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

%type<node> AbstractDeclarator AbstractDeclaratorOpt AccessSpecifier AccessSpecifierOpt ArgumentList AsmDefinition AssignmentExpression AssignmentOperator Attribute AttributeList AttributeParameters AttributeSpecifier AttributeSpecifierList AttributeWord BaseClauseOpt BaseSpecifier BaseSpecifierList BinaryExpression BinExp_high BinExp_mid BlockDeclaration BracketedWordOpt BuggyGccTypeModifier CastExpression CastKeyword CDtorModifier CDtorModifierSeq CDtorProtoDecl ClassHeadName ClassHeadNameOpt ClassKey ClassOrTypename ClassSpecifier ColonColonOpt CommaOpt CommaSepExpressionListOpt CompoundInitializer CompoundStatement CompoundStmtHelper Condition ConditionalExpression ConditionOpt ConstantExpression ConstantExpressionOpt ConversionDeclaratorOpt ConversionFunctionId ConversionTypeId CtorExpressionType CtorInitializerOpt CVQualAttr CVQualAttrSeq CVQualAttrSeqOpt CVQualifier CVQualifierSeq CVQualifierSeqOpt Declaration Declarator DeclSpecifier DefaultTemplateOpt DefaultTypeOpt DeleteExpression
%type<node> Designator DesignatorList DirectAbstractDeclarator DirectAbstractDeclaratorOpt DirectDeclarator DirectNewDeclarator ElaboratedOrSpecifier ElaboratedTypeSpecifier EnumeratorDefinition EnumeratorListOpt EnumSpecifier ExceptionSpecificationOpt ExplicitInstantiation Expression ExpressionList ExpressionListOpt ExpressionOpt ExpressionStatement FDDeclarator ForInitStatement FunctionBody FunctionDefinition Handler HandlerParameter HandlerSeq Identifier IdentifierOpt IdExpression InitDeclarator InitDeclaratorList InitDeclaratorListOpt Initializer InitializerClause InitializerList LabelAndColon LabeledEmptyStatementList LabeledEmptyStatementListOpt LinkageSpecification Literal MemberDeclaration MemberDeclarationSeqOpt MemberDeclarator MemberDeclaratorList MemInitializer MemInitializerId MemInitializerList NAD1 NAD2 NameAfterDot NamesAfterDot NamespaceDecl NamespaceDefinition NewDeclaratorOpt NewExpression NewInitializerOpt NewPlacementOpt NewTypeId NonemptyOpConstraints
%type<node> OpConstraint OpConstraintList OpConstraints Operator OperatorFunctionId ParameterDeclaration ParameterDeclarationClause ParameterDeclarationList ParameterDeclarator ParenthesizedExpression ParenthesizedExpressionOpt PostfixExpression PQClassName PQDtorName PQTypeName PQTypeName_ncc PQTypeName_notfirst PQualifiedId PreprocString PrimaryExpression PtrToMemberName SimpleDeclaration SimpleInitializerClause Statement StringLiteral TemplateArgument TemplateArgumentList TemplateArgumentListOpt TemplateArgumentListTailOpt TemplateDeclaration TemplateId TemplateParameterList TemplateParameterListContinuation TemplatePreamble ThrowExpression TranslationUnit TryBlock TypeId TypeIdList TypeofExpr TypeofType TypeofTypeSpecifier TypeSpecifier UberCVQualifier UberCVQualifierSeq UberCVQualifierSeqOpt UberModifier UberModifierSeq UberModifierSeqOpt UberTypeAndCVQualifierSeqOpt UberTypeAndModifierSeqOpt UberTypeKeyword UnaryExpression UnqualifiedDeclarator UnqualifiedId VirtualOpt

%destructor { delete $$; } <*>

%%
/****************************************************************************
 *
 *	Toplevel declarations
 *
 ****************************************************************************/
document
	: TranslationUnit	{ self->doc = make_node<document> (@$, $1); }
	;

Identifier
	: TK_NAME	{ $$ = make_node<Identifier> (@$, $1); }
	;

TranslationUnit
	:	{ $$ = make_node<TranslationUnit> (@$); }
	| TranslationUnit Declaration %dprec 1	{ $$ = make_node<TranslationUnit> (@$, $1, $2); }
	| TranslationUnit ";" %dprec 2	{ $$ = make_node<TranslationUnit> (@$, $1, $2); }
	;

PrimaryExpression
	: Literal	{ $$ = make_node<PrimaryExpression> (@$, $1); }
	| "this"	{ $$ = make_node<PrimaryExpression> (@$, $1); }
	| "(" Expression ")" %dprec 1	{ $$ = make_node<PrimaryExpression> (@$, $1, $2, $3); }
	| IdExpression	{ $$ = make_node<PrimaryExpression> (@$, $1); }
	;

Literal
	: TK_INT_LITERAL	{ $$ = make_node<Literal> (@$, $1); }
	| TK_FLOAT_LITERAL	{ $$ = make_node<Literal> (@$, $1); }
	| StringLiteral	{ $$ = make_node<Literal> (@$, $1); }
	| TK_CHAR_LITERAL	{ $$ = make_node<Literal> (@$, $1); }
	| "true"	{ $$ = make_node<Literal> (@$, $1); }
	| "false"	{ $$ = make_node<Literal> (@$, $1); }
	;

PreprocString
	: TK_STRING_LITERAL	{ $$ = make_node<PreprocString> (@$, $1); }
	;

StringLiteral
	: PreprocString	{ $$ = make_node<StringLiteral> (@$, $1); }
	| PreprocString StringLiteral	{ $$ = make_node<StringLiteral> (@$, $1, $2); }
	;

IdExpression
	: PQualifiedId	{ $$ = make_node<IdExpression> (@$, $1); }
	| "::" PQualifiedId	{ $$ = make_node<IdExpression> (@$, $1, $2); }
	;

UnqualifiedId
	: Identifier	{ $$ = make_node<UnqualifiedId> (@$, $1); }
	| OperatorFunctionId	{ $$ = make_node<UnqualifiedId> (@$, $1); }
	| ConversionFunctionId	{ $$ = make_node<UnqualifiedId> (@$, $1); }
	| TemplateId	{ $$ = make_node<UnqualifiedId> (@$, $1); }
	;

PQualifiedId
	: UnqualifiedId %prec "::" %merge<merge>	{ $$ = make_node<PQualifiedId> (@$, $1); }
	| Identifier "::" PQualifiedId	{ $$ = make_node<PQualifiedId> (@$, $1, $2, $3); }
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQualifiedId %merge<merge>	{ $$ = make_node<PQualifiedId> (@$, $1, $2, $3, $4, $5, $6); }
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQualifiedId %merge<merge>	{ $$ = make_node<PQualifiedId> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

ArgumentList
	: "(" ExpressionListOpt ")"	{ $$ = make_node<ArgumentList> (@$, $1, $2, $3); }
	;

PostfixExpression
	: PrimaryExpression	{ $$ = make_node<PostfixExpression> (@$, $1); }
	| PostfixExpression "[" Expression "]"	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4); }
	| PostfixExpression ArgumentList %merge<merge(1)>	{ $$ = make_node<PostfixExpression> (@$, $1, $2); }
	| "typename" IdExpression ArgumentList	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3); }
	| CtorExpressionType ArgumentList %merge<merge(1)>	{ $$ = make_node<PostfixExpression> (@$, $1, $2); }
	| PostfixExpression "." NameAfterDot	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3); }
	| PostfixExpression "->" NameAfterDot	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3); }
	| PostfixExpression "++"	{ $$ = make_node<PostfixExpression> (@$, $1, $2); }
	| PostfixExpression "--"	{ $$ = make_node<PostfixExpression> (@$, $1, $2); }
	| CastKeyword "<" TypeId ">" "(" Expression ")"	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4, $5, $6, $7); }
	| "typeid" "(" Expression ")" %merge<merge(2)>	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4); }
	| "typeid" "(" TypeId ")" %merge<merge(2)>	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4); }
	;

CtorExpressionType
	: PQTypeName	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "char"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "wchar_t"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "bool"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "short"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "int"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "long"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "signed"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "unsigned"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "float"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "double"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	| "void"	{ $$ = make_node<CtorExpressionType> (@$, $1); }
	;

CastKeyword
	: "dynamic_cast"	{ $$ = make_node<CastKeyword> (@$, $1); }
	| "static_cast"	{ $$ = make_node<CastKeyword> (@$, $1); }
	| "reinterpret_cast"	{ $$ = make_node<CastKeyword> (@$, $1); }
	| "const_cast"	{ $$ = make_node<CastKeyword> (@$, $1); }
	;

ExpressionList
	: AssignmentExpression %merge<merge>	{ $$ = make_node<ExpressionList> (@$, $1); }
	| AssignmentExpression "," ExpressionList %merge<merge>	{ $$ = make_node<ExpressionList> (@$, $1, $2, $3); }
	;

ExpressionListOpt
	:	{ $$ = make_node<ExpressionListOpt> (@$); }
	| ExpressionList	{ $$ = make_node<ExpressionListOpt> (@$, $1); }
	;

UnaryExpression
	: PostfixExpression	{ $$ = make_node<UnaryExpression> (@$, $1); }
	| "++" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "--" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| DeleteExpression	{ $$ = make_node<UnaryExpression> (@$, $1); }
	| "*" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "&" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "+" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "-" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "!" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "~" CastExpression	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "sizeof" UnaryExpression %merge<merge>	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	| "sizeof" "(" TypeId ")" %merge<merge>	{ $$ = make_node<UnaryExpression> (@$, $1, $2, $3, $4); }
	| NewExpression	{ $$ = make_node<UnaryExpression> (@$, $1); }
	;

ColonColonOpt
	:	{ $$ = make_node<ColonColonOpt> (@$); }
	| "::"	{ $$ = make_node<ColonColonOpt> (@$, $1); }
	;

NewExpression
	: ColonColonOpt "new" NewPlacementOpt NewTypeId NewInitializerOpt %merge<merge>	{ $$ = make_node<NewExpression> (@$, $1, $2, $3, $4, $5); }
	| ColonColonOpt "new" NewPlacementOpt "(" TypeId ")" NewInitializerOpt %merge<merge>	{ $$ = make_node<NewExpression> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

NewPlacementOpt
	:	{ $$ = make_node<NewPlacementOpt> (@$); }
	| "(" ExpressionList ")"	{ $$ = make_node<NewPlacementOpt> (@$, $1, $2, $3); }
	;

NewTypeId
	: TypeSpecifier NewDeclaratorOpt	{ $$ = make_node<NewTypeId> (@$, $1, $2); }
	;

NewDeclaratorOpt
	:	{ $$ = make_node<NewDeclaratorOpt> (@$); }
	| "*" CVQualifierSeqOpt NewDeclaratorOpt	{ $$ = make_node<NewDeclaratorOpt> (@$, $1, $2, $3); }
	| PtrToMemberName "*" CVQualifierSeqOpt NewDeclaratorOpt	{ $$ = make_node<NewDeclaratorOpt> (@$, $1, $2, $3, $4); }
	| DirectNewDeclarator	{ $$ = make_node<NewDeclaratorOpt> (@$, $1); }
	;

DirectNewDeclarator
	: "[" Expression "]"	{ $$ = make_node<DirectNewDeclarator> (@$, $1, $2, $3); }
	| DirectNewDeclarator "[" ConstantExpression "]"	{ $$ = make_node<DirectNewDeclarator> (@$, $1, $2, $3, $4); }
	;

NewInitializerOpt
	:	{ $$ = make_node<NewInitializerOpt> (@$); }
	| "(" ExpressionListOpt ")"	{ $$ = make_node<NewInitializerOpt> (@$, $1, $2, $3); }
	;

DeleteExpression
	: ColonColonOpt "delete" CastExpression	{ $$ = make_node<DeleteExpression> (@$, $1, $2, $3); }
	| ColonColonOpt "delete" "[" "]" CastExpression	{ $$ = make_node<DeleteExpression> (@$, $1, $2, $3, $4, $5); }
	;

NameAfterDot
	: NAD1	{ $$ = make_node<NameAfterDot> (@$, $1); }
	| "::" NAD2	{ $$ = make_node<NameAfterDot> (@$, $1, $2); }
	;

NAD1
	: NAD2	{ $$ = make_node<NAD1> (@$, $1); }
	| "template" Identifier "<" TemplateArgumentListOpt ">"	{ $$ = make_node<NAD1> (@$, $1, $2, $3, $4, $5); }
	| "~" Identifier	{ $$ = make_node<NAD1> (@$, $1, $2); }
	| "~" Identifier "<" TemplateArgumentListOpt ">"	{ $$ = make_node<NAD1> (@$, $1, $2, $3, $4, $5); }
	| ConversionFunctionId	{ $$ = make_node<NAD1> (@$, $1); }
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" NAD1	{ $$ = make_node<NAD1> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

NAD2
	: Identifier "<" TemplateArgumentListOpt ">"	{ $$ = make_node<NAD2> (@$, $1, $2, $3, $4); }
	| Identifier	{ $$ = make_node<NAD2> (@$, $1); }
	| OperatorFunctionId	{ $$ = make_node<NAD2> (@$, $1); }
	| OperatorFunctionId "<" TemplateArgumentListOpt ">"	{ $$ = make_node<NAD2> (@$, $1, $2, $3, $4); }
	| "template" OperatorFunctionId "<" TemplateArgumentListOpt ">"	{ $$ = make_node<NAD2> (@$, $1, $2, $3, $4, $5); }
	| Identifier "<" TemplateArgumentListOpt ">" "::" NAD1	{ $$ = make_node<NAD2> (@$, $1, $2, $3, $4, $5, $6); }
	| Identifier "::" NAD1	{ $$ = make_node<NAD2> (@$, $1, $2, $3); }
	;

CastExpression
	: UnaryExpression %merge<merge>	{ $$ = make_node<CastExpression> (@$, $1); }
	| "(" TypeId ")" CastExpression %merge<merge>	{ $$ = make_node<CastExpression> (@$, $1, $2, $3, $4); }
	;

BinExp_high
	: CastExpression %merge<merge>	{ $$ = make_node<BinExp_high> (@$, $1); }
	| BinExp_high ".*" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "->*" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "*" BinExp_high %merge<merge>	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "/" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "%" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "+" BinExp_high %merge<merge>	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "-" BinExp_high %merge<merge>	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high "<<" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	| BinExp_high ">>" BinExp_high	{ $$ = make_node<BinExp_high> (@$, $1, $2, $3); }
	;

BinExp_mid
	: BinExp_high %merge<merge>	{ $$ = make_node<BinExp_mid> (@$, $1); }
	| BinExp_mid "<" BinExp_high %merge<merge>	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	| BinExp_mid ">" BinExp_high %merge<merge>	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	| BinExp_mid "<=" BinExp_high	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	| BinExp_mid ">=" BinExp_high	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	;

BinaryExpression
	: BinExp_mid %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1); }
	| BinaryExpression "==" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "!=" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "&" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "^" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "|" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "&&" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	| BinaryExpression "||" BinaryExpression %merge<merge>	{ $$ = make_node<BinaryExpression> (@$, $1, $2, $3); }
	;

ConditionalExpression
	: BinaryExpression %merge<merge>	{ $$ = make_node<ConditionalExpression> (@$, $1); }
	| BinaryExpression "?" Expression ":" AssignmentExpression %merge<merge>	{ $$ = make_node<ConditionalExpression> (@$, $1, $2, $3, $4, $5); }
	;

AssignmentExpression
	: ConditionalExpression %merge<merge>	{ $$ = make_node<AssignmentExpression> (@$, $1); }
	| BinaryExpression AssignmentOperator AssignmentExpression %merge<merge>	{ $$ = make_node<AssignmentExpression> (@$, $1, $2, $3); }
	| ThrowExpression	{ $$ = make_node<AssignmentExpression> (@$, $1); }
	;

AssignmentOperator
	: "*="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "/="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "%="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "+="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "-="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| ">>="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "<<="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "&="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "^="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "|="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	| "="	{ $$ = make_node<AssignmentOperator> (@$, $1); }
	;

Expression
	: AssignmentExpression %merge<merge>	{ $$ = make_node<Expression> (@$, $1); }
	| Expression "," AssignmentExpression %merge<merge>	{ $$ = make_node<Expression> (@$, $1, $2, $3); }
	;

ExpressionOpt
	:	{ $$ = make_node<ExpressionOpt> (@$); }
	| Expression	{ $$ = make_node<ExpressionOpt> (@$, $1); }
	;

ConstantExpression
	: AssignmentExpression	{ $$ = make_node<ConstantExpression> (@$, $1); }
	;

ConstantExpressionOpt
	:	{ $$ = make_node<ConstantExpressionOpt> (@$); }
	| ConstantExpression	{ $$ = make_node<ConstantExpressionOpt> (@$, $1); }
	;

LabelAndColon
	: Identifier ":" %prec TK_PREFER_SHIFT	{ $$ = make_node<LabelAndColon> (@$, $1, $2); }
	;

Statement
	: LabelAndColon Statement	{ $$ = make_node<Statement> (@$, $1, $2); }
	| "case" ConstantExpression ":" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4); }
	| "default" ":" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3); }
	| ExpressionStatement %merge<merge>	{ $$ = make_node<Statement> (@$, $1); }
	| CompoundStatement	{ $$ = make_node<Statement> (@$, $1); }
	| "if" "(" Condition ")" Statement %prec TK_PREFER_SHIFT	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5); }
	| "if" "(" Condition ")" Statement "else" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5, $6, $7); }
	| "switch" "(" Condition ")" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5); }
	| "while" "(" Condition ")" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5); }
	| "do" Statement "while" "(" Expression ")" ";"	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5, $6, $7); }
	| "for" "(" ForInitStatement ConditionOpt ";" ExpressionOpt ")" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5, $6, $7, $8); }
	| "break" ";"	{ $$ = make_node<Statement> (@$, $1, $2); }
	| "continue" ";"	{ $$ = make_node<Statement> (@$, $1, $2); }
	| "return" Expression ";"	{ $$ = make_node<Statement> (@$, $1, $2, $3); }
	| "return" ";"	{ $$ = make_node<Statement> (@$, $1, $2); }
	| "goto" Identifier ";"	{ $$ = make_node<Statement> (@$, $1, $2, $3); }
	| BlockDeclaration %merge<merge>	{ $$ = make_node<Statement> (@$, $1); }
	| TryBlock	{ $$ = make_node<Statement> (@$, $1); }
	| AsmDefinition	{ $$ = make_node<Statement> (@$, $1); }
	| NamespaceDecl	{ $$ = make_node<Statement> (@$, $1); }
	;

ExpressionStatement
	: ";"	{ $$ = make_node<ExpressionStatement> (@$, $1); }
	| Expression ";"	{ $$ = make_node<ExpressionStatement> (@$, $1, $2); }
	;

CompoundStatement
	: CompoundStmtHelper "}"	{ $$ = make_node<CompoundStatement> (@$, $1, $2); }
	;

CompoundStmtHelper
	: "{"	{ $$ = make_node<CompoundStmtHelper> (@$, $1); }
	| CompoundStmtHelper Statement	{ $$ = make_node<CompoundStmtHelper> (@$, $1, $2); }
	;

Condition
	: Expression %merge<merge>	{ $$ = make_node<Condition> (@$, $1); }
	| TypeSpecifier Declarator "=" AssignmentExpression %merge<merge>	{ $$ = make_node<Condition> (@$, $1, $2, $3, $4); }
	;

ConditionOpt
	:	{ $$ = make_node<ConditionOpt> (@$); }
	| Condition	{ $$ = make_node<ConditionOpt> (@$, $1); }
	;

ForInitStatement
	: ExpressionStatement %merge<merge>	{ $$ = make_node<ForInitStatement> (@$, $1); }
	| SimpleDeclaration %merge<merge>	{ $$ = make_node<ForInitStatement> (@$, $1); }
	;

Declaration
	: BlockDeclaration	{ $$ = make_node<Declaration> (@$, $1); }
	| FunctionDefinition	{ $$ = make_node<Declaration> (@$, $1); }
	| TemplateDeclaration	{ $$ = make_node<Declaration> (@$, $1); }
	| ExplicitInstantiation	{ $$ = make_node<Declaration> (@$, $1); }
	| LinkageSpecification	{ $$ = make_node<Declaration> (@$, $1); }
	| AsmDefinition	{ $$ = make_node<Declaration> (@$, $1); }
	| NamespaceDefinition	{ $$ = make_node<Declaration> (@$, $1); }
	| NamespaceDecl	{ $$ = make_node<Declaration> (@$, $1); }
	;

BlockDeclaration
	: SimpleDeclaration	{ $$ = make_node<BlockDeclaration> (@$, $1); }
	;

SimpleDeclaration
	: DeclSpecifier InitDeclaratorListOpt ";" %merge<merge>	{ $$ = make_node<SimpleDeclaration> (@$, $1, $2, $3); }
	;

DeclSpecifier
	: PQTypeName UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2); }
	| UberModifierSeq PQTypeName UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3); }
	| UberTypeKeyword UberTypeAndModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2); }
	| UberModifierSeq UberTypeKeyword UberTypeAndModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3); }
	| ElaboratedOrSpecifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2); }
	| UberModifierSeq ElaboratedOrSpecifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3); }
	;

ElaboratedOrSpecifier
	: ElaboratedTypeSpecifier	{ $$ = make_node<ElaboratedOrSpecifier> (@$, $1); }
	| ClassSpecifier	{ $$ = make_node<ElaboratedOrSpecifier> (@$, $1); }
	| EnumSpecifier	{ $$ = make_node<ElaboratedOrSpecifier> (@$, $1); }
	;

UberModifierSeq
	: UberModifier	{ $$ = make_node<UberModifierSeq> (@$, $1); }
	| UberModifierSeq UberModifier	{ $$ = make_node<UberModifierSeq> (@$, $1, $2); }
	;

UberModifierSeqOpt
	:	{ $$ = make_node<UberModifierSeqOpt> (@$); }
	| UberModifierSeq	{ $$ = make_node<UberModifierSeqOpt> (@$, $1); }
	;

UberTypeAndModifierSeqOpt
	:	{ $$ = make_node<UberTypeAndModifierSeqOpt> (@$); }
	| UberTypeAndModifierSeqOpt UberModifier	{ $$ = make_node<UberTypeAndModifierSeqOpt> (@$, $1, $2); }
	| UberTypeAndModifierSeqOpt UberTypeKeyword	{ $$ = make_node<UberTypeAndModifierSeqOpt> (@$, $1, $2); }
	;

UberCVQualifierSeq
	: UberCVQualifier	{ $$ = make_node<UberCVQualifierSeq> (@$, $1); }
	| UberCVQualifierSeq UberCVQualifier	{ $$ = make_node<UberCVQualifierSeq> (@$, $1, $2); }
	;

UberCVQualifierSeqOpt
	:	{ $$ = make_node<UberCVQualifierSeqOpt> (@$); }
	| UberCVQualifierSeq	{ $$ = make_node<UberCVQualifierSeqOpt> (@$, $1); }
	;

UberTypeAndCVQualifierSeqOpt
	:	{ $$ = make_node<UberTypeAndCVQualifierSeqOpt> (@$); }
	| UberTypeAndCVQualifierSeqOpt UberCVQualifier	{ $$ = make_node<UberTypeAndCVQualifierSeqOpt> (@$, $1, $2); }
	| UberTypeAndCVQualifierSeqOpt UberTypeKeyword	{ $$ = make_node<UberTypeAndCVQualifierSeqOpt> (@$, $1, $2); }
	;

UberModifier
	: "auto"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "register"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "static"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "extern"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "mutable"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "inline"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "virtual"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "friend"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "typedef"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "const"	{ $$ = make_node<UberModifier> (@$, $1); }
	| "volatile"	{ $$ = make_node<UberModifier> (@$, $1); }
	;

UberCVQualifier
	: "const"	{ $$ = make_node<UberCVQualifier> (@$, $1); }
	| "volatile"	{ $$ = make_node<UberCVQualifier> (@$, $1); }
	;

UberTypeKeyword
	: "char"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "wchar_t"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "bool"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "short"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "int"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "long"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "signed"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "unsigned"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "float"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "double"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "void"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	;

ElaboratedTypeSpecifier
	: ClassKey PQTypeName	{ $$ = make_node<ElaboratedTypeSpecifier> (@$, $1, $2); }
	| "enum" PQTypeName	{ $$ = make_node<ElaboratedTypeSpecifier> (@$, $1, $2); }
	| "typename" PQTypeName	{ $$ = make_node<ElaboratedTypeSpecifier> (@$, $1, $2); }
	;

TypeSpecifier
	: PQTypeName UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2); }
	| UberCVQualifierSeq PQTypeName UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2, $3); }
	| UberTypeKeyword UberTypeAndCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2); }
	| UberCVQualifierSeq UberTypeKeyword UberTypeAndCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2, $3); }
	| ElaboratedOrSpecifier UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2); }
	| UberCVQualifierSeq ElaboratedOrSpecifier UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2, $3); }
	;

PQTypeName
	: PQTypeName_ncc	{ $$ = make_node<PQTypeName> (@$, $1); }
	| "::" PQTypeName_ncc	{ $$ = make_node<PQTypeName> (@$, $1, $2); }
	;

PQTypeName_ncc
	: Identifier %prec "::"	{ $$ = make_node<PQTypeName_ncc> (@$, $1); }
	| TemplateId %prec "::" %merge<merge>	{ $$ = make_node<PQTypeName_ncc> (@$, $1); }
	| Identifier "::" PQTypeName_notfirst	{ $$ = make_node<PQTypeName_ncc> (@$, $1, $2, $3); }
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQTypeName_notfirst %merge<merge>	{ $$ = make_node<PQTypeName_ncc> (@$, $1, $2, $3, $4, $5, $6); }
	;

PQTypeName_notfirst
	: PQTypeName_ncc %prec "::"	{ $$ = make_node<PQTypeName_notfirst> (@$, $1); }
	| "template" TemplateId %prec "::"	{ $$ = make_node<PQTypeName_notfirst> (@$, $1, $2); }
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQTypeName_notfirst	{ $$ = make_node<PQTypeName_notfirst> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

EnumSpecifier
	: "enum" "{" EnumeratorListOpt "}"	{ $$ = make_node<EnumSpecifier> (@$, $1, $2, $3, $4); }
	| "enum" Identifier "{" EnumeratorListOpt "}"	{ $$ = make_node<EnumSpecifier> (@$, $1, $2, $3, $4, $5); }
	;

EnumeratorListOpt
	:	{ $$ = make_node<EnumeratorListOpt> (@$); }
	| EnumeratorDefinition	{ $$ = make_node<EnumeratorListOpt> (@$, $1); }
	| EnumeratorDefinition "," EnumeratorListOpt	{ $$ = make_node<EnumeratorListOpt> (@$, $1, $2, $3); }
	;

EnumeratorDefinition
	: Identifier	{ $$ = make_node<EnumeratorDefinition> (@$, $1); }
	| Identifier "=" ConstantExpression	{ $$ = make_node<EnumeratorDefinition> (@$, $1, $2, $3); }
	;

AsmDefinition
	: "asm" "(" StringLiteral ")" ";"	{ $$ = make_node<AsmDefinition> (@$, $1, $2, $3, $4, $5); }
	;

LinkageSpecification
	: "extern" TK_STRING_LITERAL "{" TranslationUnit "}"	{ $$ = make_node<LinkageSpecification> (@$, $1, $2, $3, $4, $5); }
	| "extern" TK_STRING_LITERAL Declaration	{ $$ = make_node<LinkageSpecification> (@$, $1, $2, $3); }
	;

InitDeclaratorListOpt
	:	{ $$ = make_node<InitDeclaratorListOpt> (@$); }
	| InitDeclaratorList	{ $$ = make_node<InitDeclaratorListOpt> (@$, $1); }
	;

InitDeclaratorList
	: InitDeclarator %merge<merge>	{ $$ = make_node<InitDeclaratorList> (@$, $1); }
	| InitDeclarator "," InitDeclaratorList %merge<merge>	{ $$ = make_node<InitDeclaratorList> (@$, $1, $2, $3); }
	;

InitDeclarator
	: Declarator %merge<merge>	{ $$ = make_node<InitDeclarator> (@$, $1); }
	| Declarator Initializer %merge<merge>	{ $$ = make_node<InitDeclarator> (@$, $1, $2); }
	;

Initializer
	: "=" SimpleInitializerClause	{ $$ = make_node<Initializer> (@$, $1, $2); }
	| "(" ExpressionList ")"	{ $$ = make_node<Initializer> (@$, $1, $2, $3); }
	;

SimpleInitializerClause
	: AssignmentExpression	{ $$ = make_node<SimpleInitializerClause> (@$, $1); }
	;

InitializerClause
	: SimpleInitializerClause	{ $$ = make_node<InitializerClause> (@$, $1); }
	;

CompoundInitializer
	: "{" InitializerList CommaOpt "}"	{ $$ = make_node<CompoundInitializer> (@$, $1, $2, $3, $4); }
	| "{" "}"	{ $$ = make_node<CompoundInitializer> (@$, $1, $2); }
	;

CommaOpt
	:	{ $$ = make_node<CommaOpt> (@$); }
	| ","	{ $$ = make_node<CommaOpt> (@$, $1); }
	;

InitializerList
	: InitializerClause %merge<merge>	{ $$ = make_node<InitializerList> (@$, $1); }
	| InitializerList "," InitializerClause %merge<merge>	{ $$ = make_node<InitializerList> (@$, $1, $2, $3); }
	;

Declarator
	: "*" CVQualifierSeqOpt Declarator	{ $$ = make_node<Declarator> (@$, $1, $2, $3); }
	| "&" CVQualifierSeqOpt Declarator	{ $$ = make_node<Declarator> (@$, $1, $2, $3); }
	| PtrToMemberName "*" CVQualifierSeqOpt Declarator	{ $$ = make_node<Declarator> (@$, $1, $2, $3, $4); }
	| DirectDeclarator	{ $$ = make_node<Declarator> (@$, $1); }
	;

DirectDeclarator
	: IdExpression %merge<merge>	{ $$ = make_node<DirectDeclarator> (@$, $1); }
	| PQDtorName	{ $$ = make_node<DirectDeclarator> (@$, $1); }
	| DirectDeclarator "(" ParameterDeclarationClause ")" CVQualifierSeqOpt ExceptionSpecificationOpt	{ $$ = make_node<DirectDeclarator> (@$, $1, $2, $3, $4, $5, $6); }
	| DirectDeclarator "[" ConstantExpressionOpt "]" %merge<merge>	{ $$ = make_node<DirectDeclarator> (@$, $1, $2, $3, $4); }
	| "(" Declarator ")"	{ $$ = make_node<DirectDeclarator> (@$, $1, $2, $3); }
	;

PQDtorName
	: "~" Identifier	{ $$ = make_node<PQDtorName> (@$, $1, $2); }
	| "~" Identifier "<" TemplateArgumentListOpt ">"	{ $$ = make_node<PQDtorName> (@$, $1, $2, $3, $4, $5); }
	| Identifier "::" PQDtorName	{ $$ = make_node<PQDtorName> (@$, $1, $2, $3); }
	| Identifier "<" TemplateArgumentListOpt ">" "::" PQDtorName	{ $$ = make_node<PQDtorName> (@$, $1, $2, $3, $4, $5, $6); }
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" PQDtorName	{ $$ = make_node<PQDtorName> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

PtrToMemberName
	: IdExpression "::"	{ $$ = make_node<PtrToMemberName> (@$, $1, $2); }
	;

CVQualifierSeqOpt
	:	{ $$ = make_node<CVQualifierSeqOpt> (@$); }
	| CVQualifierSeq	{ $$ = make_node<CVQualifierSeqOpt> (@$, $1); }
	;

CVQualifierSeq
	: CVQualifier	{ $$ = make_node<CVQualifierSeq> (@$, $1); }
	| CVQualifier CVQualifierSeq	{ $$ = make_node<CVQualifierSeq> (@$, $1, $2); }
	;

CVQualifier
	: "const"	{ $$ = make_node<CVQualifier> (@$, $1); }
	| "volatile"	{ $$ = make_node<CVQualifier> (@$, $1); }
	;

TypeId
	: TypeSpecifier AbstractDeclaratorOpt	{ $$ = make_node<TypeId> (@$, $1, $2); }
	;

AbstractDeclaratorOpt
	:	{ $$ = make_node<AbstractDeclaratorOpt> (@$); }
	| AbstractDeclarator	{ $$ = make_node<AbstractDeclaratorOpt> (@$, $1); }
	;

AbstractDeclarator
	: "*" CVQualifierSeqOpt AbstractDeclaratorOpt	{ $$ = make_node<AbstractDeclarator> (@$, $1, $2, $3); }
	| "&" CVQualifierSeqOpt AbstractDeclaratorOpt	{ $$ = make_node<AbstractDeclarator> (@$, $1, $2, $3); }
	| PtrToMemberName "*" CVQualifierSeqOpt AbstractDeclaratorOpt	{ $$ = make_node<AbstractDeclarator> (@$, $1, $2, $3, $4); }
	| DirectAbstractDeclarator	{ $$ = make_node<AbstractDeclarator> (@$, $1); }
	;

DirectAbstractDeclaratorOpt
	:	{ $$ = make_node<DirectAbstractDeclaratorOpt> (@$); }
	| DirectAbstractDeclarator	{ $$ = make_node<DirectAbstractDeclaratorOpt> (@$, $1); }
	;

DirectAbstractDeclarator
	: DirectAbstractDeclaratorOpt "(" ParameterDeclarationClause ")" CVQualifierSeqOpt ExceptionSpecificationOpt	{ $$ = make_node<DirectAbstractDeclarator> (@$, $1, $2, $3, $4, $5, $6); }
	| DirectAbstractDeclaratorOpt "[" ConstantExpressionOpt "]"	{ $$ = make_node<DirectAbstractDeclarator> (@$, $1, $2, $3, $4); }
	| "(" AbstractDeclarator ")"	{ $$ = make_node<DirectAbstractDeclarator> (@$, $1, $2, $3); }
	;

ParameterDeclarationClause
	: ParameterDeclarationList	{ $$ = make_node<ParameterDeclarationClause> (@$, $1); }
	|	{ $$ = make_node<ParameterDeclarationClause> (@$); }
	;

ParameterDeclarationList
	: "..."	{ $$ = make_node<ParameterDeclarationList> (@$, $1); }
	| ParameterDeclaration "..."	{ $$ = make_node<ParameterDeclarationList> (@$, $1, $2); }
	| ParameterDeclaration %merge<merge>	{ $$ = make_node<ParameterDeclarationList> (@$, $1); }
	| ParameterDeclaration "," ParameterDeclarationList %merge<merge>	{ $$ = make_node<ParameterDeclarationList> (@$, $1, $2, $3); }
	;

ParameterDeclaration
	: TypeSpecifier ParameterDeclarator %merge<merge>	{ $$ = make_node<ParameterDeclaration> (@$, $1, $2); }
	| "register" TypeSpecifier ParameterDeclarator	{ $$ = make_node<ParameterDeclaration> (@$, $1, $2, $3); }
	| TypeSpecifier "register" ParameterDeclarator	{ $$ = make_node<ParameterDeclaration> (@$, $1, $2, $3); }
	;

ParameterDeclarator
	: UnqualifiedDeclarator %merge<merge>	{ $$ = make_node<ParameterDeclarator> (@$, $1); }
	| UnqualifiedDeclarator "=" AssignmentExpression	{ $$ = make_node<ParameterDeclarator> (@$, $1, $2, $3); }
	| AbstractDeclaratorOpt %merge<merge>	{ $$ = make_node<ParameterDeclarator> (@$, $1); }
	| AbstractDeclaratorOpt "=" AssignmentExpression	{ $$ = make_node<ParameterDeclarator> (@$, $1, $2, $3); }
	;

FunctionDefinition
	: DeclSpecifier FDDeclarator FunctionBody %merge<merge>	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3); }
	| DeclSpecifier FDDeclarator "try" FunctionBody HandlerSeq	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3, $4, $5); }
	| CDtorModifierSeq FDDeclarator CtorInitializerOpt FunctionBody	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3, $4); }
	| FDDeclarator CtorInitializerOpt FunctionBody %merge<merge>	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3); }
	| CDtorModifierSeq FDDeclarator "try" CtorInitializerOpt FunctionBody HandlerSeq	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3, $4, $5, $6); }
	| FDDeclarator "try" CtorInitializerOpt FunctionBody HandlerSeq	{ $$ = make_node<FunctionDefinition> (@$, $1, $2, $3, $4, $5); }
	;

FDDeclarator
	: Declarator	{ $$ = make_node<FDDeclarator> (@$, $1); }
	;

FunctionBody
	: CompoundStatement	{ $$ = make_node<FunctionBody> (@$, $1); }
	;

CtorInitializerOpt
	:	{ $$ = make_node<CtorInitializerOpt> (@$); }
	| ":" MemInitializerList	{ $$ = make_node<CtorInitializerOpt> (@$, $1, $2); }
	;

ClassSpecifier
	: ClassKey ClassHeadNameOpt BaseClauseOpt "{" MemberDeclarationSeqOpt "}"	{ $$ = make_node<ClassSpecifier> (@$, $1, $2, $3, $4, $5, $6); }
	;

ClassHeadNameOpt
	:	{ $$ = make_node<ClassHeadNameOpt> (@$); }
	| ClassHeadName	{ $$ = make_node<ClassHeadNameOpt> (@$, $1); }
	;

ClassHeadName
	: Identifier %prec "::"	{ $$ = make_node<ClassHeadName> (@$, $1); }
	| TemplateId %prec "::"	{ $$ = make_node<ClassHeadName> (@$, $1); }
	| Identifier "::" ClassHeadName	{ $$ = make_node<ClassHeadName> (@$, $1, $2, $3); }
	| Identifier "<" TemplateArgumentListOpt ">" "::" ClassHeadName	{ $$ = make_node<ClassHeadName> (@$, $1, $2, $3, $4, $5, $6); }
	| "template" Identifier "<" TemplateArgumentListOpt ">" "::" ClassHeadName	{ $$ = make_node<ClassHeadName> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

ClassKey
	: "class"	{ $$ = make_node<ClassKey> (@$, $1); }
	| "struct"	{ $$ = make_node<ClassKey> (@$, $1); }
	| "union"	{ $$ = make_node<ClassKey> (@$, $1); }
	;

MemberDeclarationSeqOpt
	:	{ $$ = make_node<MemberDeclarationSeqOpt> (@$); }
	| MemberDeclarationSeqOpt ";"	{ $$ = make_node<MemberDeclarationSeqOpt> (@$, $1, $2); }
	| MemberDeclarationSeqOpt MemberDeclaration	{ $$ = make_node<MemberDeclarationSeqOpt> (@$, $1, $2); }
	| MemberDeclarationSeqOpt AccessSpecifier ":"	{ $$ = make_node<MemberDeclarationSeqOpt> (@$, $1, $2, $3); }
	;

AccessSpecifier
	: "public"	{ $$ = make_node<AccessSpecifier> (@$, $1); }
	| "private"	{ $$ = make_node<AccessSpecifier> (@$, $1); }
	| "protected"	{ $$ = make_node<AccessSpecifier> (@$, $1); }
	;

MemberDeclaration
	: DeclSpecifier MemberDeclaratorList ";" %merge<merge>	{ $$ = make_node<MemberDeclaration> (@$, $1, $2, $3); }
	| DeclSpecifier ";" %merge<merge>	{ $$ = make_node<MemberDeclaration> (@$, $1, $2); }
	| PQualifiedId ";" %merge<merge>	{ $$ = make_node<MemberDeclaration> (@$, $1, $2); }
	| "using" IdExpression ";"	{ $$ = make_node<MemberDeclaration> (@$, $1, $2, $3); }
	| FunctionDefinition	{ $$ = make_node<MemberDeclaration> (@$, $1); }
	| CDtorProtoDecl %merge<merge>	{ $$ = make_node<MemberDeclaration> (@$, $1); }
	| TemplateDeclaration	{ $$ = make_node<MemberDeclaration> (@$, $1); }
	;

CDtorProtoDecl
	: CDtorModifierSeq MemberDeclarator ";"	{ $$ = make_node<CDtorProtoDecl> (@$, $1, $2, $3); }
	| MemberDeclarator ";"	{ $$ = make_node<CDtorProtoDecl> (@$, $1, $2); }
	;

MemberDeclaratorList
	: MemberDeclarator	{ $$ = make_node<MemberDeclaratorList> (@$, $1); }
	| MemberDeclarator "," MemberDeclaratorList	{ $$ = make_node<MemberDeclaratorList> (@$, $1, $2, $3); }
	;

MemberDeclarator
	: Declarator	{ $$ = make_node<MemberDeclarator> (@$, $1); }
	| Declarator "=" ConstantExpression	{ $$ = make_node<MemberDeclarator> (@$, $1, $2, $3); }
	| IdentifierOpt ":" ConstantExpression	{ $$ = make_node<MemberDeclarator> (@$, $1, $2, $3); }
	;

IdentifierOpt
	:	{ $$ = make_node<IdentifierOpt> (@$); }
	| Identifier	{ $$ = make_node<IdentifierOpt> (@$, $1); }
	;

CDtorModifier
	: "explicit"	{ $$ = make_node<CDtorModifier> (@$, $1); }
	| "virtual"	{ $$ = make_node<CDtorModifier> (@$, $1); }
	| "inline"	{ $$ = make_node<CDtorModifier> (@$, $1); }
	| "friend"	{ $$ = make_node<CDtorModifier> (@$, $1); }
	;

CDtorModifierSeq
	: CDtorModifier	{ $$ = make_node<CDtorModifierSeq> (@$, $1); }
	| CDtorModifierSeq CDtorModifier	{ $$ = make_node<CDtorModifierSeq> (@$, $1, $2); }
	;

BaseClauseOpt
	:	{ $$ = make_node<BaseClauseOpt> (@$); }
	| ":" BaseSpecifierList	{ $$ = make_node<BaseClauseOpt> (@$, $1, $2); }
	;

BaseSpecifierList
	: BaseSpecifier	{ $$ = make_node<BaseSpecifierList> (@$, $1); }
	| BaseSpecifier "," BaseSpecifierList	{ $$ = make_node<BaseSpecifierList> (@$, $1, $2, $3); }
	;

BaseSpecifier
	: PQClassName	{ $$ = make_node<BaseSpecifier> (@$, $1); }
	| "virtual" AccessSpecifierOpt PQClassName	{ $$ = make_node<BaseSpecifier> (@$, $1, $2, $3); }
	| AccessSpecifier VirtualOpt PQClassName	{ $$ = make_node<BaseSpecifier> (@$, $1, $2, $3); }
	;

VirtualOpt
	:	{ $$ = make_node<VirtualOpt> (@$); }
	| "virtual"	{ $$ = make_node<VirtualOpt> (@$, $1); }
	;

AccessSpecifierOpt
	:	{ $$ = make_node<AccessSpecifierOpt> (@$); }
	| AccessSpecifier	{ $$ = make_node<AccessSpecifierOpt> (@$, $1); }
	;

PQClassName
	: PQTypeName	{ $$ = make_node<PQClassName> (@$, $1); }
	;

ConversionFunctionId
	: "operator" ConversionTypeId	{ $$ = make_node<ConversionFunctionId> (@$, $1, $2); }
	;

ConversionTypeId
	: TypeSpecifier ConversionDeclaratorOpt	{ $$ = make_node<ConversionTypeId> (@$, $1, $2); }
	;

ConversionDeclaratorOpt
	: %prec TK_PREFER_SHIFT	{ $$ = make_node<ConversionDeclaratorOpt> (@$); }
	| "*" CVQualifierSeqOpt ConversionDeclaratorOpt	{ $$ = make_node<ConversionDeclaratorOpt> (@$, $1, $2, $3); }
	| "&" CVQualifierSeqOpt ConversionDeclaratorOpt	{ $$ = make_node<ConversionDeclaratorOpt> (@$, $1, $2, $3); }
	| PtrToMemberName "*" CVQualifierSeqOpt ConversionDeclaratorOpt	{ $$ = make_node<ConversionDeclaratorOpt> (@$, $1, $2, $3, $4); }
	;

MemInitializerList
	: MemInitializer	{ $$ = make_node<MemInitializerList> (@$, $1); }
	| MemInitializer "," MemInitializerList	{ $$ = make_node<MemInitializerList> (@$, $1, $2, $3); }
	;

MemInitializer
	: MemInitializerId "(" ExpressionListOpt ")"	{ $$ = make_node<MemInitializer> (@$, $1, $2, $3, $4); }
	;

MemInitializerId
	: PQTypeName	{ $$ = make_node<MemInitializerId> (@$, $1); }
	;

OperatorFunctionId
	: "operator" Operator	{ $$ = make_node<OperatorFunctionId> (@$, $1, $2); }
	;

Operator
	: "new" %prec TK_PREFER_SHIFT	{ $$ = make_node<Operator> (@$, $1); }
	| "delete" %prec TK_PREFER_SHIFT	{ $$ = make_node<Operator> (@$, $1); }
	| "new" "[" "]"	{ $$ = make_node<Operator> (@$, $1, $2, $3); }
	| "delete" "[" "]"	{ $$ = make_node<Operator> (@$, $1, $2, $3); }
	| "!"	{ $$ = make_node<Operator> (@$, $1); }
	| "~"	{ $$ = make_node<Operator> (@$, $1); }
	| "++"	{ $$ = make_node<Operator> (@$, $1); }
	| "--"	{ $$ = make_node<Operator> (@$, $1); }
	| "+"	{ $$ = make_node<Operator> (@$, $1); }
	| "-"	{ $$ = make_node<Operator> (@$, $1); }
	| "*"	{ $$ = make_node<Operator> (@$, $1); }
	| "/"	{ $$ = make_node<Operator> (@$, $1); }
	| "%"	{ $$ = make_node<Operator> (@$, $1); }
	| "<<"	{ $$ = make_node<Operator> (@$, $1); }
	| ">>"	{ $$ = make_node<Operator> (@$, $1); }
	| "&"	{ $$ = make_node<Operator> (@$, $1); }
	| "^"	{ $$ = make_node<Operator> (@$, $1); }
	| "|"	{ $$ = make_node<Operator> (@$, $1); }
	| "="	{ $$ = make_node<Operator> (@$, $1); }
	| "+="	{ $$ = make_node<Operator> (@$, $1); }
	| "-="	{ $$ = make_node<Operator> (@$, $1); }
	| "*="	{ $$ = make_node<Operator> (@$, $1); }
	| "/="	{ $$ = make_node<Operator> (@$, $1); }
	| "%="	{ $$ = make_node<Operator> (@$, $1); }
	| "<<="	{ $$ = make_node<Operator> (@$, $1); }
	| ">>="	{ $$ = make_node<Operator> (@$, $1); }
	| "&="	{ $$ = make_node<Operator> (@$, $1); }
	| "^="	{ $$ = make_node<Operator> (@$, $1); }
	| "|="	{ $$ = make_node<Operator> (@$, $1); }
	| "=="	{ $$ = make_node<Operator> (@$, $1); }
	| "!="	{ $$ = make_node<Operator> (@$, $1); }
	| "<"	{ $$ = make_node<Operator> (@$, $1); }
	| ">"	{ $$ = make_node<Operator> (@$, $1); }
	| "<="	{ $$ = make_node<Operator> (@$, $1); }
	| ">="	{ $$ = make_node<Operator> (@$, $1); }
	| "&&"	{ $$ = make_node<Operator> (@$, $1); }
	| "||"	{ $$ = make_node<Operator> (@$, $1); }
	| "->"	{ $$ = make_node<Operator> (@$, $1); }
	| "->*"	{ $$ = make_node<Operator> (@$, $1); }
	| "[" "]"	{ $$ = make_node<Operator> (@$, $1, $2); }
	| "(" ")"	{ $$ = make_node<Operator> (@$, $1, $2); }
	| ","	{ $$ = make_node<Operator> (@$, $1); }
	;

TemplateDeclaration
	: TemplatePreamble FunctionDefinition	{ $$ = make_node<TemplateDeclaration> (@$, $1, $2); }
	| TemplatePreamble SimpleDeclaration %merge<merge>	{ $$ = make_node<TemplateDeclaration> (@$, $1, $2); }
	| TemplatePreamble TemplateDeclaration	{ $$ = make_node<TemplateDeclaration> (@$, $1, $2); }
	| TemplatePreamble CDtorProtoDecl %merge<merge>	{ $$ = make_node<TemplateDeclaration> (@$, $1, $2); }
	;

TemplatePreamble
	: "template" "<" TemplateParameterList ">"	{ $$ = make_node<TemplatePreamble> (@$, $1, $2, $3, $4); }
	| "export" "template" "<" TemplateParameterList ">"	{ $$ = make_node<TemplatePreamble> (@$, $1, $2, $3, $4, $5); }
	| "template" "<" ">"	{ $$ = make_node<TemplatePreamble> (@$, $1, $2, $3); }
	| "export" "template" "<" ">"	{ $$ = make_node<TemplatePreamble> (@$, $1, $2, $3, $4); }
	;

TemplateParameterList
	: ClassOrTypename IdentifierOpt DefaultTypeOpt TemplateParameterListContinuation %merge<merge>	{ $$ = make_node<TemplateParameterList> (@$, $1, $2, $3, $4); }
	| ParameterDeclaration TemplateParameterListContinuation %merge<merge>	{ $$ = make_node<TemplateParameterList> (@$, $1, $2); }
	| "template" "<" TemplateParameterList ">" "class" IdentifierOpt DefaultTemplateOpt TemplateParameterListContinuation	{ $$ = make_node<TemplateParameterList> (@$, $1, $2, $3, $4, $5, $6, $7, $8); }
	;

TemplateParameterListContinuation
	:	{ $$ = make_node<TemplateParameterListContinuation> (@$); }
	| "," TemplateParameterList	{ $$ = make_node<TemplateParameterListContinuation> (@$, $1, $2); }
	;

ClassOrTypename
	: "class"	{ $$ = make_node<ClassOrTypename> (@$, $1); }
	| "typename"	{ $$ = make_node<ClassOrTypename> (@$, $1); }
	;

DefaultTypeOpt
	:	{ $$ = make_node<DefaultTypeOpt> (@$); }
	| "=" TypeId	{ $$ = make_node<DefaultTypeOpt> (@$, $1, $2); }
	;

DefaultTemplateOpt
	:	{ $$ = make_node<DefaultTemplateOpt> (@$); }
	| "=" IdExpression	{ $$ = make_node<DefaultTemplateOpt> (@$, $1, $2); }
	;

TemplateArgumentListOpt
	:	{ $$ = make_node<TemplateArgumentListOpt> (@$); }
	| TemplateArgumentList	{ $$ = make_node<TemplateArgumentListOpt> (@$, $1); }
	;

TemplateId
	: Identifier "<" TemplateArgumentListOpt ">"	{ $$ = make_node<TemplateId> (@$, $1, $2, $3, $4); }
	| OperatorFunctionId "<" TemplateArgumentListOpt ">"	{ $$ = make_node<TemplateId> (@$, $1, $2, $3, $4); }
	;

TemplateArgumentList
	: TemplateArgument	{ $$ = make_node<TemplateArgumentList> (@$, $1); }
	;

TemplateArgumentListTailOpt
	:	{ $$ = make_node<TemplateArgumentListTailOpt> (@$); }
	| "," TemplateArgument	{ $$ = make_node<TemplateArgumentListTailOpt> (@$, $1, $2); }
	;

TemplateArgument
	: TypeId TemplateArgumentListTailOpt %merge<merge>	{ $$ = make_node<TemplateArgument> (@$, $1, $2); }
	| AssignmentExpression TemplateArgumentListTailOpt %merge<merge>	{ $$ = make_node<TemplateArgument> (@$, $1, $2); }
	;

ExplicitInstantiation
	: "template" BlockDeclaration	{ $$ = make_node<ExplicitInstantiation> (@$, $1, $2); }
	| "inline" "template" BlockDeclaration	{ $$ = make_node<ExplicitInstantiation> (@$, $1, $2, $3); }
	;

TryBlock
	: "try" CompoundStatement HandlerSeq	{ $$ = make_node<TryBlock> (@$, $1, $2, $3); }
	;

HandlerSeq
	: Handler	{ $$ = make_node<HandlerSeq> (@$, $1); }
	| Handler HandlerSeq	{ $$ = make_node<HandlerSeq> (@$, $1, $2); }
	;

Handler
	: "catch" "(" HandlerParameter ")" CompoundStatement	{ $$ = make_node<Handler> (@$, $1, $2, $3, $4, $5); }
	| "catch" "(" "..." ")" CompoundStatement	{ $$ = make_node<Handler> (@$, $1, $2, $3, $4, $5); }
	;

HandlerParameter
	: TypeSpecifier UnqualifiedDeclarator	{ $$ = make_node<HandlerParameter> (@$, $1, $2); }
	| TypeSpecifier AbstractDeclaratorOpt	{ $$ = make_node<HandlerParameter> (@$, $1, $2); }
	;

UnqualifiedDeclarator
	: Declarator	{ $$ = make_node<UnqualifiedDeclarator> (@$, $1); }
	;

ThrowExpression
	: "throw"	{ $$ = make_node<ThrowExpression> (@$, $1); }
	| "throw" AssignmentExpression	{ $$ = make_node<ThrowExpression> (@$, $1, $2); }
	;

ExceptionSpecificationOpt
	:	{ $$ = make_node<ExceptionSpecificationOpt> (@$); }
	| "throw" "(" ")"	{ $$ = make_node<ExceptionSpecificationOpt> (@$, $1, $2, $3); }
	| "throw" "(" TypeIdList ")"	{ $$ = make_node<ExceptionSpecificationOpt> (@$, $1, $2, $3, $4); }
	;

TypeIdList
	: TypeId	{ $$ = make_node<TypeIdList> (@$, $1); }
	| TypeId "," TypeIdList	{ $$ = make_node<TypeIdList> (@$, $1, $2, $3); }
	;

NamespaceDefinition
	: "inline" "namespace" IdentifierOpt "{" TranslationUnit "}"	{ $$ = make_node<NamespaceDefinition> (@$, $1, $2, $3, $4, $5, $6); }
	| "namespace" IdentifierOpt "{" TranslationUnit "}"	{ $$ = make_node<NamespaceDefinition> (@$, $1, $2, $3, $4, $5); }
	;

NamespaceDecl
	: "namespace" Identifier "=" IdExpression ";"	{ $$ = make_node<NamespaceDecl> (@$, $1, $2, $3, $4, $5); }
	| "using" IdExpression ";"	{ $$ = make_node<NamespaceDecl> (@$, $1, $2, $3); }
	| "using" "namespace" IdExpression ";"	{ $$ = make_node<NamespaceDecl> (@$, $1, $2, $3, $4); }
	;

/****************************************************************************
 *
 * GNU Extensions
 *
 ****************************************************************************/

PrimaryExpression
	: "(" CompoundStatement ")" %dprec 2	{ $$ = make_node<PrimaryExpression> (@$, $1, $2, $3); }
	;

PrimaryExpression
	: "(" TypeId ")" CompoundInitializer	{ $$ = make_node<PrimaryExpression> (@$, $1, $2, $3, $4); }
	;

AssignmentExpression
	: CompoundInitializer	{ $$ = make_node<AssignmentExpression> (@$, $1); }
	;

PreprocString
	: "__func__"	{ $$ = make_node<PreprocString> (@$, $1); }
	| "__FUNCTION__"	{ $$ = make_node<PreprocString> (@$, $1); }
	| "__PRETTY_FUNCTION__"	{ $$ = make_node<PreprocString> (@$, $1); }
	;

PostfixExpression
	: "alignof" "(" TypeId ")" %merge<merge>	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4); }
	| "alignof" "(" Expression ")" %merge<merge>	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4); }
	| "__builtin_constant_p" ParenthesizedExpression	{ $$ = make_node<PostfixExpression> (@$, $1, $2); }
	| "__builtin_offsetof" "(" TypeId "," NamesAfterDot ")"	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4, $5, $6); }
	| "__builtin_expect" "(" Expression "," Expression ")"	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4, $5, $6); }
	| "__builtin_va_arg" "(" Expression "," TypeId ")"	{ $$ = make_node<PostfixExpression> (@$, $1, $2, $3, $4, $5, $6); }
	;

NamesAfterDot
	: NameAfterDot	{ $$ = make_node<NamesAfterDot> (@$, $1); }
	| NamesAfterDot "." NameAfterDot	{ $$ = make_node<NamesAfterDot> (@$, $1, $2, $3); }
	| NamesAfterDot "[" Expression "]"	{ $$ = make_node<NamesAfterDot> (@$, $1, $2, $3, $4); }
	;

ParenthesizedExpression
	: "(" Expression ")"	{ $$ = make_node<ParenthesizedExpression> (@$, $1, $2, $3); }
	;

ConditionalExpression
	: BinaryExpression "?" ":" AssignmentExpression	{ $$ = make_node<ConditionalExpression> (@$, $1, $2, $3, $4); }
	;

Statement
	: FunctionDefinition	{ $$ = make_node<Statement> (@$, $1); }
	;

ExplicitInstantiation
	: "extern" "template" BlockDeclaration	{ $$ = make_node<ExplicitInstantiation> (@$, $1, $2, $3); }
	;

Statement
	: "case" ConstantExpression "..." ConstantExpression ":" Statement	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4, $5, $6); }
	;

CompoundStatement
	: CompoundStmtHelper LabeledEmptyStatementList "}"	{ $$ = make_node<CompoundStatement> (@$, $1, $2, $3); }
	;

LabeledEmptyStatementList
	: LabelAndColon LabeledEmptyStatementListOpt	{ $$ = make_node<LabeledEmptyStatementList> (@$, $1, $2); }
	| "case" ConstantExpression ":" LabeledEmptyStatementListOpt	{ $$ = make_node<LabeledEmptyStatementList> (@$, $1, $2, $3, $4); }
	| "case" ConstantExpression "..." ConstantExpression ":" LabeledEmptyStatementListOpt	{ $$ = make_node<LabeledEmptyStatementList> (@$, $1, $2, $3, $4, $5, $6); }
	| "default" ":" LabeledEmptyStatementListOpt	{ $$ = make_node<LabeledEmptyStatementList> (@$, $1, $2, $3); }
	;

LabeledEmptyStatementListOpt
	:	{ $$ = make_node<LabeledEmptyStatementListOpt> (@$); }
	| LabeledEmptyStatementList	{ $$ = make_node<LabeledEmptyStatementListOpt> (@$, $1); }
	;

DeclSpecifier
	: TypeofTypeSpecifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2); }
	| UberModifierSeq TypeofTypeSpecifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3); }
	;

TypeSpecifier
	: TypeofTypeSpecifier UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2); }
	| UberCVQualifierSeq TypeofTypeSpecifier UberCVQualifierSeqOpt	{ $$ = make_node<TypeSpecifier> (@$, $1, $2, $3); }
	;

TypeofTypeSpecifier
	: TypeofExpr %merge<merge>	{ $$ = make_node<TypeofTypeSpecifier> (@$, $1); }
	| TypeofType %merge<merge>	{ $$ = make_node<TypeofTypeSpecifier> (@$, $1); }
	;

TypeofExpr
	: "typeof" "(" Expression ")"	{ $$ = make_node<TypeofExpr> (@$, $1, $2, $3, $4); }
	;

TypeofType
	: "typeof" "(" TypeId ")"	{ $$ = make_node<TypeofType> (@$, $1, $2, $3, $4); }
	;

BinExp_mid
	: BinExp_mid "<?" BinExp_high	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	| BinExp_mid ">?" BinExp_high	{ $$ = make_node<BinExp_mid> (@$, $1, $2, $3); }
	;

Operator
	: "<?"	{ $$ = make_node<Operator> (@$, $1); }
	| ">?"	{ $$ = make_node<Operator> (@$, $1); }
	;

BracketedWordOpt
	:	{ $$ = make_node<BracketedWordOpt> (@$); }
	| "[" Identifier "]"	{ $$ = make_node<BracketedWordOpt> (@$, $1, $2, $3); }
	;

ParenthesizedExpressionOpt
	:	{ $$ = make_node<ParenthesizedExpressionOpt> (@$); }
	| "(" Expression ")"	{ $$ = make_node<ParenthesizedExpressionOpt> (@$, $1, $2, $3); }
	;

OpConstraint
	: BracketedWordOpt StringLiteral ParenthesizedExpressionOpt	{ $$ = make_node<OpConstraint> (@$, $1, $2, $3); }
	;

OpConstraintList
	:	{ $$ = make_node<OpConstraintList> (@$); }
	| OpConstraint	{ $$ = make_node<OpConstraintList> (@$, $1); }
	| OpConstraint "," OpConstraintList	{ $$ = make_node<OpConstraintList> (@$, $1, $2, $3); }
	;

OpConstraints
	:	{ $$ = make_node<OpConstraints> (@$); }
	| NonemptyOpConstraints	{ $$ = make_node<OpConstraints> (@$, $1); }
	;

NonemptyOpConstraints
	: OpConstraints ":" OpConstraintList	{ $$ = make_node<NonemptyOpConstraints> (@$, $1, $2, $3); }
	| OpConstraints "::" OpConstraintList	{ $$ = make_node<NonemptyOpConstraints> (@$, $1, $2, $3); }
	;

AsmDefinition
	: "asm" CVQualifierSeq "(" StringLiteral ")" ";"	{ $$ = make_node<AsmDefinition> (@$, $1, $2, $3, $4, $5, $6); }
	| "asm" CVQualifierSeq "(" StringLiteral NonemptyOpConstraints ")" ";"	{ $$ = make_node<AsmDefinition> (@$, $1, $2, $3, $4, $5, $6, $7); }
	| "asm" "(" StringLiteral NonemptyOpConstraints ")" ";"	{ $$ = make_node<AsmDefinition> (@$, $1, $2, $3, $4, $5, $6); }
	;

Declarator
	: DirectDeclarator "asm" "(" StringLiteral ")"	{ $$ = make_node<Declarator> (@$, $1, $2, $3, $4, $5); }
	| DirectDeclarator "asm" "(" StringLiteral ")" AttributeSpecifierList	{ $$ = make_node<Declarator> (@$, $1, $2, $3, $4, $5, $6); }
	;

InitializerClause
	: Identifier ":" SimpleInitializerClause	{ $$ = make_node<InitializerClause> (@$, $1, $2, $3); }
	| DesignatorList "=" SimpleInitializerClause	{ $$ = make_node<InitializerClause> (@$, $1, $2, $3); }
	| DesignatorList SimpleInitializerClause	{ $$ = make_node<InitializerClause> (@$, $1, $2); }
	;

DesignatorList
	: Designator	{ $$ = make_node<DesignatorList> (@$, $1); }
	| Designator DesignatorList	{ $$ = make_node<DesignatorList> (@$, $1, $2); }
	;

Designator
	: "." Identifier	{ $$ = make_node<Designator> (@$, $1, $2); }
	| "[" ConstantExpression "]"	{ $$ = make_node<Designator> (@$, $1, $2, $3); }
	| "[" ConstantExpression "..." ConstantExpression "]"	{ $$ = make_node<Designator> (@$, $1, $2, $3, $4, $5); }
	;

CVQualifier
	: "restrict"	{ $$ = make_node<CVQualifier> (@$, $1); }
	;

UberModifier
	: "restrict"	{ $$ = make_node<UberModifier> (@$, $1); }
	;

UberCVQualifier
	: "restrict"	{ $$ = make_node<UberCVQualifier> (@$, $1); }
	;

DirectDeclarator
	: DirectDeclarator "[" CVQualifierSeq "]"	{ $$ = make_node<DirectDeclarator> (@$, $1, $2, $3, $4); }
	;

DirectAbstractDeclarator
	: DirectAbstractDeclaratorOpt "[" CVQualifierSeq "]"	{ $$ = make_node<DirectAbstractDeclarator> (@$, $1, $2, $3, $4); }
	;

DeclSpecifier
	: PQTypeName BuggyGccTypeModifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3); }
	| UberModifierSeq PQTypeName BuggyGccTypeModifier UberModifierSeqOpt	{ $$ = make_node<DeclSpecifier> (@$, $1, $2, $3, $4); }
	;

BuggyGccTypeModifier
	: "long"	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1); }
	| "short"	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1); }
	| "signed"	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1); }
	| "unsigned"	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1); }
	| "long" BuggyGccTypeModifier	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1, $2); }
	| "short" BuggyGccTypeModifier	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1, $2); }
	| "signed" BuggyGccTypeModifier	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1, $2); }
	| "unsigned" BuggyGccTypeModifier	{ $$ = make_node<BuggyGccTypeModifier> (@$, $1, $2); }
	;

UnaryExpression
	: "&&" Identifier	{ $$ = make_node<UnaryExpression> (@$, $1, $2); }
	;

Statement
	: "goto" "*" Expression ";"	{ $$ = make_node<Statement> (@$, $1, $2, $3, $4); }
	;

UberTypeKeyword
	: "_Complex"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	| "_Imaginary"	{ $$ = make_node<UberTypeKeyword> (@$, $1); }
	;

PrimaryExpression
	: "__real__" PrimaryExpression	{ $$ = make_node<PrimaryExpression> (@$, $1, $2); }
	| "__imag__" PrimaryExpression	{ $$ = make_node<PrimaryExpression> (@$, $1, $2); }
	;

LabelAndColon
	: Identifier ":" AttributeSpecifierList %prec TK_PREFER_REDUCE	{ $$ = make_node<LabelAndColon> (@$, $1, $2, $3); }
	;

ElaboratedTypeSpecifier
	: ClassKey AttributeSpecifier PQTypeName	{ $$ = make_node<ElaboratedTypeSpecifier> (@$, $1, $2, $3); }
	;

ClassSpecifier
	: ClassKey AttributeSpecifierList ClassHeadNameOpt BaseClauseOpt "{" MemberDeclarationSeqOpt "}"	{ $$ = make_node<ClassSpecifier> (@$, $1, $2, $3, $4, $5, $6, $7); }
	;

ElaboratedTypeSpecifier
	: "enum" AttributeSpecifierList PQTypeName	{ $$ = make_node<ElaboratedTypeSpecifier> (@$, $1, $2, $3); }
	;

EnumSpecifier
	: "enum" AttributeSpecifierList "{" EnumeratorListOpt "}"	{ $$ = make_node<EnumSpecifier> (@$, $1, $2, $3, $4, $5); }
	| "enum" AttributeSpecifierList PQTypeName "{" EnumeratorListOpt "}"	{ $$ = make_node<EnumSpecifier> (@$, $1, $2, $3, $4, $5, $6); }
	;

UberModifier
	: AttributeSpecifier	{ $$ = make_node<UberModifier> (@$, $1); }
	;

UberCVQualifier
	: AttributeSpecifier	{ $$ = make_node<UberCVQualifier> (@$, $1); }
	;

InitDeclaratorList
	: InitDeclarator "," AttributeSpecifierList InitDeclaratorList	{ $$ = make_node<InitDeclaratorList> (@$, $1, $2, $3, $4); }
	;

CVQualAttrSeqOpt
	:	{ $$ = make_node<CVQualAttrSeqOpt> (@$); }
	| CVQualAttrSeq	{ $$ = make_node<CVQualAttrSeqOpt> (@$, $1); }
	;

CVQualAttrSeq
	: CVQualAttr	{ $$ = make_node<CVQualAttrSeq> (@$, $1); }
	| CVQualAttr CVQualAttrSeq	{ $$ = make_node<CVQualAttrSeq> (@$, $1, $2); }
	;

CVQualAttr
	: CVQualifier	{ $$ = make_node<CVQualAttr> (@$, $1); }
	| AttributeSpecifier	{ $$ = make_node<CVQualAttr> (@$, $1); }
	;

Declarator
	: "*" CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt Declarator	{ $$ = make_node<Declarator> (@$, $1, $2, $3, $4, $5); }
	| DirectDeclarator AttributeSpecifierList	{ $$ = make_node<Declarator> (@$, $1, $2); }
	;

DirectDeclarator
	: "(" AttributeSpecifierList Declarator ")"	{ $$ = make_node<DirectDeclarator> (@$, $1, $2, $3, $4); }
	;

AbstractDeclarator
	: "*" CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt AbstractDeclaratorOpt	{ $$ = make_node<AbstractDeclarator> (@$, $1, $2, $3, $4, $5); }
	| DirectAbstractDeclarator AttributeSpecifierList	{ $$ = make_node<AbstractDeclarator> (@$, $1, $2); }
	;

DirectAbstractDeclarator
	: "(" AttributeSpecifierList AbstractDeclarator ")"	{ $$ = make_node<DirectAbstractDeclarator> (@$, $1, $2, $3, $4); }
	;

MemberDeclarator
	: IdentifierOpt ":" ConstantExpression AttributeSpecifierList	{ $$ = make_node<MemberDeclarator> (@$, $1, $2, $3, $4); }
	;

CDtorModifier
	: AttributeSpecifier	{ $$ = make_node<CDtorModifier> (@$, $1); }
	;

NamespaceDefinition
	: "namespace" IdentifierOpt AttributeSpecifierList "{" TranslationUnit "}"	{ $$ = make_node<NamespaceDefinition> (@$, $1, $2, $3, $4, $5, $6); }
	;

AttributeWord
	: TK_NAME	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "asm"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "auto"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "break"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "bool"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "case"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "catch"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "cdecl"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "char"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "class"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "const"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "const_cast"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "continue"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "default"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "delete"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "do"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "double"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "dynamic_cast"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "else"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "enum"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "explicit"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "export"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "extern"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "false"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "float"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "for"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "friend"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "goto"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "if"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "inline"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "int"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "long"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "mutable"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "namespace"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "new"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "operator"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "pascal"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "private"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "protected"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "public"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "register"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "reinterpret_cast"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "return"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "short"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "signed"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "sizeof"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "static"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "static_cast"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "struct"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "switch"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "template"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "this"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "throw"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "true"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "try"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "typedef"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "typeid"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "typename"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "union"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "unsigned"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "using"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "virtual"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "void"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "volatile"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "wchar_t"	{ $$ = make_node<AttributeWord> (@$, $1); }
	| "while"	{ $$ = make_node<AttributeWord> (@$, $1); }
	;

CommaSepExpressionListOpt
	:	{ $$ = make_node<CommaSepExpressionListOpt> (@$); }
	| ExpressionList	{ $$ = make_node<CommaSepExpressionListOpt> (@$, $1); }
	;

AttributeParameters
	: CommaSepExpressionListOpt	{ $$ = make_node<AttributeParameters> (@$, $1); }
	;

Attribute
	:	{ $$ = make_node<Attribute> (@$); }
	| AttributeWord	{ $$ = make_node<Attribute> (@$, $1); }
	| AttributeWord "(" AttributeParameters ")"	{ $$ = make_node<Attribute> (@$, $1, $2, $3, $4); }
	;

AttributeList
	: Attribute	{ $$ = make_node<AttributeList> (@$, $1); }
	| Attribute "," AttributeList	{ $$ = make_node<AttributeList> (@$, $1, $2, $3); }
	;

AttributeSpecifier
	: "__attribute__" "(" "(" AttributeList ")" ")"	{ $$ = make_node<AttributeSpecifier> (@$, $1, $2, $3, $4, $5, $6); }
	;

AttributeSpecifierList
	: AttributeSpecifier	{ $$ = make_node<AttributeSpecifierList> (@$, $1); }
	| AttributeSpecifier AttributeSpecifierList	{ $$ = make_node<AttributeSpecifierList> (@$, $1, $2); }
	;

NamespaceDecl
	: "using" "namespace" IdExpression AttributeSpecifierList ";"	{ $$ = make_node<NamespaceDecl> (@$, $1, $2, $3, $4, $5); }
	;

%%

char const *
tokname (yySymbol yytoken)
{
  return yytname[yytoken - 255];
}
