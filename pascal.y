%{
#include <stdio.h>
#include <stdlib.h>

extern int yylex();
extern int yyparse();
void yyerror(char const *);
%}

%union {
	int ival;
	float fval;
	char* identifier;
} 

%token<ival> INTNUM
%token<fval> REALNUM
%token<identifier> IDENTIFIER
%token PROGRAM PROCEDURE FUNCTION ARRAY
%token INTEGER REAL
%token VAR OF
%token IF THEN ELSE
%token DO WHILE FOR TO
%token BEG END
%token AND OR NOT
%token DIV MODULUS
%token WRITELN WRITE READLN READ
%token PLUS MINUS ASTERISK SOLIDUS
%token EQUALS NOTEQUALS
%token LESS LESSEQUALS GREAT GREATEQUALS
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token ASSIGN
%token SEMICOLON COLON COMMA
%token DOT DOUBLEDOT

%left EQUALS NOTEQUALS LESS LESSEQUALS GREAT GREATEQUALS
%left PLUS MINUS OR
%left ASTERISK SOLIDUS DIV MODULUS AND
%left NOT

%type<identifier> identifierlist variable procstatement

%start program

%%
program: PROGRAM IDENTIFIER SEMICOLON declarations subdeclarations compoundstatement DOT
;

identifierlist: IDENTIFIER
			  | identifierlist COMMA IDENTIFIER
;

declarations: %empty
			| declarations VAR identifierlist COLON type SEMICOLON
;

type: standardtype
	| arraytype
;

standardtype: INTEGER
			| REAL
;

arraytype: ARRAY LSQUARE INTNUM DOUBLEDOT INTNUM RSQUARE OF standardtype
;

subdeclarations: %empty
			   | subprogdeclaration subdeclarations
;

subprogdeclaration: subprogramhead declarations compoundstatement
;

subprogramhead: FUNCTION IDENTIFIER arguments COLON standardtype SEMICOLON
			  | PROCEDURE IDENTIFIER arguments SEMICOLON
;

arguments: %empty
		 | LPAREN parameterlist RPAREN
;

parameterlist: identifierlist COLON type
			 | parameterlist SEMICOLON identifierlist COLON type
;

compoundstatement: BEG statementlist END
;

statementlist: %empty
			 | statement
			 | statement SEMICOLON statementlist
;

statement: variable ASSIGN expression
		 | procstatement
		 | compoundstatement
		 | IF expression THEN statement elseclause
		 | WHILE expression DO statement
		 | FOR IDENTIFIER ASSIGN expression TO expression DO statement
;

elseclause: %empty
		  | ELSE statement
;

variable: IDENTIFIER
		| IDENTIFIER LSQUARE expression RSQUARE
;

procstatement: IDENTIFIER
			 | IDENTIFIER LPAREN expressionlist RPAREN
;

expressionlist: expression
			  | expressionlist COMMA expression
;

expression: simpleexpression
		  | simpleexpression relop expression
;

relop: EQUALS
	 | NOTEQUALS
	 | LESS
	 | LESSEQUALS
	 | GREAT
	 | GREATEQUALS
;

simpleexpression: term
				| sign term
				| simpleexpression addop term
;

addop: PLUS
	 | MINUS
	 | OR
;

term: factor
	| term mulop factor
;

mulop: ASTERISK
	 | SOLIDUS
	 | DIV
	 | MODULUS
	 | AND
;

factor: IDENTIFIER LPAREN expressionlist RPAREN
	  | variable
	  | INTNUM
	  | REALNUM
	  | LPAREN expression RPAREN
	  | NOT factor
;

sign: PLUS
	| MINUS
;

%%

int main() {
	yyparse();
}

void yyerror(char const *s) {
	fprintf(stderr, "%s\n", s);
}
