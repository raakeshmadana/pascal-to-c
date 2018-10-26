%{
#include <stdio.h>
#include <stdlib.h>

#define YY_DECL int yylex()

#include "pascal.tab.h"
%}

%option noyywrap

delim			[ \t\n\r]
ws				{delim}+
letter			[A-Za-z]
digit			[0-9]
identifier		({letter}|_)({letter}|{digit}|_)*
realnum			{digit}+(\.{digit}+)?
integer			{digit}+

%%
"of"			{ return OF; }
"or"			{ return OR; }
"program"		{ return PROGRAM; }
"integer"		{ return INTEGER; }
"real"			{ return REAL; }
"do"			{ return DO; }
"else"			{ return ELSE; }
"end"			{ return END; }
"for"			{ return FOR; }
"if"			{ return IF; }
"array"			{ return ARRAY; }
"mod"			{ return MODULUS; }
"not"			{ return NOT; }
"procedure"		{ return PROCEDURE; }
"then"			{ return THEN; }
"var"			{ return VAR; }
"while"			{ return WHILE; }
"and"			{ return AND; }
"begin"			{ return BEG; }
"div"			{ return DIV; }
"function"		{ return FUNCTION; }
"to"			{ return TO; }
"writeln"		{ return WRITELN; }
"write"			{ return WRITE; }
"readln"		{ return READLN; }
"read"			{ return READ; }
"+"				{ return PLUS; }
"-"				{ return MINUS; }
"*"				{ return ASTERISK; }
"/"				{ return SOLIDUS; }
"<"				{ return LESS; }
"<="			{ return LESSEQUALS; }
"="				{ return EQUALS; }
"<>"			{ return NOTEQUALS; }
">"				{ return GREAT; }
">="			{ return GREATEQUALS; }
"."				{ return DOT; }
","				{ return COMMA; }
":"				{ return COLON; }
";"				{ return SEMICOLON; }
":="			{ return ASSIGN; }
".."			{ return DOUBLEDOT; }
"("				{ return LPAREN; }
")"				{ return RPAREN; }
"["				{ return LSQUARE; }
"]"				{ return RSQUARE; }
{ws}			// Ignore whitespace
{identifier}	{
					yylval.identifier = (char*)malloc(sizeof(yytext));
					strcpy(yylval.identifier, yytext);
					return IDENTIFIER;
				}
{integer}		{
					yylval.ival = atoi(yytext);
					return INTNUM;
				}
{realnum}		{
					yylval.fval = atof(yytext);
					return REALNUM;
				}
^\{.*\}$		// Ignore comments
%%
