%{
#include <stdio.h>
#include <stdlib.h>

#include "pascal.h"

extern int yylex();
extern int yyparse();
void yyerror(char const *);
%}

%union {
	int ival;
	float fval;
	char* identifier;
	Program* program;
	IdentifierList* identifier_list;
	Declarations* declarations;
	Type* type;
	ArrayType* array_type;
	SubDeclarations* sub_declarations;
	SubprogDeclaration* subprog_declaration;
	SubprogramHead* subprogram_head;
	ParameterList* parameter_list;
	StatementList* statement_list;
	Statement* statement;
	Variable* variable;
	ProcStatement* proc_statement;
	ExpressionList* expression_list;
	Expression* expression;
	SimpleExpression* simple_expression;
	Term* term;
	Factor* factor;
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

%type<program> program
%type<declarations> declarations
%type<sub_declarations> subdeclarations
%type<statement_list> compoundstatement statementlist
%type<identifier_list> identifierlist
%type<subprog_declaration> subprogdeclaration
%type<type> type
%type<ival> standardtype relop addop mulop sign
%type<ival> INTEGER REAL
%type<ival> EQUALS NOTEQUALS LESS LESSEQUALS GREAT GREATEQUALS 
%type<ival> PLUS MINUS OR ASTERISK SOLIDUS DIV MODULUS AND
%type<array_type> arraytype
%type<subprogram_head> subprogramhead
%type<parameter_list> parameterlist arguments
%type<statement> statement elseclause
%type<variable> variable
%type<expression> expression
%type<proc_statement> procstatement
%type<expression_list> expressionlist
%type<factor> factor
%type<simple_expression> simpleexpression
%type<term> term

%start program

%%
program: PROGRAM IDENTIFIER SEMICOLON declarations subdeclarations compoundstatement DOT { 
			printf("program\n");
			Program* program = (Program*)malloc(sizeof(Program));
			program->identifier = $2;
			program->declarations = $4;
			program->sub_declarations = $5;
			program->compound_statement = $6;
			$$ = program;
		}
;

identifierlist: IDENTIFIER {
					printf("identifierlist-1\n");
					IdentifierList* identifier_list = (IdentifierList*)malloc(sizeof(identifier_list));
					identifier_list->identifier = $1;
					identifier_list->next = NULL;
					$$ = identifier_list;
				}
			  | identifierlist COMMA IDENTIFIER {
					printf("identifierlist-2\n");
					IdentifierList* identifier_list = (IdentifierList*)malloc(sizeof(identifier_list));
					identifier_list->identifier = $3;
					identifier_list->next = $1;
					$$ = identifier_list;
				}
;

declarations: %empty { $$ = NULL; printf("declarations-1\n"); }
			| declarations VAR identifierlist COLON type SEMICOLON {
					printf("declarations-2\n");
					Declarations* declarations = (Declarations*)malloc(sizeof(Declarations));
					declarations->identifier_list = $3;
					declarations->type = $5;
					declarations->next = $1;
					$$ = declarations;
			}
;

type: standardtype {
			printf("type-1\n");
			Type* type = (Type*)malloc(sizeof(Type));
			type->node_type = 0;
			type->type.standard_type = $1;
			$$ = type;
		}
	| arraytype {
			printf("type-2\n");
			Type* type = (Type*)malloc(sizeof(Type));
			type->node_type = 1;
			type->type.array_type = $1;
			$$ = type;
		}
;

standardtype: INTEGER { $$ = $1; printf("standardtype-1\n"); }
			| REAL { $$ = $1; printf("standardtype-2\n"); }
;

arraytype: ARRAY LSQUARE INTNUM DOUBLEDOT INTNUM RSQUARE OF standardtype {
				printf("arraytype\n");
				ArrayType* array_type =	(ArrayType*)malloc(sizeof(ArrayType));
				array_type->from = $3;
				array_type->to = $5;
				array_type->standard_type = $8;
				$$ = array_type;
			}
;

subdeclarations: %empty { $$ = NULL; printf("subdeclarations-1\n"); }
			   | subprogdeclaration subdeclarations {
						printf("subdeclarations-2\n");
						SubDeclarations* sub_declarations = (SubDeclarations*)malloc(sizeof(SubDeclarations));
						sub_declarations->subprog_declaration = $1;
						sub_declarations->next = $2;
						$$ = sub_declarations;
					}
;

subprogdeclaration: subprogramhead declarations compoundstatement {
							printf("subprogdeclarations\n");
							SubprogDeclaration* subprog_declaration = (SubprogDeclaration*)malloc(sizeof(SubprogDeclaration));
							subprog_declaration->subprogram_head = $1;
							subprog_declaration->declarations = $2;
							subprog_declaration->compound_statement = $3;
							$$ = subprog_declaration;
						}
;

subprogramhead: FUNCTION IDENTIFIER arguments COLON standardtype SEMICOLON {
						printf("subprogramhead-1\n");
						SubprogramHead* subprogram_head = (SubprogramHead*)malloc(sizeof(SubprogramHead));
						subprogram_head->node_type = 0;
						subprogram_head->subprogram_head.function = (Function*)malloc(sizeof(Function));
						subprogram_head->subprogram_head.function->identifier = $2;
						subprogram_head->subprogram_head.function->arguments = $3;
						subprogram_head->subprogram_head.function->standard_type = $5;
						$$ = subprogram_head;
					}
			  | PROCEDURE IDENTIFIER arguments SEMICOLON {
						printf("subprogramhead-2\n");
						SubprogramHead* subprogram_head = (SubprogramHead*)malloc(sizeof(SubprogramHead));
						subprogram_head->node_type = 1;
						subprogram_head->subprogram_head.procedure = (Procedure*)malloc(sizeof(Procedure));
						subprogram_head->subprogram_head.procedure->identifier = $2;
						subprogram_head->subprogram_head.procedure->arguments = $3;
						$$ = subprogram_head;
					}
;

arguments: %empty { $$ = NULL; printf("arguments-1\n"); }
		 | LPAREN parameterlist RPAREN { $$ = $2; printf("arguments-2\n");}
;

parameterlist: identifierlist COLON type {
					printf("parameterlist-1\n");
					ParameterList* parameter_list = (ParameterList*)malloc(sizeof(ParameterList));
					parameter_list->identifier_list = $1;
					parameter_list->type = $3;
					parameter_list->next = NULL;
					$$ = parameter_list;
				}
			 | parameterlist SEMICOLON identifierlist COLON type {
					printf("parameterlist-2\n");
					ParameterList* parameter_list = (ParameterList*)malloc(sizeof(ParameterList));
					parameter_list->identifier_list = $3;
					parameter_list->type = $5;
					parameter_list->next = $1;
					$$ = parameter_list;
				}
;

compoundstatement: BEG statementlist END { $$ = $2; printf("compoundstatement\n"); }
;

statementlist: %empty { $$ = NULL; printf("statementlist-1\n"); }
			 | statement {
					printf("statementlist-2\n");
					StatementList* statement_list = (StatementList*)malloc(sizeof(StatementList));
					statement_list->statement = $1;
					statement_list->next = NULL;
					$$ = statement_list;
				}
			 | statement SEMICOLON statementlist {
					printf("statementlist-3\n");
					StatementList* statement_list = (StatementList*)malloc(sizeof(StatementList));
					statement_list->statement = $1;
					statement_list->next = $3;
					$$ = statement_list;
				}
;

statement: variable ASSIGN expression {
				printf("statement-1\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 0;
				statement->statement.assignment = (Assignment*)malloc(sizeof(Assignment));
				statement->statement.assignment->variable = $1;
				statement->statement.assignment->expression = $3;
				$$ = statement;
			}
		 | procstatement {
				printf("statement-2\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 1;
				statement->statement.proc_statement = $1;
				$$ = statement;
			}
		 | compoundstatement {
				printf("statement-3\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 2;
				statement->statement.compound_statement = $1;
				$$ = statement;
			}
		 | IF expression THEN statement elseclause {
				printf("if\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 3;
				statement->statement.if_then = (IfThen*)malloc(sizeof(IfThen));
				statement->statement.if_then->expression = $2;
				statement->statement.if_then->next = $4;
				statement->statement.if_then->else_clause = $5;
				$$ = statement;
			}
		 | WHILE expression DO statement {
				printf("while\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->statement.while_do = (WhileDo*)malloc(sizeof(WhileDo));
				statement->statement.while_do->expression = $2;
				statement->statement.while_do->next = $4;
				$$ = statement;
			}
		 | FOR IDENTIFIER ASSIGN expression TO expression DO statement {
				printf("for\n");
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->statement.for_to = (ForTo*)malloc(sizeof(ForTo));
				statement->statement.for_to->identifier = $2;
				statement->statement.for_to->expression1 = $4;
				statement->statement.for_to->expression2 = $6;
				statement->statement.for_to->next = $8;
				$$ = statement;
			}
;

elseclause: %empty { $$ = NULL; printf("elseclause-1\n"); }
		  | ELSE statement { $$ = $2; printf("elseclause-2\n"); }
;

variable: IDENTIFIER {
				printf("variable-1\n");
				Variable* variable = (Variable*)malloc(sizeof(Variable));
				variable->node_type = 0;
				variable->variable.identifier = $1;
				$$ = variable;
			}
		| IDENTIFIER LSQUARE expression RSQUARE {
				printf("variable-2\n");
				Variable* variable = (Variable*)malloc(sizeof(Variable));
				variable->node_type = 1;
				variable->variable.expression = (VariableExpression*)malloc(sizeof(VariableExpression));
				variable->variable.expression->identifier = $1;
				variable->variable.expression->expression = $3;
				$$ = variable;
			}
;

procstatement: IDENTIFIER {
					printf("procstatement-1\n");
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 0;
					proc_statement->proc_statement.identifier = $1;
					$$ = proc_statement;
				}
			 | IDENTIFIER LPAREN expressionlist RPAREN {
					printf("procstatement-2\n");
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 1;
					proc_statement->proc_statement.expression_list = (ProcStatementExpressionList*)malloc(sizeof(ProcStatementExpressionList));
					proc_statement->proc_statement.expression_list->identifier = $1;
					proc_statement->proc_statement.expression_list->expression_list = $3;
					$$ = proc_statement;
				}
;

expressionlist: expression {
					printf("expressionlist-1\n");
					ExpressionList* expression_list = (ExpressionList*)malloc(sizeof(ExpressionList));
					expression_list->expression = $1;
					expression_list->next = NULL;
					$$ = expression_list;
				}
			  | expressionlist COMMA expression {
					printf("expressionlist-2\n");
					ExpressionList* expression_list = (ExpressionList*)malloc(sizeof(ExpressionList));
					expression_list->expression = $3;
					expression_list->next = $1;
					$$ = expression_list;
					/*$1->next = expression_list;
					$$ = expression_list;*/
				}
;

expression: simpleexpression {
				printf("expression-1\n");
				Expression* expression = (Expression*)malloc(sizeof(Expression));
				expression->node_type = 0;
				expression->expression.simple_expression = $1;
				$$ = expression;
			}
		  | simpleexpression relop expression {
				printf("expression-2\n");
				Expression* expression = (Expression*)malloc(sizeof(Expression));
				expression->node_type = 1;
				expression->expression.relation = (RelationalExpression*)malloc(sizeof(RelationalExpression));
				expression->expression.relation->simple_expression = $1;
				expression->expression.relation->relop = $2;
				expression->expression.relation->next = $3;
				$$ = expression;
			}
;

relop: EQUALS { $$ = $1; printf("=\n"); }
	 | NOTEQUALS { $$ = $1; printf("<>\n"); }
	 | LESS { $$ = $1; printf("<\n"); }
	 | LESSEQUALS { $$ = $1; printf("<=\n"); }
	 | GREAT { $$ = $1; printf(">\n"); }
	 | GREATEQUALS { $$ = $1; printf(">=\n"); }
;

simpleexpression: term {
						printf("simpleexpression-1\n");
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 0;
						simple_expression->simple_expression.term = $1;
						$$ = simple_expression;
					}
				| sign term {
						printf("simpleexpression-2\n");
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 1;
						simple_expression->simple_expression.signed_term = (SignedTerm*)malloc(sizeof(SignedTerm));
						simple_expression->simple_expression.signed_term->sign = $1;
						simple_expression->simple_expression.signed_term->term = $2;
						$$ = simple_expression;
					}
				| simpleexpression addop term {
						printf("simpleexpression-3\n");
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 2;
						simple_expression->simple_expression.addition = (Addition*)malloc(sizeof(Addition));
						simple_expression->simple_expression.addition->addop = $2;
						simple_expression->simple_expression.addition->term = $3;
						simple_expression->simple_expression.addition->next = $1;
						$$ = simple_expression;
					}
;

addop: PLUS { $$ = $1; printf("+\n"); }
	 | MINUS { $$ = $1; printf("-\n"); }
	 | OR { $$ = $1; printf("or\n"); }
;

term: factor {
			printf("term-1\n");
			Term* term = (Term*)malloc(sizeof(Term));
			term->node_type = 0;
			term->term.factor = $1;
			$$ = term;
		}
	| term mulop factor {
			printf("term-2\n");
			Term* term = (Term*)malloc(sizeof(Term));
			term->node_type = 1;
			term->term.multiplication = (Multiplication*)malloc(sizeof(Multiplication));
			term->term.multiplication->mulop = $2;
			term->term.multiplication->factor = $3;
			term->term.multiplication->next = $1;
			$$ = term;
		}
;

mulop: ASTERISK { $$ = $1; printf("*\n"); }
	 | SOLIDUS { $$ = $1; printf("/\n"); }
	 | DIV { $$ = $1; printf("div\n"); }
	 | MODULUS { $$ = $1; printf("modulus\n"); }
	 | AND { $$ = $1; printf("and\n"); }
;

factor: IDENTIFIER LPAREN expressionlist RPAREN {
			printf("factor-1\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 0;
			factor->factor.expression_list = (FactorExpressionList*)malloc(sizeof(FactorExpressionList));
			factor->factor.expression_list->identifier = $1;
			factor->factor.expression_list->expression_list = $3;
			$$ = factor;
		}
	  | variable {
			printf("factor-2\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 1;
			factor->factor.variable = $1;
			$$ = factor;
		}
	  | INTNUM {
			printf("factor-3\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 2;
			factor->factor.integer = $1;
			$$ = factor;
		}
	  | REALNUM {
			printf("factor-4\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 3;
			factor->factor.real = $1;
			$$ = factor;
		}
	  | LPAREN expression RPAREN {
			printf("factor-5\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 5;
			factor->factor.expression = $2;
			$$ = factor;
		}
	  | NOT factor {
			printf("factor-6\n");
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 6;
			factor->factor.factor = $2;
			$$ = factor;
		}
;

sign: PLUS { $$ = $1; printf("sign+\n"); }
	| MINUS { $$ = $1; printf("sign-\n"); }
;

%%

int main() {
	yyparse();
}

void yyerror(char const *s) {
	fprintf(stderr, "%s\n", s);
}
