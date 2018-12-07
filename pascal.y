%{
#include <stdio.h>
#include <stdlib.h>

#include "code-gen.h"

extern int yylex();
extern int yyparse();
extern int yylineno;
void yyerror(char const *);

int error = 0;
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
			Program* program = (Program*)malloc(sizeof(Program));
			program->identifier = $2;
			program->declarations = $4;
			program->sub_declarations = $5;
			program->compound_statement = $6;
			$$ = program;

			addProgramName($2);
			if (!error) {
				printSymbols();
				printTree(program);
			}
		}
;

identifierlist: IDENTIFIER {
					IdentifierList* identifier_list = (IdentifierList*)malloc(sizeof(identifier_list));
					identifier_list->identifier = $1;
					identifier_list->next = NULL;
					$$ = identifier_list;
				}
			  | identifierlist COMMA IDENTIFIER {
					IdentifierList* identifier_list = (IdentifierList*)malloc(sizeof(identifier_list));
					identifier_list->identifier = $3;
					identifier_list->next = NULL;
					IdentifierList* temp = $1;
					while(temp->next != NULL) {
						temp = temp->next;
					}
					temp->next = identifier_list;
					$$ = $1;
				}
;

declarations: %empty { $$ = NULL; }
			| declarations VAR identifierlist COLON type SEMICOLON {
					Declarations* declarations = (Declarations*)malloc(sizeof(Declarations));
					declarations->identifier_list = $3;
					declarations->type = $5;
					declarations->next = $1;
					$$ = declarations;

					IdentifierList* temp = $3;
					while(temp != NULL) {
						SymbolTable* s = NULL;
						s = isDeclared(temp->identifier);
						if (s) {
							printf("%d: %s Re-declared\n", yylineno, s->symbol);
							error = 1;
						} else {
							addVariable(temp->identifier, $5);
						}
						temp = temp->next;
					}
			}
;

type: standardtype {
			Type* type = (Type*)malloc(sizeof(Type));
			type->node_type = 0;
			type->type.standard_type = $1;
			$$ = type;
		}
	| arraytype {
			Type* type = (Type*)malloc(sizeof(Type));
			type->node_type = 1;
			type->type.array_type = $1;
			$$ = type;
		}
;

standardtype: INTEGER { $$ = 0; }
			| REAL { $$ = 1; }
;

arraytype: ARRAY LSQUARE INTNUM DOUBLEDOT INTNUM RSQUARE OF standardtype {
				ArrayType* array_type =	(ArrayType*)malloc(sizeof(ArrayType));
				array_type->from = $3;
				array_type->to = $5;
				array_type->standard_type = $8;
				$$ = array_type;
			}
;

subdeclarations: %empty { $$ = NULL; }
			   | subprogdeclaration subdeclarations {
						SubDeclarations* sub_declarations = (SubDeclarations*)malloc(sizeof(SubDeclarations));
						sub_declarations->subprog_declaration = $1;
						sub_declarations->next = $2;
						$$ = sub_declarations;
					}
;

subprogdeclaration: subprogramhead declarations compoundstatement {
							SubprogDeclaration* subprog_declaration = (SubprogDeclaration*)malloc(sizeof(SubprogDeclaration));
							subprog_declaration->subprogram_head = $1;
							subprog_declaration->declarations = $2;
							subprog_declaration->compound_statement = $3;
							$$ = subprog_declaration;
						}
;

subprogramhead: FUNCTION IDENTIFIER arguments COLON standardtype SEMICOLON {
						SubprogramHead* subprogram_head = (SubprogramHead*)malloc(sizeof(SubprogramHead));
						subprogram_head->node_type = 0;
						subprogram_head->subprogram_head.function_rule = (FunctionRule*)malloc(sizeof(FunctionRule));
						subprogram_head->subprogram_head.function_rule->identifier = $2;
						subprogram_head->subprogram_head.function_rule->arguments = $3;
						subprogram_head->subprogram_head.function_rule->standard_type = $5;
						$$ = subprogram_head;
						
						error = addFunction($2, $3, $5, yylineno);
					}
			  | PROCEDURE IDENTIFIER arguments SEMICOLON {
						SubprogramHead* subprogram_head = (SubprogramHead*)malloc(sizeof(SubprogramHead));
						subprogram_head->node_type = 1;
						subprogram_head->subprogram_head.procedure_rule = (ProcedureRule*)malloc(sizeof(ProcedureRule));
						subprogram_head->subprogram_head.procedure_rule->identifier = $2;
						subprogram_head->subprogram_head.procedure_rule->arguments = $3;
						$$ = subprogram_head;

						error = addProcedure($2, $3, yylineno);
					}
;

arguments: %empty { $$ = NULL; }
		 | LPAREN parameterlist RPAREN { $$ = $2; }
;

parameterlist: identifierlist COLON type {
					ParameterList* parameter_list = (ParameterList*)malloc(sizeof(ParameterList));
					parameter_list->identifier_list = $1;
					parameter_list->type = $3;
					parameter_list->next = NULL;
					$$ = parameter_list;
				}
			 | parameterlist SEMICOLON identifierlist COLON type {
					ParameterList* parameter_list = (ParameterList*)malloc(sizeof(ParameterList));
					parameter_list->identifier_list = $3;
					parameter_list->type = $5;
					parameter_list->next = NULL;
					ParameterList* temp = $1;
					while(temp->next != NULL) {
						temp = temp->next;
					}
					temp->next = parameter_list;
					$$ = $1;
				}
;

compoundstatement: BEG statementlist END { $$ = $2; }
;

statementlist: %empty { $$ = NULL; }
			 | statement {
					StatementList* statement_list = (StatementList*)malloc(sizeof(StatementList));
					statement_list->statement = $1;
					statement_list->next = NULL;
					$$ = statement_list;
				}
			 | statement SEMICOLON statementlist {
					StatementList* statement_list = (StatementList*)malloc(sizeof(StatementList));
					statement_list->statement = $1;
					statement_list->next = $3;
					$$ = statement_list;
				}
;

statement: variable ASSIGN expression {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 0;
				statement->statement.assignment = (Assignment*)malloc(sizeof(Assignment));
				statement->statement.assignment->variable = $1;
				statement->statement.assignment->expression = $3;
				$$ = statement;
			}
		 | procstatement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 1;
				statement->statement.proc_statement = $1;
				$$ = statement;
			}
		 | compoundstatement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 2;
				statement->statement.compound_statement = $1;
				$$ = statement;
			}
		 | IF expression THEN statement elseclause {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 3;
				statement->statement.if_then = (IfThen*)malloc(sizeof(IfThen));
				statement->statement.if_then->expression = $2;
				statement->statement.if_then->statement = $4;
				statement->statement.if_then->else_clause = $5;
				$$ = statement;
			}
		 | WHILE expression DO statement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 4;
				statement->statement.while_do = (WhileDo*)malloc(sizeof(WhileDo));
				statement->statement.while_do->expression = $2;
				statement->statement.while_do->statement = $4;
				$$ = statement;
			}
		 | FOR IDENTIFIER ASSIGN expression TO expression DO statement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 5;
				statement->statement.for_to = (ForTo*)malloc(sizeof(ForTo));
				statement->statement.for_to->identifier = $2;
				statement->statement.for_to->expression1 = $4;
				statement->statement.for_to->expression2 = $6;
				statement->statement.for_to->statement = $8;
				$$ = statement;
				
				SymbolTable *s = NULL;
				s = isDeclared($2);
				if (s == NULL) {
					printf("%d: Undeclared variable %s\n", yylineno, $2);
					error = 1;
				} else if (s->type != kVariable) {
					printf("%d: %s is not a variable\n", yylineno, $2);
				}
			}
		| WRITE LPAREN IDENTIFIER RPAREN {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 6;
				statement->statement.identifier = $3;
				$$ = statement;
			}
		| READ LPAREN IDENTIFIER RPAREN {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 7;
				statement->statement.identifier = $3;
				$$ = statement;
			}
		| WRITELN LPAREN IDENTIFIER RPAREN {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 8;
				statement->statement.identifier = $3;
				$$ = statement;
			}
		| READLN LPAREN IDENTIFIER RPAREN {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 9;
				statement->statement.identifier = $3;
				$$ = statement;
			}
;

elseclause: %empty { $$ = NULL; }
		  | ELSE statement { $$ = $2; }
;

variable: IDENTIFIER {
				Variable* variable = (Variable*)malloc(sizeof(Variable));
				variable->node_type = 0;
				variable->variable.identifier = $1;
				$$ = variable;
				
				SymbolTable *s = NULL;
				s = isDeclared($1);
				if (s == NULL) {
					printf("%d: Undeclared variable %s\n", yylineno, $1);
					error = 1;
				}
			}
		| IDENTIFIER LSQUARE expression RSQUARE {
				Variable* variable = (Variable*)malloc(sizeof(Variable));
				variable->node_type = 1;
				variable->variable.expression = (VariableExpression*)malloc(sizeof(VariableExpression));
				variable->variable.expression->identifier = $1;
				variable->variable.expression->expression = $3;
				$$ = variable;
				
				SymbolTable *s = NULL;
				s = isDeclared($1);
				if (s == NULL) {
					printf("%d: Undeclared variable %s\n", yylineno, $1);
					error = 1;
				} else if (s->type != kArray) {
					printf("%d: %s is not an array\n", yylineno, $1);
				}
			}
;

procstatement: IDENTIFIER {
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 0;
					proc_statement->proc_statement.identifier = $1;
					$$ = proc_statement;

					SymbolTable *s = NULL;
					s = isDeclared($1);
					if (s == NULL) {
						printf("%d: Undeclared function %s\n", yylineno, $1);
						error = 1;
					}
				}
			 | IDENTIFIER LPAREN expressionlist RPAREN {
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 1;
					proc_statement->proc_statement.expression_list = (ProcStatementExpressionList*)malloc(sizeof(ProcStatementExpressionList));
					proc_statement->proc_statement.expression_list->identifier = $1;
					proc_statement->proc_statement.expression_list->expression_list = $3;
					$$ = proc_statement;

					SymbolTable *s = NULL;
					s = isDeclared($1);
					if (s == NULL) {
						printf("%d: Undeclared function %s\n", yylineno, $1);
						error = 1;
					} else if (s->type != kFunction) {
						printf("%d: %s is not a function\n", yylineno, $1);
						error = 1;
					} else {
						ExpressionList* temp = $3;
						int num_arguments = 0;
						while (temp != NULL) {
							num_arguments++;
							temp = temp->next;
						}

						if(utarray_len(s->attributes.function->arguments_type) < num_arguments) {
							printf("%d: Too few parameters in call to the function %s\n", yylineno, s->symbol);
						} else if(utarray_len(s->attributes.function->arguments_type) > num_arguments) {
							printf("%d: Too many parameters in call to the function %s\n", yylineno, s->symbol);
						}
					}
				}
;

expressionlist: expression {
					ExpressionList* expression_list = (ExpressionList*)malloc(sizeof(ExpressionList));
					expression_list->expression = $1;
					expression_list->next = NULL;
					$$ = expression_list;
				}
			  | expressionlist COMMA expression {
					ExpressionList* expression_list = (ExpressionList*)malloc(sizeof(ExpressionList));
					expression_list->expression = $3;
					expression_list->next = NULL;
					ExpressionList* temp = $1;
					while(temp->next != NULL) {
						temp = temp->next;
					}
					temp->next = expression_list;
					$$ = $1;
				}
;

expression: simpleexpression {
				Expression* expression = (Expression*)malloc(sizeof(Expression));
				expression->node_type = 0;
				expression->expression.simple_expression = $1;
				$$ = expression;
			}
		  | simpleexpression relop expression {
				Expression* expression = (Expression*)malloc(sizeof(Expression));
				expression->node_type = 1;
				expression->expression.relation = (RelationalExpression*)malloc(sizeof(RelationalExpression));
				expression->expression.relation->simple_expression = $1;
				expression->expression.relation->relop = $2;
				expression->expression.relation->expression = $3;
				$$ = expression;
			}
;

relop: EQUALS { $$ = 0; }
	 | NOTEQUALS { $$ = 1; }
	 | LESS { $$ = 2; }
	 | LESSEQUALS { $$ = 3; }
	 | GREAT { $$ = 4; }
	 | GREATEQUALS { $$ = 5; }
;

simpleexpression: term {
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 0;
						simple_expression->simple_expression.term = $1;
						$$ = simple_expression;
					}
				| sign term {
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 1;
						simple_expression->simple_expression.signed_term = (SignedTerm*)malloc(sizeof(SignedTerm));
						simple_expression->simple_expression.signed_term->sign = $1;
						simple_expression->simple_expression.signed_term->term = $2;
						$$ = simple_expression;
					}
				| simpleexpression addop term {
						SimpleExpression* simple_expression = (SimpleExpression*)malloc(sizeof(SimpleExpression));
						simple_expression->node_type = 2;
						simple_expression->simple_expression.addition = (Addition*)malloc(sizeof(Addition));
						simple_expression->simple_expression.addition->addop = $2;
						simple_expression->simple_expression.addition->term = $3;
						simple_expression->simple_expression.addition->simple_expression = $1;
						$$ = simple_expression;
					}
;

addop: PLUS { $$ = 0; }
	 | MINUS { $$ = 1; }
	 | OR { $$ = 2; }
;

term: factor {
			Term* term = (Term*)malloc(sizeof(Term));
			term->node_type = 0;
			term->term.factor = $1;
			$$ = term;
		}
	| term mulop factor {
			Term* term = (Term*)malloc(sizeof(Term));
			term->node_type = 1;
			term->term.multiplication = (Multiplication*)malloc(sizeof(Multiplication));
			term->term.multiplication->mulop = $2;
			term->term.multiplication->factor = $3;
			term->term.multiplication->term = $1;
			$$ = term;
		}
;

mulop: ASTERISK { $$ = 0; }
	 | SOLIDUS { $$ = 1; }
	 | DIV { $$ = 2; }
	 | MODULUS { $$ = 3; }
	 | AND { $$ = 4; }
;

factor: IDENTIFIER LPAREN expressionlist RPAREN {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 0;
			factor->factor.expression_list = (FactorExpressionList*)malloc(sizeof(FactorExpressionList));
			factor->factor.expression_list->identifier = $1;
			factor->factor.expression_list->expression_list = $3;
			$$ = factor;
		}
	  | variable {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 1;
			factor->factor.variable = $1;
			$$ = factor;
		}
	  | INTNUM {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 2;
			factor->factor.integer = $1;
			$$ = factor;
		}
	  | REALNUM {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 3;
			factor->factor.real = $1;
			$$ = factor;
		}
	  | LPAREN expression RPAREN {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 4;
			factor->factor.expression = $2;
			$$ = factor;
		}
	  | NOT factor {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 5;
			factor->factor.factor = $2;
			$$ = factor;
		}
;

sign: PLUS { $$ = 0; }
	| MINUS { $$ = 1; }
;

%%

int main() {
	yyparse();
}

void yyerror(char const *s) {
	fprintf(stderr, "%s\n", s);
}
