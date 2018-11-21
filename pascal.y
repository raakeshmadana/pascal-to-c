%{
#include <stdio.h>
#include <stdlib.h>

#include "pascal.h"

extern int yylex();
extern int yyparse();
void yyerror(char const *);

void printTree(Program*);
void printIdentifierList(IdentifierList*);
void printDeclarations(Declarations*);
void printType(Type*);
void printArrayType(ArrayType*);
void printSubDeclarations(SubDeclarations*);
void printSubprogDeclaration(SubprogDeclaration*);
void printSubprogramHead(SubprogramHead*);
void printFunction(Function*);
void printProcedure(Procedure*);
void printParameterList(ParameterList*);
void printStatementList(StatementList*);
void printCompoundStatement(StatementList*);
void printStatement(Statement*);
void printAssignment(Assignment*);
void printIfThen(IfThen*);
void printWhileDo(WhileDo*);
void printForTo(ForTo*);
void printVariable(Variable*);
void printVariableExpression(VariableExpression*);
void printProcStatement(ProcStatement*);
void printProcStatementExpressionList(ProcStatementExpressionList*);
void printExpressionList(ExpressionList*);
void printExpression(Expression*);
void printRelationalExpression(RelationalExpression*);
void printSimpleExpression(SimpleExpression*);
void printSignedTerm(SignedTerm*);
void printAddition(Addition*);
void printTerm(Term*);
void printMultiplication(Multiplication*);
void printFactor(Factor*);
void printFactorExpressionList(FactorExpressionList*);
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
			printTree(program);
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

standardtype: INTEGER { $$ = $1; }
			| REAL { $$ = $1; }
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
						subprogram_head->subprogram_head.function = (Function*)malloc(sizeof(Function));
						subprogram_head->subprogram_head.function->identifier = $2;
						subprogram_head->subprogram_head.function->arguments = $3;
						subprogram_head->subprogram_head.function->standard_type = $5;
						$$ = subprogram_head;
					}
			  | PROCEDURE IDENTIFIER arguments SEMICOLON {
						SubprogramHead* subprogram_head = (SubprogramHead*)malloc(sizeof(SubprogramHead));
						subprogram_head->node_type = 1;
						subprogram_head->subprogram_head.procedure = (Procedure*)malloc(sizeof(Procedure));
						subprogram_head->subprogram_head.procedure->identifier = $2;
						subprogram_head->subprogram_head.procedure->arguments = $3;
						$$ = subprogram_head;
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
				statement->statement.if_then->next = $4;
				statement->statement.if_then->else_clause = $5;
				$$ = statement;
			}
		 | WHILE expression DO statement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 4;
				statement->statement.while_do = (WhileDo*)malloc(sizeof(WhileDo));
				statement->statement.while_do->expression = $2;
				statement->statement.while_do->next = $4;
				$$ = statement;
			}
		 | FOR IDENTIFIER ASSIGN expression TO expression DO statement {
				Statement* statement = (Statement*)malloc(sizeof(Statement));
				statement->node_type = 5;
				statement->statement.for_to = (ForTo*)malloc(sizeof(ForTo));
				statement->statement.for_to->identifier = $2;
				statement->statement.for_to->expression1 = $4;
				statement->statement.for_to->expression2 = $6;
				statement->statement.for_to->next = $8;
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
			}
		| IDENTIFIER LSQUARE expression RSQUARE {
				Variable* variable = (Variable*)malloc(sizeof(Variable));
				variable->node_type = 1;
				variable->variable.expression = (VariableExpression*)malloc(sizeof(VariableExpression));
				variable->variable.expression->identifier = $1;
				variable->variable.expression->expression = $3;
				$$ = variable;
			}
;

procstatement: IDENTIFIER {
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 0;
					proc_statement->proc_statement.identifier = $1;
					$$ = proc_statement;
				}
			 | IDENTIFIER LPAREN expressionlist RPAREN {
					ProcStatement* proc_statement = (ProcStatement*)malloc(sizeof(ProcStatement));
					proc_statement->node_type = 1;
					proc_statement->proc_statement.expression_list = (ProcStatementExpressionList*)malloc(sizeof(ProcStatementExpressionList));
					proc_statement->proc_statement.expression_list->identifier = $1;
					proc_statement->proc_statement.expression_list->expression_list = $3;
					$$ = proc_statement;
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
				expression->expression.relation->next = $3;
				$$ = expression;
			}
;

relop: EQUALS { $$ = $1; }
	 | NOTEQUALS { $$ = $1; }
	 | LESS { $$ = $1; }
	 | LESSEQUALS { $$ = $1; }
	 | GREAT { $$ = $1; }
	 | GREATEQUALS { $$ = $1; }
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
						simple_expression->simple_expression.addition->next = $1;
						$$ = simple_expression;
					}
;

addop: PLUS { $$ = $1; }
	 | MINUS { $$ = $1; }
	 | OR { $$ = $1; }
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
			term->term.multiplication->next = $1;
			$$ = term;
		}
;

mulop: ASTERISK { $$ = $1; }
	 | SOLIDUS { $$ = $1; }
	 | DIV { $$ = $1; }
	 | MODULUS { $$ = $1; }
	 | AND { $$ = $1; }
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
			factor->node_type = 5;
			factor->factor.expression = $2;
			$$ = factor;
		}
	  | NOT factor {
			Factor* factor = (Factor*)malloc(sizeof(Factor));
			factor->node_type = 6;
			factor->factor.factor = $2;
			$$ = factor;
		}
;

sign: PLUS { $$ = $1; }
	| MINUS { $$ = $1; }
;

%%

int main() {
	yyparse();
}

void yyerror(char const *s) {
	fprintf(stderr, "%s\n", s);
}

void printTree(Program* program) {
	printf("%s\n", program->identifier);
	printDeclarations(program->declarations);
	printSubDeclarations(program->sub_declarations);
	printCompoundStatement(program->compound_statement);
}

void printIdentifierList(IdentifierList* identifier_list) {
	printf("IdentifierList\n");
	while (identifier_list != NULL) {
		printf("%s\n", identifier_list->identifier);
		identifier_list = identifier_list->next;
	}
}
void printDeclarations(Declarations* declarations) {
	printf("Declarations\n");
	while (declarations != NULL) {
		printIdentifierList(declarations->identifier_list);
		printType(declarations->type);
		declarations = declarations->next;
	}
}

void printType(Type* type) {
	printf("Type\n");
	switch (type->node_type) {
		case 0:
			printf("StandardType:%d\n", type->type.standard_type);
			break;
		case 1:
			printArrayType(type->type.array_type);
			break;
	}
}

void printArrayType(ArrayType* array_type) {
	printf("ArrayType\n");
	printf("From:%d\n", array_type->from);
	printf("To:%d\n", array_type->to);
	printf("StandardType:%d\n", array_type->standard_type);
	printf("Exiting ArrayType\n");
}

void printSubDeclarations(SubDeclarations* sub_declarations) {
	printf("SubDeclarations\n");
	while (sub_declarations != NULL) {
		printSubprogDeclaration(sub_declarations->subprog_declaration);
		sub_declarations = sub_declarations->next;
	}
}

void printSubprogDeclaration(SubprogDeclaration* subprog_declaration) {
	printf("SubprogDeclaration\n");
	printSubprogramHead(subprog_declaration->subprogram_head);
	printDeclarations(subprog_declaration->declarations);
	printCompoundStatement(subprog_declaration->compound_statement);
}

void printSubprogramHead(SubprogramHead* subprogram_head) {
	printf("SubprogramHead\n");
	switch (subprogram_head->node_type) {
		case 0:
			printFunction(subprogram_head->subprogram_head.function);
			break;
		case 1:
			printProcedure(subprogram_head->subprogram_head.procedure);
			break;
	}
}

void printFunction(Function* function) {
	printf("Function\n");
	printf("%s\n", function->identifier);
	printParameterList(function->arguments);
	printf("%d\n", function->standard_type);
}

void printProcedure(Procedure* procedure) {
	printf("Procedure\n");
	printf("%s\n", procedure->identifier);
	printParameterList(procedure->arguments);
}

void printParameterList(ParameterList* parameter_list) {
	printf("ParameterList\n");
	while (parameter_list != NULL) {
		printIdentifierList(parameter_list->identifier_list);
		printType(parameter_list->type);
		parameter_list = parameter_list->next;
	}
}

void printStatementList(StatementList* statement_list) {
	printf("StatementList\n");
	while (statement_list != NULL) {
		printStatement(statement_list->statement);
		statement_list = statement_list->next;
	}
}

void printCompoundStatement(StatementList* statement_list) {
	printf("CompoundStatement\n");
	while (statement_list != NULL) {
		printStatement(statement_list->statement);
		statement_list = statement_list->next;
	}
}

void printStatement(Statement* statement) {
	printf("Statement: %d\n", statement->node_type);
	if (statement != NULL) {
		switch (statement->node_type) {
			case 0:
				printAssignment(statement->statement.assignment);
				break;
			case 1:
				printProcStatement(statement->statement.proc_statement);
				break;
			case 2:
				printCompoundStatement(statement->statement.compound_statement);
				break;
			case 3:
				printIfThen(statement->statement.if_then);
				break;
			case 4:
				printWhileDo(statement->statement.while_do);
				break;
			case 5:
				printForTo(statement->statement.for_to);
				break;
		}
	}
}

void printAssignment(Assignment* assignment) {
	printf("Assignment\n");
	printVariable(assignment->variable);
	printExpression(assignment->expression);
}

void printIfThen(IfThen* if_then) {
	printf("IfThen\n");
	printExpression(if_then->expression);
	printStatement(if_then->next);
	printStatement(if_then->else_clause);
}

void printWhileDo(WhileDo* while_do) {
	printf("WhileDo\n");
	printExpression(while_do->expression);
	printStatement(while_do->next);
}

void printForTo(ForTo* for_to) {
	printf("ForTo\n");
	printf("%s\n", for_to->identifier);
	printExpression(for_to->expression1);
	printExpression(for_to->expression2);
	printStatement(for_to->next);
}

void printVariable(Variable* variable) {
	printf("Variable: %d\n", variable->node_type);
	switch (variable->node_type) {
		case 0:
			printf("%s\n", variable->variable.identifier);
			break;
		case 1:
			printVariableExpression(variable->variable.expression);
			break;
	}
}

void printVariableExpression(VariableExpression* expression) {
	printf("VariableExpression\n");
	printf("%s\n", expression->identifier);
	printExpression(expression->expression);
}

void printProcStatement(ProcStatement* proc_statement) {
	printf("ProcStatement\n");
	switch (proc_statement->node_type) {
		case 0:
			printf("%s\n", proc_statement->proc_statement.identifier);
		case 1:
			printProcStatementExpressionList(proc_statement->proc_statement.expression_list);
	}
}

void printProcStatementExpressionList(ProcStatementExpressionList* expression_list) {
	printf("ProcStatementExpressionList\n");
	printf("%s\n", expression_list->identifier);
	printExpressionList(expression_list->expression_list);
}

void printExpressionList(ExpressionList* expression_list) {
	printf("ExpressionList\n");
	while (expression_list->next != NULL) {
		printExpression(expression_list->expression);
	}
}

void printExpression(Expression* expression) {
	printf("Expression: %d\n", expression->node_type);
	switch(expression->node_type) {
		case 0:
			printSimpleExpression(expression->expression.simple_expression);
			break;
		case 1:
			printRelationalExpression(expression->expression.relation);
			break;
	}
}

void printRelationalExpression(RelationalExpression* relation) {
	printf("RelationalExpression\n");
	printSimpleExpression(relation->simple_expression);
	printf("%d\n", relation->relop);
	printExpression(relation->next);
}

void printSimpleExpression(SimpleExpression* simple_expression) {
	printf("SimpleExpression\n");
	switch (simple_expression->node_type) {
		case 0:
			printTerm(simple_expression->simple_expression.term);
			break;
		case 1:
			printSignedTerm(simple_expression->simple_expression.signed_term);
			break;
		case 2:
			printAddition(simple_expression->simple_expression.addition);
			break;
	}
}

void printSignedTerm(SignedTerm* signed_term) {
	printf("SignedTerm\n");
	printf("%d\n", signed_term->sign);
	printTerm(signed_term->term);
}

void printAddition(Addition* addition) {
	printf("Addition\n");
	printSimpleExpression(addition->next);
	printf("%d\n", addition->addop);
	printTerm(addition->term);
}

void printTerm(Term* term) {
	printf("Term\n");
	switch (term->node_type) {
		case 0:
			printFactor(term->term.factor);
			break;
		case 1:
			printMultiplication(term->term.multiplication);
			break;
	}
}

void printMultiplication(Multiplication* multiplication) {
	printf("Multiplication\n");
	printf("%d\n", multiplication->mulop);
	printFactor(multiplication->factor);
	printTerm(multiplication->next);
}

void printFactor(Factor* factor) {
	printf("Factor: %d\n", factor->node_type);
	switch (factor->node_type) {
		case 0:
			printFactorExpressionList(factor->factor.expression_list);
			break;
		case 1:
			printVariable(factor->factor.variable);
			break;
		case 2:
			printf("%d\n", factor->factor.integer);
			break;
		case 3:
			printf("%f\n", factor->factor.real);
			break;
		case 4:
			printExpression(factor->factor.expression);
			break;
		case 5:
			printFactor(factor->factor.factor);
			break;
	}
}

void printFactorExpressionList(FactorExpressionList* expression_list) {
	printf("FactorExpressionList\n");
	printf("%s\n", expression_list->identifier);
	printExpressionList(expression_list->expression_list);
}
