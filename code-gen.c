#include <stdio.h>
#include <stdlib.h>

#include "code-gen.h"

SymbolTable* symbols = NULL;

// Adds the program name to the symbol table
void addProgramName(char* symbol) {
	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kProgram;
	HASH_ADD_KEYPTR(hh,	symbols, s->symbol, strlen(s->symbol), s);
}

// Add variables and arrays to the symbol table
void addVariable(char* symbol, Type* type) {
	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	
	switch(type->node_type) {
		case 0:
			s->type = kVariable;
			s->attributes.data_type = type->type.standard_type;
			break;
		case 1:
			s->type = kArray;
			ArrayType* array_type = type->type.array_type;
			s->attributes.array = (Array*)malloc(sizeof(Array));
			s->attributes.array->data_type = array_type->standard_type;
			s->attributes.array->start_index = array_type->from;
			s->attributes.array->end_index = array_type->to;
			break;
	}

	HASH_ADD_KEYPTR(hh,	symbols, s->symbol, strlen(s->symbol), s);
}

// Adds the type of arguments (variable or array) and the data type of the arguments
// to the attributes of the procedure in the symbol table
void addProcedureAttributes(Procedure* procedure, Type* type) {
	utarray_push_back(procedure->arguments_type, &type->node_type);

	switch(type->node_type) {
		case 0:
			utarray_push_back(procedure->arguments_data_type, &type->type.standard_type);
			break;
		case 1:
			utarray_push_back(procedure->arguments_data_type, &type->type.array_type->standard_type);
			break;
	}
}

// Adds the procedure to the symbol table
int addProcedure(char* symbol, ParameterList* arguments, int linenum) {
	int err = 0;

	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kProcedure;
	s->attributes.procedure = (Procedure*)malloc(sizeof(Procedure));
	utarray_new(s->attributes.procedure->arguments_type, &ut_int_icd);
	utarray_new(s->attributes.procedure->arguments_data_type, &ut_int_icd);

	while(arguments != NULL) {
		IdentifierList* temp = arguments->identifier_list;
		while(temp != NULL) {
			// Checks if the arguments are already in the symbol table
			SymbolTable* check = NULL;
			check = isDeclared(temp->identifier);
			if (check) {
				printf("%d: %s Re-declared\n", linenum, check->symbol);
				err = 1;
			} else {
				addVariable(temp->identifier, arguments->type);
			}
			addProcedureAttributes(s->attributes.procedure, arguments->type);
			temp = temp->next;
		}

		arguments = arguments->next;
	}
	
	HASH_ADD_KEYPTR(hh, symbols, s->symbol, strlen(s->symbol), s);

	return err;
}

// Adds the type of arguments (variable or array) and the data type of the arguments
// to the attributes of the function in the symbol table
void addFunctionAttributes(Function* function, Type* type, int return_type) {
	utarray_push_back(function->arguments_type, &type->node_type);

	switch(type->node_type) {
		case 0:
			utarray_push_back(function->arguments_data_type, &type->type.standard_type);
			break;
		case 1:
			utarray_push_back(function->arguments_data_type, &type->type.array_type->standard_type);
			break;
	}

	function->return_type = return_type;
}

// Adds the procedure to the symbol table
int addFunction(char* symbol, ParameterList* arguments, int return_type, int linenum) {
	int err = 0;

	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kFunction;
	s->attributes.function = (Function*)malloc(sizeof(Function));
	utarray_new(s->attributes.function->arguments_type, &ut_int_icd);
	utarray_new(s->attributes.function->arguments_data_type, &ut_int_icd);

	while(arguments != NULL) {
		IdentifierList* temp = arguments->identifier_list;
		while(temp != NULL) {
			// Checks if the arguments are already in the symbol table
			SymbolTable* check;
			check = isDeclared(temp->identifier);
			if (check) {
				printf("%d: %s Re-declared\n", linenum, check->symbol);
				err = 1;
			} else {
				addVariable(temp->identifier, arguments->type);
			}
			addFunctionAttributes(s->attributes.function, arguments->type, return_type);
			temp = temp->next;
		}

		arguments = arguments->next;
	}
	
	HASH_ADD_KEYPTR(hh, symbols, s->symbol, strlen(s->symbol), s);

	return err;
}

// Gets the attributes of the symbol from the symbol table.
// Returns NULL if the symbol is not present.
SymbolTable* isDeclared(char* identifier) {
	SymbolTable* s = NULL;
	HASH_FIND_STR(symbols, identifier, s);
	return s;
}

// Checks if the expression inside the array's subscript is a number.
// If it is a number, checks if it is out of bounds or a real number 
int checkExpression(char* identifier, Expression* expression, int linenum) {
	int err = 0;

	if (expression->node_type == 0) {
		SimpleExpression* simple_expression = expression->expression.simple_expression;
		if(simple_expression->node_type == 0) {
			Term* term = simple_expression->simple_expression.term;
			if (term->node_type == 0) {
				Factor* factor = term->term.factor;
				if (factor->node_type == 2) {
					SymbolTable* s = NULL;
					HASH_FIND_STR(symbols, identifier, s);
					int start_index = s->attributes.array->start_index;
					int end_index = s->attributes.array->end_index;
					int index = factor->factor.integer;
					if (index < start_index || index > end_index) {
						printf("%d: Array index out of bounds\n", linenum);
						err = 1;
					}
				} else if (factor->node_type == 3) {
					printf("%d: Array index cannot be real number\n", linenum); 
					err = 1;
				}
			}
		}
	}

	return err;
}

// Prints all the symbols in the symbol table
void printSymbols() {
	SymbolTable *s, *temp;

	printf("***********************\n");
	HASH_ITER(hh, symbols, s, temp) {
		printf("Symbol %s, Type %d\n", s->symbol, s->type);
		switch(s->type) {
			printf("Attributes\n");

			case kVariable:
				printf("%d\n", s->attributes.data_type);
				break;

			case kArray:
				printf("%d\n", s->attributes.array->data_type);
				printf("%d\n", s->attributes.array->start_index);
				printf("%d\n", s->attributes.array->end_index);
				break;
				
			case kProcedure:
				printf("Type of Arguments\n");
				for(int* p = (int*)utarray_front(s->attributes.procedure->arguments_type);
					p != NULL;
					p = (int*)utarray_next(s->attributes.procedure->arguments_type, p)) {
						printf("%d\t", *p);
				}
				printf("\n");

				printf("Datatype of arguments\n");
				for(int* p = (int*)utarray_front(s->attributes.procedure->arguments_data_type);
					p != NULL;
					p = (int*)utarray_next(s->attributes.procedure->arguments_data_type, p)) {
						printf("%d\t", *p);
				}
				printf("\n");
				break;
			case kFunction:
				printf("Type of Arguments\n");
				for(int* p = (int*)utarray_front(s->attributes.function->arguments_type);
					p != NULL;
					p = (int*)utarray_next(s->attributes.function->arguments_type, p)) {
						printf("%d\t", *p);
				}
				printf("\n");

				printf("Datatype of arguments\n");
				for(int* p = (int*)utarray_front(s->attributes.function->arguments_data_type);
					p != NULL;
					p = (int*)utarray_next(s->attributes.function->arguments_data_type, p)) {
						printf("%d\t", *p);
				}
				printf("\n");
				break;
		}
	}
	printf("***********************\n");
}

// Begins generating code
void genCode(Program* program) {
	// Uses the program name as the main file name
	char *file = (char*)malloc(strlen(program->identifier) + 1 + 2);
	strcpy(file, program->identifier);
	strcat(file, ".c");
	FILE* main = fopen(file, "a");

	// Include headers
	fprintf(main, "#include <stdio.h>\n");
	fprintf(main, "#include <stdlib.h>\n\n");

	genCodeDeclarations(program->declarations, main);

	// Write main function
	fprintf(main, "int main(void) {\n");

	genCodeSubDeclarations(program->sub_declarations);
	genCodeCompoundStatement(program->compound_statement, main);

	// Return and close main
	fprintf(main, "return 0;\n}");

	fclose(main);
}

// Generates variable and array declarations
void genCodeDeclarations(Declarations* declarations, FILE* file) {
	while (declarations != NULL) {
		// Determine data type
		Type* type = declarations->type;
		char* data_type;
		switch(type->node_type) {
			case 0:
				data_type = (type->type.standard_type == 0) ? "int" : "float";
				break;
			case 1:
				data_type = (type->type.array_type->standard_type == 0) ? "int" : "float";
				break;
		}
		fprintf(file, "%s ", data_type);

		IdentifierList* temp = declarations->identifier_list;
		char *identifier;
		while(temp != NULL) {
			identifier = (char*)malloc(strlen(temp->identifier) + 1 + 2 + 6);
			strcpy(identifier, temp->identifier);

			if(type->node_type == 1) {
				// Find the size of the array
				ArrayType* array_type = type->type.array_type;
				int size = array_type->to - array_type->from + 1;
				char* subscript = (char*)malloc(1 + 2 + 6);
				sprintf(subscript, "[%d]", size);
				strcat(identifier, subscript);
				free(subscript);
			}

			fprintf(file, "%s", identifier);
			free(identifier);
			
			// Add a semi-colon after the last variable
			if(temp->next != NULL) {
				fprintf(file, ", ");
			} else {
				fprintf(file, ";\n");
			}

			temp = temp->next;
		}
		declarations = declarations->next;
	}
}

// Starts generating function declarations and function definitions
void genCodeSubDeclarations(SubDeclarations* sub_declarations) {
	// Generates function prototypes in a header file
	// Generates function definitions in a separate .c file
	if(sub_declarations != NULL) {
		FILE* definitions = fopen("functions.c", "a");
		fprintf(definitions, "#include \"functions.h\"\n\n");
		fclose(definitions);
	}

	while (sub_declarations != NULL) {
		genCodeSubprogDeclaration(sub_declarations->subprog_declaration);
		sub_declarations = sub_declarations->next;
	}
}

void genCodeSubprogDeclaration(SubprogDeclaration* subprog_declaration) {
	genCodeSubprogramHead(subprog_declaration->subprogram_head);

	FILE* definitions = fopen("functions.c", "a");
	genCodeDeclarations(subprog_declaration->declarations, definitions);

	genCodeCompoundStatement(subprog_declaration->compound_statement, definitions);

	fprintf(definitions, "}\n\n");

	fclose(definitions);
}

// Generate function and procedure declarations
void genCodeSubprogramHead(SubprogramHead* subprogram_head) {
	switch (subprogram_head->node_type) {
		case 0:
			genCodeFunction(subprogram_head->subprogram_head.function_rule);
			break;
		case 1:
			genCodeProcedure(subprogram_head->subprogram_head.procedure_rule);
			break;
	}
}

// Generate arguments of functions and procedures
void genCodeArguments(ParameterList* arguments, FILE* prototypes, FILE* definitions) {
	while(arguments != NULL) {
		IdentifierList* temp = arguments->identifier_list;
		Type* type = arguments->type;

		// Determine datatype of the parameters
		char* data_type;
		switch(type->node_type) {
			case 0:
				data_type = (type->type.standard_type == 0) ? "int" : "float";
				break;
			case 1:
				data_type = (type->type.array_type->standard_type == 0) ? "int" : "float";
				break;
		}

		char *parameter;
		while(temp != NULL) {
			fprintf(prototypes, "%s ", data_type);
			fprintf(definitions, "%s ", data_type);
			
			parameter = (char*)malloc(strlen(temp->identifier) + 1 + 2 + 6);
			strcpy(parameter, temp->identifier);

			if(type->node_type == 1) {
				// Find the size of the array
				ArrayType* array_type = type->type.array_type;
				int size = array_type->to - array_type->from + 1;
				char* subscript = (char*)malloc(1 + 2 + 6);
				sprintf(subscript, "[%d]", size);
				strcat(parameter, subscript);
				free(subscript);
			}

			fprintf(prototypes, "%s", parameter);
			fprintf(definitions, "%s", parameter);
			free(parameter);

			// Don't add a comma after the last parameter
			if(temp->next != NULL || arguments->next != NULL) {
				fprintf(prototypes, ", ");
				fprintf(definitions, ", ");
			}

			temp = temp->next;
		}

		arguments = arguments->next;
	}

	fprintf(prototypes, ");\n");
	fprintf(definitions, ") {\n");
}

// Generates function declarations
void genCodeFunction(FunctionRule* function_rule) {
	FILE *prototypes, *definitions;
	prototypes = fopen("functions.h", "a");
	definitions = fopen("functions.c", "a");

	// Determine return type
	char *return_type = (function_rule->standard_type == 0) ? "int" : "float";
	fprintf(prototypes, "%s ", return_type);
	fprintf(definitions, "%s ", return_type);

	fprintf(prototypes, "%s(", function_rule->identifier);
	fprintf(definitions, "%s(", function_rule->identifier);

	genCodeArguments(function_rule->arguments, prototypes, definitions);

	fclose(prototypes);
	fclose(definitions);
}

void genCodeProcedure(ProcedureRule* procedure_rule) {
	FILE *prototypes, *definitions;
	prototypes = fopen("functions.h", "a");
	definitions = fopen("functions.c", "a");

	// Procedures don't return
	fprintf(prototypes, "void ");
	fprintf(definitions, "void ");

	fprintf(prototypes, "%s(", procedure_rule->identifier);
	fprintf(definitions, "%s(", procedure_rule->identifier);

	genCodeArguments(procedure_rule->arguments, prototypes, definitions);

	fclose(prototypes);
	fclose(definitions);
}

// Generates compound statements
void genCodeCompoundStatement(StatementList* statement_list, FILE* file) {
	while (statement_list != NULL) {
		genCodeStatement(statement_list->statement, file);
		statement_list = statement_list->next;
	}
}

// Generate statements
void genCodeStatement(Statement* statement, FILE* file) {
	if (statement != NULL) {
		switch (statement->node_type) {
			case 0:
				genCodeAssignment(statement->statement.assignment, file);
				break;
			case 1:
				genCodeProcStatement(statement->statement.proc_statement, file);
				break;
			case 2:
				genCodeCompoundStatement(statement->statement.compound_statement, file);
				break;
			case 3:
				genCodeIfThen(statement->statement.if_then, file);
				break;
			case 4:
				genCodeWhileDo(statement->statement.while_do, file);
				break;
			case 5:
				genCodeForTo(statement->statement.for_to, file);
				break;
			case 6:
				genCodeWrite(statement->statement.identifier, file);
				break;
			case 7:
				genCodeRead(statement->statement.identifier, file);
				break;
			case 8:
				genCodeWriteln(statement->statement.identifier, file);
				break;
			case 9:
				genCodeReadln(statement->statement.identifier, file);
				break;
		}
	}
}

// Generates assignment statements
void genCodeAssignment(Assignment* assignment, FILE* file) {
	// If the left hand side contains a function name
	// generate return statement
	SymbolTable* s = NULL;
	HASH_FIND_STR(symbols, assignment->variable->variable.identifier, s);
	if (s) {
		if (s->type == kFunction) {
			fprintf(file, "return ");
		} else {
			genCodeVariable(assignment->variable, file);
			fprintf(file, " = ");
		}
	}

	genCodeExpression(assignment->expression, file);
	fprintf(file, ";\n");
}

// Generates if statements
void genCodeIfThen(IfThen* if_then, FILE* file) {
	fprintf(file, "if (");
	genCodeExpression(if_then->expression, file);
	fprintf(file, ") {\n");

	genCodeStatement(if_then->statement, file);
	fprintf(file, "}");

	if(if_then->else_clause != NULL) {
		fprintf(file, " else {\n");
		genCodeStatement(if_then->else_clause, file);
		fprintf(file, "}\n");
	} else {
		fprintf(file, "\n");
	}
}

// Generates while loops
void genCodeWhileDo(WhileDo* while_do, FILE* file) {
	fprintf(file, "while (");
	genCodeExpression(while_do->expression, file);
	fprintf(file, ") {\n");
	genCodeStatement(while_do->statement, file);
	fprintf(file, "}\n");
}

// Generates for loops
void genCodeForTo(ForTo* for_to, FILE* file) {
	fprintf(file, "for (%s = ", for_to->identifier);
	genCodeExpression(for_to->expression1, file);

	fprintf(file, "; %s < ", for_to->identifier);
	genCodeExpression(for_to->expression2, file);
	fprintf(file, "; %s++) {\n", for_to->identifier);

	genCodeStatement(for_to->statement, file);

	fprintf(file, "}\n");
}

// Generates printf statements
void genCodeWrite(char* identifier, FILE* file) {
	// Determine the datatype of the variable to be printed
	SymbolTable* s = NULL;
	char print_verb;
	HASH_FIND_STR(symbols, identifier, s);
	if (s) {
		if (s->attributes.data_type == 0) {
			print_verb = 'd';
		} else {
			print_verb = 'f';
		}
	}

	fprintf(file, "printf(\"%%%c\", %s);\n", print_verb, identifier);
}

// Generates scanf statements
void genCodeRead(char* identifier, FILE* file) {
	// Determine the datatype of the variable to be scanned
	SymbolTable* s = NULL;
	char scan_verb;
	HASH_FIND_STR(symbols, identifier, s);
	if (s) {
		if (s->attributes.data_type == 0) {
			scan_verb = 'd';
		} else {
			scan_verb = 'f';
		}
	}

	fprintf(file, "scanf(\"%%%c\", &%s);\n", scan_verb, identifier);
}

// Generates printf statements
void genCodeWriteln(char* identifier, FILE* file) {
	// Determine the datatype of the variable to be printed
	SymbolTable* s = NULL;
	char print_verb;
	HASH_FIND_STR(symbols, identifier, s);
	if (s) {
		if (s->attributes.data_type == 0) {
			print_verb = 'd';
		} else {
			print_verb = 'f';
		}
	}

	fprintf(file, "printf(\"%%%c\\n\", %s);\n", print_verb, identifier);
}

// Generates scanf statements
void genCodeReadln(char* identifier, FILE* file) {
	// Determine the datatype of the variable to be scanned
	SymbolTable* s = NULL;
	char scan_verb;
	HASH_FIND_STR(symbols, identifier, s);
	if (s) {
		if (s->attributes.data_type == 0) {
			scan_verb = 'd';
		} else {
			scan_verb = 'f';
		}
	}

	fprintf(file, "scanf(\"%%%c\\n\", &%s);\n", scan_verb, identifier);
}

// Generates variables
void genCodeVariable(Variable* variable, FILE* file) {
	switch (variable->node_type) {
		case 0:
			fprintf(file, "%s", variable->variable.identifier);
			break;
		case 1:
			genCodeVariableExpression(variable->variable.expression, file);
			break;
	}
}

// Generates array assignments
void genCodeVariableExpression(VariableExpression* variable_expression, FILE* file) {
	fprintf(file, "%s[", variable_expression->identifier);
	int printed = 0;
	if (variable_expression->expression->node_type == 0) {
		SimpleExpression* simple_expression = variable_expression->expression->expression.simple_expression;
		if(simple_expression->node_type == 0) {
			Term* term = simple_expression->simple_expression.term;
			if (term->node_type == 0) {
				Factor* factor = term->term.factor;
				if (factor->node_type == 2) {
					SymbolTable* s = NULL;
					HASH_FIND_STR(symbols, variable_expression->identifier, s);
					int index = factor->factor.integer - s->attributes.array->start_index;
					fprintf(file, "%d", index);
					printed = 1;
				}
			}
		}
	}

	if (!printed) {
		genCodeExpression(variable_expression->expression, file);
	}

	fprintf(file, "]");
}

// Generates function calls
void genCodeProcStatement(ProcStatement* proc_statement, FILE* file) {
	switch (proc_statement->node_type) {
		case 0:
			fprintf(file, "%s(", proc_statement->proc_statement.identifier);
		case 1:
			genCodeProcStatementExpressionList(proc_statement->proc_statement.expression_list, file);
	}
}

// Generates function calls with arguments
void genCodeProcStatementExpressionList(ProcStatementExpressionList* expression_list, FILE* file) {
	fprintf(file, "%s(", expression_list->identifier);

	genCodeExpressionList(expression_list->expression_list, file);

	fprintf(file, ");\n");
}

// Generates list of expressions
void genCodeExpressionList(ExpressionList* expression_list, FILE* file) {
	while (expression_list != NULL) {
		genCodeExpression(expression_list->expression, file);
		if (expression_list->next != NULL) {
			fprintf(file, ", ");
		}
		expression_list = expression_list->next;
	}
}

// Generates expressions
void genCodeExpression(Expression* expression, FILE* file) {
	switch(expression->node_type) {
		case 0:
			genCodeSimpleExpression(expression->expression.simple_expression, file);
			break;
		case 1:
			genCodeRelationalExpression(expression->expression.relation, file);
			break;
	}
}

// Generates relational expressions
void genCodeRelationalExpression(RelationalExpression* relation, FILE* file) {
	genCodeSimpleExpression(relation->simple_expression, file);

	// Determines relational operator
	char* relop;
	switch (relation->relop) {
		case 0:
			relop = "==";
			break;
		case 1:
			relop = "!=";
			break;
		case 2:
			relop = "<";
			break;
		case 3:
			relop = "<=";
			break;
		case 4:
			relop = ">";
			break;
		case 5:
			relop = ">=";
			break;
	}

	fprintf(file, " %s ", relop);

	genCodeExpression(relation->expression, file);
}

void genCodeSimpleExpression(SimpleExpression* simple_expression, FILE* file) {
	switch (simple_expression->node_type) {
		case 0:
			genCodeTerm(simple_expression->simple_expression.term, file);
			break;
		case 1:
			genCodeSignedTerm(simple_expression->simple_expression.signed_term, file);
			break;
		case 2:
			genCodeAddition(simple_expression->simple_expression.addition, file);
			break;
	}
}

void genCodeSignedTerm(SignedTerm* signed_term, FILE* file) {
	char sign = (signed_term->sign == 0) ? '+' : '-';
	fprintf(file, "%c", sign);
	genCodeTerm(signed_term->term, file);
}

void genCodeAddition(Addition* addition, FILE* file) {
	genCodeSimpleExpression(addition->simple_expression, file);

	// Determines addition operator
	char* addop;
	switch (addition->addop) {
		case 0:
			addop = "+";
			break;
		case 1:
			addop = "-";
			break;
		case 2:
			addop = "||";
			break;
	}

	fprintf(file, " %s ", addop);

	genCodeTerm(addition->term, file);
}

void genCodeTerm(Term* term, FILE* file) {
	switch (term->node_type) {
		case 0:
			genCodeFactor(term->term.factor, file);
			break;
		case 1:
			genCodeMultiplication(term->term.multiplication, file);
			break;
	}
}

void genCodeMultiplication(Multiplication* multiplication, FILE* file) {
	// Determines multiplication operator
	char* mulop;
	switch (multiplication->mulop) {
		case 0:
			mulop = "*";
			break;
		case 1:
			fprintf(file, "(float) ");
		case 2:
			mulop = "/";
			break;
		case 3:
			mulop = "%";
			break;
		case 4:
			mulop = "&&";
			break;
	}

	genCodeTerm(multiplication->term, file);

	fprintf(file, " %s ", mulop);

	genCodeFactor(multiplication->factor, file);
}

void genCodeFactor(Factor* factor, FILE* file) {
	switch (factor->node_type) {
		case 0:
			genCodeFactorExpressionList(factor->factor.expression_list, file);
			break;
		case 1:
			genCodeVariable(factor->factor.variable, file);
			break;
		case 2:
			fprintf(file, "%d", factor->factor.integer);
			break;
		case 3:
			fprintf(file, "%f", factor->factor.real);
			break;
		case 4:
			genCodeExpression(factor->factor.expression, file);
			break;
		case 5:
			fprintf(file, "!");
			genCodeFactor(factor->factor.factor, file);
			break;
	}
}

void genCodeFactorExpressionList(FactorExpressionList* expression_list, FILE* file) {
	fprintf(file, "%s(", expression_list->identifier);
	genCodeExpressionList(expression_list->expression_list, file);
	fprintf(file, ")");
}
