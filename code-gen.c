#include <stdio.h>
#include <stdlib.h>

#include "code-gen.h"

SymbolTable* symbols = NULL;

void addProgramName(char* symbol) {
	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kProgram;
	HASH_ADD_KEYPTR(hh,	symbols, s->symbol, strlen(s->symbol), s);
}

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

void addProcedureAttributes(Procedure* procedure, Type* type) {
	utarray_new(procedure->arguments_type, &ut_int_icd);
	utarray_new(procedure->arguments_data_type, &ut_int_icd);

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

void addProcedure(char* symbol, ParameterList* arguments) {
	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kProcedure;

	while(arguments != NULL) {
		IdentifierList* temp = arguments->identifier_list;
		while(temp != NULL) {
			addVariable(temp->identifier, arguments->type);
			s->attributes.procedure = (Procedure*)malloc(sizeof(Procedure));
			addProcedureAttributes(s->attributes.procedure, arguments->type);
			temp = temp->next;
		}
		arguments = arguments->next;
	}
	
	HASH_ADD_KEYPTR(hh, symbols, s->symbol, strlen(s->symbol), s);
}

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

void addFunction(char* symbol, ParameterList* arguments, int return_type) {
	SymbolTable *s = (SymbolTable*)malloc(sizeof(SymbolTable));
	s->symbol = symbol;
	s->type = kFunction;
	s->attributes.function = (Function*)malloc(sizeof(Function));
	utarray_new(s->attributes.function->arguments_type, &ut_int_icd);
	utarray_new(s->attributes.function->arguments_data_type, &ut_int_icd);

	while(arguments != NULL) {
		IdentifierList* temp = arguments->identifier_list;
		while(temp != NULL) {
			addVariable(temp->identifier, arguments->type);
			addFunctionAttributes(s->attributes.function, arguments->type, return_type);
			temp = temp->next;
		}

		arguments = arguments->next;
	}
	
	HASH_ADD_KEYPTR(hh, symbols, s->symbol, strlen(s->symbol), s);
}

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

void printTree(Program* program) {
	printf("%s\n", program->identifier);
	// Determine file name
	/*char *file = (char*)malloc(strlen(program->identifier) + 1 + 2);
	strcpy(file, program->identifier);
	strcat(file, ".c");
	FILE* main = fopen(file, "a");*/

	FILE* main = fopen("main.c", "a");

	// Include headers
	fprintf(main, "#include <stdio.h>\n");
	fprintf(main, "#include <stdlib.h>\n\n");

	printDeclarations(program->declarations, main);

	// Write main function
	fprintf(main, "int main(void) {\n");

	printSubDeclarations(program->sub_declarations);
	printCompoundStatement(program->compound_statement, main);

	// Return and close main
	fprintf(main, "return 0;\n}");

	fclose(main);
}

void printIdentifierList(IdentifierList* identifier_list) {
	printf("IdentifierList\n");
	while (identifier_list != NULL) {
		printf("%s\n", identifier_list->identifier);
		identifier_list = identifier_list->next;
	}
}
void printDeclarations(Declarations* declarations, FILE* file) {
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
	if(sub_declarations != NULL) {
		FILE* definitions = fopen("functions.c", "a");
		fprintf(definitions, "#include \"functions.h\"\n\n");
		fclose(definitions);
	}

	while (sub_declarations != NULL) {
		printSubprogDeclaration(sub_declarations->subprog_declaration);
		sub_declarations = sub_declarations->next;
	}
}

void printSubprogDeclaration(SubprogDeclaration* subprog_declaration) {
	printf("SubprogDeclaration\n");
	printSubprogramHead(subprog_declaration->subprogram_head);

	FILE* definitions = fopen("functions.c", "a");
	printDeclarations(subprog_declaration->declarations, definitions);

	printCompoundStatement(subprog_declaration->compound_statement, definitions);

	fprintf(definitions, "}\n\n");

	fclose(definitions);
}

void printSubprogramHead(SubprogramHead* subprogram_head) {
	printf("SubprogramHead\n");
	switch (subprogram_head->node_type) {
		case 0:
			printFunction(subprogram_head->subprogram_head.function_rule);
			break;
		case 1:
			printProcedure(subprogram_head->subprogram_head.procedure_rule);
			break;
	}
}

void printArguments(ParameterList* arguments, FILE* prototypes, FILE* definitions) {
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

void printFunction(FunctionRule* function_rule) {
	printf("Function\n");
	printf("%s\n", function_rule->identifier);
	printParameterList(function_rule->arguments);
	printf("%d\n", function_rule->standard_type);

	FILE *prototypes, *definitions;
	prototypes = fopen("functions.h", "a");
	definitions = fopen("functions.c", "a");

	// Determine return type
	char *return_type = (function_rule->standard_type == 0) ? "int" : "float";
	fprintf(prototypes, "%s ", return_type);
	fprintf(definitions, "%s ", return_type);
	fprintf(prototypes, "%s(", function_rule->identifier);
	fprintf(definitions, "%s(", function_rule->identifier);

	printArguments(function_rule->arguments, prototypes, definitions);

	fclose(prototypes);
	fclose(definitions);
}

void printProcedure(ProcedureRule* procedure_rule) {
	printf("Procedure\n");
	printf("%s\n", procedure_rule->identifier);
	printParameterList(procedure_rule->arguments);

	FILE *prototypes, *definitions;
	prototypes = fopen("functions.h", "a");
	definitions = fopen("functions.c", "a");

	// Procedures don't return
	fprintf(prototypes, "void ");
	fprintf(definitions, "void ");
	fprintf(prototypes, "%s(", procedure_rule->identifier);
	fprintf(definitions, "%s(", procedure_rule->identifier);

	printArguments(procedure_rule->arguments, prototypes, definitions);

	fclose(prototypes);
	fclose(definitions);
}

void printParameterList(ParameterList* parameter_list) {
	printf("ParameterList\n");
	while (parameter_list != NULL) {
		printIdentifierList(parameter_list->identifier_list);
		printType(parameter_list->type);
		parameter_list = parameter_list->next;
	}
}

void printCompoundStatement(StatementList* statement_list, FILE* file) {
	printf("CompoundStatement\n");
	while (statement_list != NULL) {
		printStatement(statement_list->statement, file);
		statement_list = statement_list->next;
	}
}

void printStatement(Statement* statement, FILE* file) {
	printf("Statement: %d\n", statement->node_type);
	if (statement != NULL) {
		switch (statement->node_type) {
			case 0:
				printAssignment(statement->statement.assignment, file);
				break;
			case 1:
				printProcStatement(statement->statement.proc_statement, file);
				break;
			case 2:
				printCompoundStatement(statement->statement.compound_statement, file);
				break;
			case 3:
				printIfThen(statement->statement.if_then, file);
				break;
			case 4:
				printWhileDo(statement->statement.while_do, file);
				break;
			case 5:
				printForTo(statement->statement.for_to, file);
				break;
		}
	}
}

void printAssignment(Assignment* assignment, FILE* file) {
	printf("Assignment\n");
	printVariable(assignment->variable, file);
	fprintf(file, "= ");
	fprintf(file, "EXPRESSION;\n");
	printExpression(assignment->expression);
}

void printIfThen(IfThen* if_then, FILE* file) {
	printf("IfThen\n");

	fprintf(file, "if (");
	fprintf(file, "EXPRESSION");
	printExpression(if_then->expression);
	fprintf(file, ") {\n");

	printStatement(if_then->statement, file);
	fprintf(file, "}");

	if(if_then->else_clause != NULL) {
		fprintf(file, " else {\n");
		printStatement(if_then->else_clause, file);
		fprintf(file, "}\n");
	}
}

void printWhileDo(WhileDo* while_do, FILE* file) {
	printf("WhileDo\n");
	fprintf(file, "while (");
	printExpression(while_do->expression);
	fprintf(file, "EXPRESSION) {\n");
	printStatement(while_do->statement, file);
	fprintf(file, "}\n");
}

void printForTo(ForTo* for_to, FILE* file) {
	printf("ForTo\n");
	printf("%s\n", for_to->identifier);

	fprintf(file, "for (%s = ", for_to->identifier);
	fprintf(file, "EXPRESSION; ");
	printExpression(for_to->expression1);

	fprintf(file, "%s < ", for_to->identifier);
	fprintf(file, "EXPRESSION; ");
	printExpression(for_to->expression2);
	fprintf(file, "%s++) {\n", for_to->identifier);

	printStatement(for_to->statement, file);

	fprintf(file, "}\n");
}

void printVariable(Variable* variable, FILE* file) {
	printf("Variable: %d\n", variable->node_type);
	switch (variable->node_type) {
		case 0:
			printf("%s\n", variable->variable.identifier);
			fprintf(file, "%s ", variable->variable.identifier);
			break;
		case 1:
			printVariableExpression(variable->variable.expression, file);
			break;
	}
}

void printVariableExpression(VariableExpression* expression, FILE* file) {
	printf("VariableExpression\n");
	printf("%s\n", expression->identifier);
	fprintf(file, "%s[EXPRESSION] ", expression->identifier);
	printExpression(expression->expression);
}

void printProcStatement(ProcStatement* proc_statement, FILE* file) {
	printf("ProcStatement\n");
	switch (proc_statement->node_type) {
		case 0:
			printf("%s\n", proc_statement->proc_statement.identifier);
			fprintf(file, "%s(", proc_statement->proc_statement.identifier);
		case 1:
			printProcStatementExpressionList(proc_statement->proc_statement.expression_list, file);
	}
}

void printProcStatementExpressionList(ProcStatementExpressionList* expression_list, FILE* file) {
	printf("ProcStatementExpressionList\n");
	printf("%s\n", expression_list->identifier);
	fprintf(file, "%s(", expression_list->identifier);
	fprintf(file, "EXPRESSION-LIST);\n");
	printExpressionList(expression_list->expression_list);
}

void printExpressionList(ExpressionList* expression_list) {
	printf("ExpressionList\n");
	while (expression_list != NULL) {
		printExpression(expression_list->expression);
		expression_list = expression_list->next;
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
	printExpression(relation->expression);
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
	printSimpleExpression(addition->simple_expression);
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
	printTerm(multiplication->term);
}

void printFactor(Factor* factor) {
	printf("Factor: %d\n", factor->node_type);
	switch (factor->node_type) {
		case 0:
			printFactorExpressionList(factor->factor.expression_list);
			break;
		case 1:
			//printVariable(factor->factor.variable);
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
