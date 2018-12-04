#include "uthash.h"
#include "utarray.h"
#include "pascal.h"

typedef enum SymbolType{
	kProgram,
	kVariable,
	kArray,
	kProcedure,
	kFunction
} SymbolType;

typedef struct Array {
	int data_type;
	int start_index;
	int end_index;
} Array;

typedef struct Procedure {
	UT_array* arguments_type;
	UT_array* arguments_data_type;
} Procedure;

typedef struct Function {
	UT_array* arguments_type;
	UT_array* arguments_data_type;
	int return_type;
} Function;

typedef struct SymbolTable {
	char* symbol;
	SymbolType type;
	union {
		int data_type;
		Array* array;
		Procedure* procedure;
		Function* function;
	} attributes;
	UT_hash_handle hh;
} SymbolTable;

void addProgramName(char* symbol);
void addVariable(char* symbol, Type* type);
void addProcedure(char* symbol, ParameterList* arguments);
void addFunction(char* symbol, ParameterList* arguments, int return_type);
void printSymbols();

void printTree(Program*);
void printIdentifierList(IdentifierList*);
void printDeclarations(Declarations*, FILE*);
void printType(Type*);
void printArrayType(ArrayType*);
void printSubDeclarations(SubDeclarations*);
void printSubprogDeclaration(SubprogDeclaration*);
void printSubprogramHead(SubprogramHead*);
void printFunction(FunctionRule*);
void printProcedure(ProcedureRule*);
void printArguments(ParameterList*, FILE*, FILE*);
void printParameterList(ParameterList*);
void printCompoundStatement(StatementList*, FILE*);
void printStatement(Statement*, FILE*);
void printAssignment(Assignment*, FILE*);
void printIfThen(IfThen*, FILE*);
void printWhileDo(WhileDo*, FILE*);
void printForTo(ForTo*, FILE*);
void printVariable(Variable*, FILE*);
void printVariableExpression(VariableExpression*, FILE*);
void printProcStatement(ProcStatement*, FILE*);
void printProcStatementExpressionList(ProcStatementExpressionList*, FILE*);
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
