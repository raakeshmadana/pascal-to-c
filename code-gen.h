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
int addProcedure(char* symbol, ParameterList* arguments, int linenum);
int addFunction(char* symbol, ParameterList* arguments, int return_type, int linenum);
void printSymbols();

SymbolTable* isDeclared(char*);
int checkExpression(char*, Expression*, int);

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
void printRead(char*, FILE*);
void printWrite(char*, FILE*);
void printReadln(char*, FILE*);
void printWriteln(char*, FILE*);
void printVariable(Variable*, FILE*);
void printVariableExpression(VariableExpression*, FILE*);
void printProcStatement(ProcStatement*, FILE*);
void printProcStatementExpressionList(ProcStatementExpressionList*, FILE*);
void printExpressionList(ExpressionList*, FILE*);
void printExpression(Expression*, FILE*);
void printRelationalExpression(RelationalExpression*, FILE*);
void printSimpleExpression(SimpleExpression*, FILE*);
void printSignedTerm(SignedTerm*, FILE*);
void printAddition(Addition*, FILE*);
void printTerm(Term*, FILE*);
void printMultiplication(Multiplication*, FILE*);
void printFactor(Factor*, FILE*);
void printFactorExpressionList(FactorExpressionList*, FILE*);
