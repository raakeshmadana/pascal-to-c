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

// Structure of the symbol table
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

// Routines to add symbols to symbol table
void addProgramName(char*);
void addVariable(char*, Type*);
int addProcedure(char*, ParameterList*, int);
int addFunction(char*, ParameterList*, int, int);
void printSymbols();

// Error check helpers
SymbolTable* isDeclared(char*);
int checkExpression(char*, Expression*, int);

// Code generation routines
void genCode(Program*);
void genCodeDeclarations(Declarations*, FILE*);
void genCodeSubDeclarations(SubDeclarations*);
void genCodeSubprogDeclaration(SubprogDeclaration*);
void genCodeSubprogramHead(SubprogramHead*);
void genCodeFunction(FunctionRule*);
void genCodeProcedure(ProcedureRule*);
void genCodeArguments(ParameterList*, FILE*, FILE*);
void genCodeCompoundStatement(StatementList*, FILE*);
void genCodeStatement(Statement*, FILE*);
void genCodeAssignment(Assignment*, FILE*);
void genCodeIfThen(IfThen*, FILE*);
void genCodeWhileDo(WhileDo*, FILE*);
void genCodeForTo(ForTo*, FILE*);
void genCodeRead(char*, FILE*);
void genCodeWrite(char*, FILE*);
void genCodeReadln(char*, FILE*);
void genCodeWriteln(char*, FILE*);
void genCodeVariable(Variable*, FILE*);
void genCodeVariableExpression(VariableExpression*, FILE*);
void genCodeProcStatement(ProcStatement*, FILE*);
void genCodeProcStatementExpressionList(ProcStatementExpressionList*, FILE*);
void genCodeExpressionList(ExpressionList*, FILE*);
void genCodeExpression(Expression*, FILE*);
void genCodeRelationalExpression(RelationalExpression*, FILE*);
void genCodeSimpleExpression(SimpleExpression*, FILE*);
void genCodeSignedTerm(SignedTerm*, FILE*);
void genCodeAddition(Addition*, FILE*);
void genCodeTerm(Term*, FILE*);
void genCodeMultiplication(Multiplication*, FILE*);
void genCodeFactor(Factor*, FILE*);
void genCodeFactorExpressionList(FactorExpressionList*, FILE*);
