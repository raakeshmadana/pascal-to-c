typedef struct Program Program;
typedef struct IdentifierList IdentifierList;
typedef struct Declarations Declarations;
typedef struct Type Type;
typedef struct ArrayType ArrayType;
typedef struct SubDeclarations SubDeclarations;
typedef struct SubprogDeclaration SubprogDeclaration;
typedef struct SubprogramHead SubprogramHead;
typedef struct ParameterList ParameterList;
typedef struct StatementList StatementList;
typedef struct Statement Statement;
typedef struct Variable Variable;
typedef struct ProcStatement ProcStatement;
typedef struct ExpressionList ExpressionList;
typedef struct Expression Expression;
typedef struct SimpleExpression SimpleExpression;
typedef struct Term Term;
typedef struct Factor Factor;
typedef struct FunctionRule FunctionRule;
typedef struct ProcedureRule ProcedureRule;
typedef struct Assignment Assignment;
typedef struct IfThen IfThen;
typedef struct WhileDo WhileDo;
typedef struct ForTo ForTo;
typedef struct VariableExpression VariableExpression;
typedef struct ProcStatementExpressionList ProcStatementExpressionList;
typedef struct SignedTerm SignedTerm;
typedef struct Addition Addition;
typedef struct Multiplication Multiplication;
typedef struct FactorExpressionList FactorExpressionList;
typedef struct RelationalExpression RelationalExpression;

struct Program {
	char* identifier;
	Declarations* declarations;
	SubDeclarations* sub_declarations;
	StatementList* compound_statement;
};

struct IdentifierList {
	char* identifier;
	IdentifierList* next;
};

struct Declarations {
	IdentifierList* identifier_list;
	Type* type;
	Declarations* next;
};

struct Type {
	int node_type;
	union {
		int standard_type;
		ArrayType* array_type;
	} type;
};

struct ArrayType {
	int from;
	int to;
	int standard_type;
};

struct SubDeclarations {
	SubprogDeclaration* subprog_declaration;
	SubDeclarations* next;
};

struct SubprogDeclaration {
	SubprogramHead* subprogram_head;
	Declarations* declarations;
	StatementList* compound_statement;
};

struct SubprogramHead {
	int node_type;
	union {
		struct FunctionRule {
			char* identifier;
			ParameterList* arguments;
			int standard_type;
		} *function_rule;
		struct ProcedureRule {
			char* identifier;
			ParameterList* arguments;
		} *procedure_rule;
	} subprogram_head;
};

struct ParameterList {
	IdentifierList* identifier_list;
	Type* type;
	ParameterList* next;
};

struct StatementList {
	Statement* statement;
	StatementList* next;
};

struct Statement {
	int node_type;
	union {
		struct Assignment {
			Variable* variable;
			Expression* expression;
		} *assignment;
		ProcStatement* proc_statement;
		StatementList* compound_statement;
		struct IfThen {
			Expression* expression;
			Statement* statement;
			Statement* else_clause;
		} *if_then;
		struct WhileDo {
			Expression* expression;
			Statement* statement;
		} *while_do;
		struct ForTo {
			char* identifier;
			Expression* expression1;
			Expression* expression2;
			Statement* statement;
		} *for_to;
		char* identifier;
	} statement;
};

struct Variable {
	int node_type;
	union {
		char* identifier;
		struct VariableExpression {
			char* identifier;
			Expression* expression;
		} *expression;
	} variable;
};

struct ProcStatement {
	int node_type;
	union {
		char* identifier;
		struct ProcStatementExpressionList {
			char* identifier;
			ExpressionList* expression_list;
		} *expression_list;
	} proc_statement;
};

struct ExpressionList {
	Expression* expression;
	ExpressionList* next;
};

struct Expression {
	int node_type;
	union {
		SimpleExpression* simple_expression;
		struct RelationalExpression {
			SimpleExpression* simple_expression;
			int relop;
			Expression* expression;
		} *relation;
	} expression;
};

struct SimpleExpression {
	int node_type;
	union {
		Term* term;
		struct SignedTerm {
			int sign;
			Term* term;
		} *signed_term;
		struct Addition {
			SimpleExpression* simple_expression;
			int addop;
			Term* term;
		} *addition;
	} simple_expression;
};

struct Term {
	int node_type;
	union {
		Factor* factor;
		struct Multiplication {
			int mulop;
			Factor* factor;
			Term* term;
		} *multiplication;
	} term;
};

struct Factor {
	int node_type;
	union {
		struct FactorExpressionList {
			char* identifier;
			ExpressionList* expression_list;
		} *expression_list;
		Variable* variable;
		int integer;
		float real;
		Expression* expression;
		Factor* factor;
	} factor;
};
