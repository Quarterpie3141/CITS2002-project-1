//  CITS2002 Project 1 2024
//  Student1:   23783481 Prashan Wijesinghe 
//  Student2:   23180621 William Craig
//  Platform:   Apple

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define MAX_IDENTIFIER_LENGTH 12
#define MAX_REAL_LENGTH 16
#define MAX_IDENTIFIERS 50
#define INITIAL_ARRAY_SIZE 1024
#define INITAL_ROOT_NODE_CAPACITY 2
//todo - close fopen calls and free() malloc'd nodes

int syntaxErrorFlag = 0;

// these are the types declarations for the lexer and tokeniser

typedef enum {
    TOKEN_IDENTIFIER,
    TOKEN_REAL,
    TOKEN_FUNCTION,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_MULT,
    TOKEN_DIV,
    TOKEN_ASSIGN,    // <-
    TOKEN_LPAREN,    // (
    TOKEN_RPAREN,    // )
    TOKEN_COMMA,
    TOKEN_EOF,
    TOKEN_TAB,
    TOKEN_COMMENT, // #
    TOKEN_UNKNOWN    // for syntax
} TokenType;
typedef struct {
    TokenType type;
    char* lexeme;  // The string representation of the token
    int line;      // Line number for error reporting
    int position;
} Token;

// end of lexer & tokeniser types 

// these are the type declarations for the nodes in the abstract syntax tree

typedef enum { // types of ast nodes(the structs for each type are in the same order as these types inside the ASTNode type)
    NODE_PROGRAM,
    NODE_PROGRAM_ITEMS,
    NODE_FUNCTION,
    NODE_STATEMENT,
    NODE_ASSIGNMENT,
    NODE_PRINT,
    NODE_RETURN,
    NODE_EXPRESSION,
    NODE_TERM,
    NODE_FACTOR,
    NODE_FUNCTION_CALL
} ASTNodeType;

typedef enum { // types for a factor node 
        FACTOR_CONSTANT,
        FACTOR_IDENTIFIER,
        FACTOR_FUNCTION_CALL,
        FACTOR_EXPRESSION
} FactorType;

typedef enum {  // types for a program item nods
        PROGRAM_STATEMENT,
        PROGRAM_FUNCTION
} ProgramItemType;

typedef enum { // types for a statement node
        STATEMENT_ASSIGNMENT,
        STATEMENT_PRINT,
        STATEMENT_RETURN,
        STATEMENT_FUNCTION_CALL
} StatementType;

typedef struct ASTNode ASTNode;
typedef struct ASTNode {
    ASTNodeType type;
    ASTNode* next;
    union {                 // union allows you to have multiple, structs in this case, under one memory address. 
                            // so each node has only one struct. but that struct can be any of the structs under union{}
        
         // root node
        struct {
            struct ASTNode** programItems; //(optional) double astrik to aloow the root node to store an arry of more nodes
        } program;

        struct {
            ProgramItemType programType; //
            union{
                struct ASTNode* statement;
                struct ASTNode* function;
            };
        } programItems;

        struct {
            char* functionIdentifier;
            char** parameterIdentifiers; // (optional)
            struct ASTNode* statements;
        } function;

        //need to implement
        struct {
            StatementType statementType;
            struct ASTNode* statement;
        } statement;

        // x <- 5, this node has an identifier(x) and an expression node(5)
        struct {
            char* identifierName;   // (required)
            struct ASTNode* expression; // (required)
        } assignment;

        // print(2.5), this node just has another expression node(2.5)
        struct {
            struct ASTNode* expression; //(required)
        } printStatement;

        // return(a+b), this node just has an expression node(a+b)
        struct {
            struct ASTNode* expression; //(required)
        } returnStatement;

        // 5+2, this node has a term node(5), an operator(+) and an expression(2) node
        struct {
            struct ASTNode* term; // left hand side of expression (required)
            struct ASTNode* expression; // right hand side of expression (optional)
            char operator; // middle of expression  (optional)
        } expression;

        // 6*4, this node has a factor node(6), an operator(*) and anoter term node(4)
        struct {
            struct ASTNode* factor; // left hand side of expression (required)
            struct ASTNode* term;   // right hand side of expression (optional)
            char operator;          // middle of expression (optional)
        } term;

        // factoes 
        struct {
             FactorType factorType;            // what the factor is(constant, identifier, functional call, expression)
             union {
                char* constantValue;             // for when the factor is a real constant
                char* identifierName;          // For when the factor is a variable
                struct {
                    char* function_name;        // for when the factor is a function call
                    struct ASTNode* args;       // list of expressions for the function
                } functionCall;                
                struct ASTNode* expression;     // for when the factor is another expression
            };
        } factor;

        //add(2,4), this node has an identifier and a list of expressions
        struct {
            char* identifierName;
            struct ASTNode* expressions;
        } functionCall;

    };
} ASTNode;

typedef enum {
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION
} SymbolType;

typedef struct Symbol {
    char* name;
    SymbolType type;
} Symbol;

// end of AST types

Symbol* symbolList[MAX_IDENTIFIERS];

// symbols are just already declared identifiers, they can be variables or function calls
void addSymbol(char* name, SymbolType type) {
    static int currentSymbol = 0; // keep track of the current number of symbols, TODO: warn the user when they use too many identifiers
    Symbol* symbol = malloc(sizeof(Symbol));
    (*symbol).name = name; //dup
    (*symbol).type = type;
    symbolList[currentSymbol++] = symbol;
}

// loop through the array and find a symbol with the same name, return null otherwise
Symbol* findSymbol(char* name) {
    int index = 0;
    while (index < MAX_IDENTIFIERS && symbolList[index] != NULL) {
        Symbol* current = symbolList[index];
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        index++;
    }
    return NULL; // Symbol not found
}



//abstract syntax tree parsing functions

ASTNode* parseProgramItems(Token* tokenList, int* currentToken);
ASTNode* parseFunction(Token* tokenList, int* currentToken);
ASTNode* parseStatementList(Token* tokenList, int* currentToken);
ASTNode* parseStatement(Token* tokenList, int* currentToken);
ASTNode* parseAssignment(Token* tokenList, int* currentToken);
ASTNode* parsePrintStatement(Token* tokenList, int* currentToken);
ASTNode* parseReturnStatement(Token* tokenList, int* currentToken);
ASTNode* parseExpression(Token* tokenList, int* currentToken);
ASTNode* parseTerm(Token* tokenList, int* currentToken);
ASTNode* parseFactor(Token* tokenList, int* currentToken);
ASTNode* parseFunctionCall(Token* tokenList, int* currentToken);
ASTNode* parseArgumentList(Token* tokenList, int* currentToken);
char** parseParameterList(Token* tokenList, int* currentToken, int hasParentheses);


// if a 'function' token is found `parseFunction` will take care of
// consuming all the tokens related to the function and checking syntax
ASTNode* parseFunction(Token* tokenList, int* currentToken) {
    ASTNode* functionNode = malloc(sizeof(ASTNode));
    if (functionNode == NULL) {
        fprintf(stderr, " ! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    functionNode->type = NODE_FUNCTION;

    // expect 'function' keyword
    if (tokenList[*currentToken].type != TOKEN_FUNCTION) {
        fprintf(stderr, "! Expected 'function' keyword\n");
        syntaxErrorFlag = 1;
        return NULL;
    }
    (*currentToken)++;

    // expect function identifier
    if (tokenList[*currentToken].type != TOKEN_IDENTIFIER) {
        fprintf(stderr, "! Expected function name\n");
        syntaxErrorFlag = 1;
        return NULL;
    }

    functionNode->function.functionIdentifier = tokenList[*currentToken].lexeme;
    addSymbol(tokenList[*currentToken].lexeme, SYMBOL_FUNCTION);
    (*currentToken)++;

    // check for the '('
    int hasParentheses = 0;
    if (tokenList[*currentToken].type == TOKEN_LPAREN) {
        hasParentheses = 1;
        (*currentToken)++; // skip the '('
    }

    // parse parameters
    functionNode->function.parameterIdentifiers = parseParameterList(tokenList, currentToken, hasParentheses);

    // if there was '(', expect a ')'
    if (hasParentheses == 1) {
        if (tokenList[*currentToken].type != TOKEN_RPAREN) {
            fprintf(stderr, "! Expected ')' after function parameters\n");
            syntaxErrorFlag = 1;
            return NULL;
        }
        (*currentToken)++; // skip ')'
    }

    // parse function body (statements)
    functionNode->function.statements = parseStatementList(tokenList, currentToken);

    return functionNode;
}

// this function is the first to start actually parsing items, each program item (line or gorup of lines) in the top
// the top level of the program is either a statement or a funciton
ASTNode* parseProgramItems(Token* tokenList, int* currentToken) {
    // assign mem
    ASTNode* programItem = malloc(sizeof(ASTNode));
    if (programItem == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    
    Token token = tokenList[*currentToken];

    if (token.type == TOKEN_FUNCTION) {
        // construct a function node
        programItem->type = NODE_PROGRAM_ITEMS;
        programItem->programItems.programType = PROGRAM_FUNCTION;
        programItem->programItems.function = parseFunction(tokenList, currentToken);
    } else {
        // construct a statement node
        programItem->type = NODE_PROGRAM_ITEMS;
        programItem->programItems.programType = PROGRAM_STATEMENT;
        programItem->programItems.statement = parseStatement(tokenList, currentToken);
    }

    return programItem;
}

// for function nodes, and functioncall nodes, this function parses the list of parameters provided
char** parseParameterList(Token* tokenList, int* currentToken, int hasParentheses) {
    char** parameters = malloc(MAX_IDENTIFIERS * sizeof(char*));
    int paramCount = 0;

    // if no parentheses, and the next token is not an identifier, there are no parameters
    if (hasParentheses == 0 && tokenList[*currentToken].type != TOKEN_IDENTIFIER) {
        parameters[paramCount] = NULL; // Null-terminate the list
        return parameters;
    }

    while (tokenList[*currentToken].type == TOKEN_IDENTIFIER) {
        parameters[paramCount++] = tokenList[*currentToken].lexeme; //dup
        addSymbol(tokenList[*currentToken].lexeme, SYMBOL_VARIABLE);
        (*currentToken)++;

        // if parameters are within parentheses, check for comma
        if (hasParentheses == 1) {
            if (tokenList[*currentToken].type == TOKEN_COMMA) {
                (*currentToken)++; // Skip ','
            } else {
                // no comma, break if next token is ')'
                if (tokenList[*currentToken].type == TOKEN_RPAREN) {
                    break;
                } else {
                    fprintf(stderr, "Expected ',' or ')' in parameter list\n");
                    syntaxErrorFlag = 1;
                    break;
                }
            }
        } else {
            // if no parentheses, stop parsing if next token is not an identifier
            if (tokenList[*currentToken].type != TOKEN_IDENTIFIER) {
                break;
            }
        }
    }

    parameters[paramCount] = NULL; // null-terminate the list
    return parameters;
}


// this node is secretly a linked list, since function bodies, and the top level program
// can have multiple statements or functions. the linked list ensures that all statements are accounted for
// TODO: consider tab indentations
ASTNode* parseStatementList(Token* tokenList, int* currentToken) {
    ASTNode* head = NULL;
    ASTNode* tail = NULL;

    // assume that the function body starts with a TAB token
    if (tokenList[*currentToken].type != TOKEN_TAB) {
        fprintf(stderr, "! Expected indentation in function body\n");
        syntaxErrorFlag = 1;
        return NULL;
    }

    // consume the initial TAB token
    (*currentToken)++;

    while (tokenList[*currentToken].type != TOKEN_EOF &&
           tokenList[*currentToken].type != TOKEN_FUNCTION &&
           tokenList[*currentToken].type != TOKEN_TAB) { // assuming dedent is represented by lack of TAB
        ASTNode* statement = parseStatement(tokenList, currentToken);
        if (statement == NULL) {
            // handle parsing error
            return NULL;
        }

        if (head == NULL) {
            head = statement;
            tail = statement;
        } else {
            tail->next = statement;
            tail = statement;
        }

        // check for TAB token at the beginning of the next line
        if (tokenList[*currentToken].type == TOKEN_TAB) {
            (*currentToken)++; // Consume TAB
        } else {
            break; // end of indented block
        }
    }

    return head;
}

// there are four types of statements, ones that start with an identifier( these can be assignments, or function calls)
// and ones that start with a reserved keyword(print, return)
ASTNode* parseStatement(Token* tokenList, int* currentToken) {
    Token token = tokenList[*currentToken];
    //allocate mem
    ASTNode* statementNode = malloc(sizeof(ASTNode));
    if (statementNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    statementNode->type = NODE_STATEMENT;

    if (token.type == TOKEN_IDENTIFIER) {
        // could be an assignment or function call
        if (tokenList[*currentToken + 1].type == TOKEN_ASSIGN) {
            // assignment
            statementNode->statement.statementType = STATEMENT_ASSIGNMENT;
            statementNode->statement.statement = parseAssignment(tokenList, currentToken);
            fprintf(stderr, "Entered assign with currentToken: %d\n", *currentToken);
            
        } else {
            // function call
            statementNode->statement.statementType = STATEMENT_FUNCTION_CALL;
            statementNode->statement.statement = parseFunctionCall(tokenList, currentToken);
        }
    } else if (token.type == TOKEN_PRINT) {
        // print statement
        statementNode->statement.statementType = STATEMENT_PRINT;
        statementNode->statement.statement = parsePrintStatement(tokenList, currentToken);
    } else if (token.type == TOKEN_RETURN) {
        // return statement
        statementNode->statement.statementType = STATEMENT_RETURN;
        statementNode->statement.statement = parseReturnStatement(tokenList, currentToken);
    } else {
        fprintf(stderr, "! Unexpected token '%s' in statement\n", token.lexeme);
        syntaxErrorFlag = 1;
        *currentToken = *currentToken + 1;
        return NULL;
    }

    return statementNode;
}

// pretty straightforward, just checks that the assignment is structured correctly
ASTNode* parseAssignment(Token* tokenList, int* currentToken) {
    //assign mem 
    ASTNode* assignmentNode = malloc(sizeof(ASTNode));
    if (assignmentNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    assignmentNode->type = NODE_ASSIGNMENT;

    

    // expect an identifier and add it to the list of known symbols
    assignmentNode->assignment.identifierName = tokenList[*currentToken].lexeme;
    
    addSymbol(tokenList[(*currentToken)].lexeme, SYMBOL_VARIABLE);
    (*currentToken)++;

    // expect a '<-'
    if (tokenList[*currentToken].type != TOKEN_ASSIGN) {
        fprintf(stderr, "! Expected '<-' in assignment\n");
        syntaxErrorFlag = 1;
        return NULL;
    }
    (*currentToken)++;

    // parse expression
    assignmentNode->assignment.expression = parseExpression(tokenList, currentToken);

    return assignmentNode;
}

// parses a print
ASTNode* parsePrintStatement(Token* tokenList, int* currentToken) {
    // allocate memory for the print node
    ASTNode* printNode = malloc(sizeof(ASTNode));
    if (printNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    printNode->type = NODE_PRINT;

    (*currentToken)++;

    // check if the next token is '('
    if (tokenList[*currentToken].type == TOKEN_LPAREN) {
        (*currentToken)++; // skip '('

        // parse the expression
        printNode->printStatement.expression = parseExpression(tokenList, currentToken);

        // expect a ')'
        if (tokenList[*currentToken].type != TOKEN_RPAREN) {
            fprintf(stderr, "! Expected closing ')' after expression in print statement\n");
            syntaxErrorFlag = 1;
            return NULL;
        }
        (*currentToken)++; // skip the ')'
    } else {
        // parse the expression directly without parentheses
        printNode->printStatement.expression = parseExpression(tokenList, currentToken);
    }

    return printNode;
}

//parses a return, assumes that return can only return a single value wil break if it returns more than 1
ASTNode* parseReturnStatement(Token* tokenList, int* currentToken) {
    ASTNode* returnNode = malloc(sizeof(ASTNode));
    if (returnNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    returnNode->type = NODE_RETURN;

    // expect the 'return' keyword
    (*currentToken)++;

    // parse expression
    returnNode->returnStatement.expression = parseExpression(tokenList, currentToken);

    return returnNode;
}

//a function call can be identified since it is a just a identifier with a list of parameters() so we just chack for a parentheses
ASTNode* parseFunctionCall(Token* tokenList, int* currentToken) {
    fprintf(stderr, "Entered parseFunctionCall with currentToken: %d\n", *currentToken);
    ASTNode* functionCallNode = malloc(sizeof(ASTNode));
    if (functionCallNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    functionCallNode->type = NODE_FUNCTION_CALL;

    // expect identifier
    functionCallNode->functionCall.identifierName = tokenList[*currentToken].lexeme;
    (*currentToken)++;

    // expect '('
    if (tokenList[*currentToken].type != TOKEN_LPAREN) {
        fprintf(stderr, "! Expected '(' after function name\n");
        syntaxErrorFlag = 1;
        return NULL;
    }
    (*currentToken)++;

    // parse arguments if there are any
    functionCallNode->functionCall.expressions = parseArgumentList(tokenList, currentToken);

    // expect a ')'
    if (tokenList[*currentToken].type != TOKEN_RPAREN) {
        fprintf(stderr, "! Expected ')' after function arguments\n");
        syntaxErrorFlag = 1;
        return NULL;
    }
    (*currentToken)++;

    return functionCallNode;
}

//for functions that have multiple arguments i.e. `sum(x, 24+y, z*x)`
//this function will parse all of the arguments as another linked list, so we can navigate through them when generating the code
ASTNode* parseArgumentList(Token* tokenList, int* currentToken) {
    ASTNode* head = NULL;
    ASTNode* tail = NULL;

    //check if the argument list is empty
    if (tokenList[*currentToken].type == TOKEN_RPAREN) {
        return NULL;
    }

    //parse first argument
    ASTNode* arg = parseExpression(tokenList, currentToken);
    if (arg == NULL) {
        return NULL;
    }
    head = arg;
    tail = arg;

    //keep parsing arguments
    while (tokenList[*currentToken].type == TOKEN_COMMA) {
        (*currentToken)++; //skip the ','
        arg = parseExpression(tokenList, currentToken);
        if (arg == NULL) {
            return NULL;
        }
        tail->next = arg;
        tail = arg;
    }

    return head;
}

// this function jsut parses the left and right hand side of an expression
ASTNode* parseExpression(Token* tokenList, int* currentToken) {
    // parse a term
    ASTNode* left = parseTerm(tokenList, currentToken);

    Token token = tokenList[*currentToken];

    // handle '+' and '-' ops
    while (token.type == TOKEN_PLUS || token.type == TOKEN_MINUS) {
        ASTNode* exprNode = malloc(sizeof(ASTNode));
        if (exprNode == NULL) {
            fprintf(stderr, "! Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }

        exprNode->type = NODE_EXPRESSION;
        exprNode->expression.term = left; // left hand side of the expression
        exprNode->expression.operator = token.lexeme[0];
        (*currentToken)++;

        // parse the right side of the expression
        exprNode->expression.expression = parseTerm(tokenList, currentToken);

        left = exprNode;
        token = tokenList[*currentToken];
    }

    return left;
}

//parses the left and right and side of a term, a term is what is either on the left or right hand
//side of an expression
ASTNode* parseTerm(Token* tokenList, int* currentToken) {
    // parse a factor, similar to a expression, a factor is whats on the left or right hand side 
    // of a term
    ASTNode* left = parseFactor(tokenList, currentToken);

    Token token = tokenList[*currentToken];

    // Handle '*' and '/' operators
    while (token.type == TOKEN_MULT || token.type == TOKEN_DIV) {
        ASTNode* termNode = malloc(sizeof(ASTNode));
        if (termNode == NULL) {
            fprintf(stderr, "! Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }

        termNode->type = NODE_TERM;
        termNode->term.factor = left;
        termNode->term.operator = token.lexeme[0];
        (*currentToken)++;

        // Parse the right side of the term
        termNode->term.term = parseFactor(tokenList, currentToken);

        left = termNode;
        token = tokenList[*currentToken];
    }

    return left;
}

 // parse a factor, similar to a expression, a factor is whats on the left or right hand side 
 // of a term
ASTNode* parseFactor(Token* tokenList, int* currentToken) {
    Token token = tokenList[*currentToken];
    //assign mem
    ASTNode* factorNode = malloc(sizeof(ASTNode));
    if (factorNode == NULL) {
        fprintf(stderr, "! Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    factorNode->type = NODE_FACTOR;

    if (token.type == TOKEN_REAL) {
        // constant value
        factorNode->factor.factorType = FACTOR_CONSTANT;
        factorNode->factor.constantValue = token.lexeme;
        (*currentToken)++;
    } else if (token.type == TOKEN_IDENTIFIER) {
        // the council(symbol table) will decide its fate
        Symbol* symbol = findSymbol(token.lexeme);
        if (symbol == NULL) {
            fprintf(stderr, "! Error: Undefined identifier '%s'\n", token.lexeme);
            syntaxErrorFlag = 1;
            return NULL;
        }

        if (symbol->type == SYMBOL_FUNCTION && tokenList[*currentToken + 1].type == TOKEN_LPAREN) {
            // function call
            factorNode->factor.factorType = FACTOR_FUNCTION_CALL;
            factorNode->factor.functionCall.function_name = token.lexeme;
            (*currentToken) += 2; // skip identifier and '('

            // parse arguments
            factorNode->factor.functionCall.args = parseArgumentList(tokenList, currentToken);

            // expect ')'
            if (tokenList[*currentToken].type != TOKEN_RPAREN) {
                fprintf(stderr, "! Expected ')' after function arguments\n");
                syntaxErrorFlag = 1;
                return NULL;
            }
            (*currentToken)++;
        } else if (symbol->type == SYMBOL_VARIABLE) {
            // variable
            factorNode->factor.factorType = FACTOR_IDENTIFIER;
            factorNode->factor.identifierName = token.lexeme; //dup
            (*currentToken)++;
        } else {
            fprintf(stderr, "! Error: '%s' is not a variable or function\n", token.lexeme);
            syntaxErrorFlag = 1;
            return NULL;
        }
    } else if (token.type == TOKEN_LPAREN) {
        // parenthesized expression
        (*currentToken)++; // skip '('
        factorNode->factor.factorType = FACTOR_EXPRESSION;
        factorNode->factor.expression = parseExpression(tokenList, currentToken);

        // expect ')'
        if (tokenList[*currentToken].type != TOKEN_RPAREN) {
            fprintf(stderr, "! Expected ')' after expression\n");
            syntaxErrorFlag = 1;
            return NULL;
        }
        (*currentToken)++;
    } else {
        fprintf(stderr, "! Unexpected token '%s' in factor\n", token.lexeme);
        syntaxErrorFlag = 1;
        return NULL;
    }

    return factorNode;
}

// this function initialises the AST, it will make the root node(NODE_PROGRAM) that all other nodes will
// eventually link back to. since there can be multiple program items in the main body of the program
// it also stores a linked list of program items, since its the root node we also dynamically allocate
// memory for the whole tree here

ASTNode* constructAST(Token* tokenList) {
    fprintf(stderr, "@ Starting to construct AST");
    static int firstProgram = 0;
    if(firstProgram == 0){
        int currentToken = 0;
        firstProgram = 1;
        int capacity = INITAL_ROOT_NODE_CAPACITY;
        int amountOfProgramItems = 0;

        // allocate space for the root node
        ASTNode* ast = malloc(sizeof(ASTNode));

        if (ast == NULL) {
            fprintf(stderr, "! Failed to allocate memory for root node\n");
            exit(EXIT_FAILURE);
        }
        //root node
        ast->type = NODE_PROGRAM;
        ast->next = NULL;

        ast->program.programItems = malloc(capacity * sizeof(ASTNode*)); // anitial allocation for program items
        if (ast->program.programItems == NULL) {
            fprintf(stderr, "! Failed to allocate memory for program items\n");
            exit(EXIT_FAILURE);
        }

        while (tokenList[currentToken].type != TOKEN_EOF) {
            // eeallocate memory for more program items when its needed
            if (amountOfProgramItems >= capacity) {
                capacity *= 2;
                ast->program.programItems = realloc(ast->program.programItems, capacity * sizeof(ASTNode*));
                if (ast->program.programItems == NULL) {
                    fprintf(stderr, "! Failed to reallocate memory for program items\n");
                    exit(EXIT_FAILURE);
                }
            }

            // parse the next program item and add it to the array of nodes
            ast->program.programItems[amountOfProgramItems++] = parseProgramItems(tokenList, &currentToken);
        }

        // add null to the program items array
        ast->program.programItems[amountOfProgramItems] = NULL;

        return ast;
    }
    return NULL;
}

// end ast parsing functions

// start code generation funcions
//these functions are similar to the parsing ones, just in reverse

void generateCode(ASTNode* ast, const char* outputFilename);
void generateProgram(ASTNode* node, FILE* outputFile);
void generateProgramItem(ASTNode* node, FILE* outputFile);
void generateFunction(ASTNode* node, FILE* outputFile);
void generateStatement(ASTNode* node, FILE* outputFile);
void generateAssignment(ASTNode* node, FILE* outputFile);
void generatePrintStatement(ASTNode* node, FILE* outputFile);
void generateReturnStatement(ASTNode* node, FILE* outputFile);
void generateExpression(ASTNode* node, FILE* outputFile);
void generateTerm(ASTNode* node, FILE* outputFile);
void generateFactor(ASTNode* node, FILE* outputFile);
void generateFunctionCall(ASTNode* node, FILE* outputFile);
void generateFunctionCallInExpression(ASTNode* factor, FILE* outputFile);
void generateDeclarations(FILE* outputFile);

int indentLevel = 0; // for indenting the c code for readability
void increaseIndent() {
    indentLevel++;
}

void decreaseIndent() {//^
    if (indentLevel > 0) {
        indentLevel--;
    }
}

void emitIndentation(FILE* outputFile) {
    for (int i = 0; i < indentLevel; i++) {
        fprintf(outputFile, "    "); // 4 spaces per indent level
    }
}

// this functions similarly to the CrateAST function where this is the entery point into the recursion
void generateCode(ASTNode* ast, const char* outputFilename) {
    FILE* outputFile = fopen(outputFilename, "w");
    if (outputFile == NULL) {
        fprintf(stderr, "! Failed to open output file for writing.\n");
        exit(EXIT_FAILURE);
    }

    // add necessary headers
    fprintf(outputFile, "#include <stdio.h>\n\n");
    // add all the variable declarations(need to change with scope)
    generateDeclarations(outputFile);

    // generate code for the AST
    generateProgram(ast, outputFile);

    // start the main function
    fprintf(outputFile, "int main() {\n");
    increaseIndent();

    // Generate code for top-level statements, i.e. loop over the linked list of program items in the tree and start a recursion
    // for each one
    if (ast->type == NODE_PROGRAM) {
        for (int i = 0; ast->program.programItems[i] != NULL; i++) {
            ASTNode* programItem = ast->program.programItems[i];
            if (programItem->programItems.programType == PROGRAM_STATEMENT) {
                generateStatement(programItem->programItems.statement, outputFile);
            } else if (programItem->programItems.programType == PROGRAM_FUNCTION) {
                // function definitions are already generated above
            }
        }
    }

    decreaseIndent();
    fprintf(outputFile, "    return 0;\n");
    fprintf(outputFile, "}\n");

    fclose(outputFile);
}

// generate code for the entire program this just acts liek a proxy for gen statements
void generateProgram(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_PROGRAM) {
        return;
    }

    // generate code for each program item
    for (int i = 0; node->program.programItems[i] != NULL; i++) {
        generateProgramItem(node->program.programItems[i], outputFile);
    }
}

// generates program items
void generateProgramItem(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_PROGRAM_ITEMS) {
        return;
    }

    if (node->programItems.programType == PROGRAM_FUNCTION) {
        generateFunction(node->programItems.function, outputFile);
    }

}

// generates the functions, also has to parse a list of arguments, we assume
// that each argument is a double
void generateFunction(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_FUNCTION) {
        return;
    }

    // add function identifier
    fprintf(outputFile, "double %s(", node->function.functionIdentifier);

    // add parameters / parse list of arguments
    char** params = node->function.parameterIdentifiers;
    for (int i = 0; params && params[i] != NULL; i++) {
        fprintf(outputFile, "double %s", params[i]);
        if (params[i + 1] != NULL) {
            fprintf(outputFile, ", ");
        }
    }
    fprintf(outputFile, ") {\n");

    increaseIndent(); // make it look nice

    // add  function body ( linked list of statements)
    ASTNode* statement = node->function.statements;
    while (statement != NULL) {
        generateStatement(statement, outputFile);
        statement = statement->next;
    }

    decreaseIndent();
    fprintf(outputFile, "return 0; \n");
    fprintf(outputFile, "}\n\n");

    
}

// generates a statement based on what type of statement it is. thi sfunction itself
// dosent generate it, it just acts liek a revrse proxy and hands the node to the
// respective functions
void generateStatement(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_STATEMENT) {
        return;
    }

    emitIndentation(outputFile);

    switch (node->statement.statementType) {
        case STATEMENT_ASSIGNMENT:
            generateAssignment(node->statement.statement, outputFile);
            break;

        case STATEMENT_PRINT:
            generatePrintStatement(node->statement.statement, outputFile);
            break;

        case STATEMENT_RETURN:
            generateReturnStatement(node->statement.statement, outputFile);
            break;

        case STATEMENT_FUNCTION_CALL:
            generateFunctionCall(node->statement.statement, outputFile);
            fprintf(outputFile, ";\n");
            break;

        default:
            fprintf(stderr, "! Unknown statement type in code generation.\n");
            break;
    }
}

// generate code for an assignment statement
void generateAssignment(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_ASSIGNMENT) {
        return;
    }

    // declare the variable if first time, otherwise assign
    // TODO: keep track of identifiers
    fprintf(outputFile, "double %s = ", node->assignment.identifierName);

    generateExpression(node->assignment.expression, outputFile);
    fprintf(outputFile, ";\n");
}

// generate code for a print statement
void generatePrintStatement(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_PRINT) {
        return;
    }

    fprintf(outputFile, "printf(\"%%f\\n\", ");
    generateExpression(node->printStatement.expression, outputFile);
    fprintf(outputFile, ");\n");
}

// generate code for a return statement
void generateReturnStatement(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_RETURN) {
        return;
    }

    fprintf(outputFile, "return ");
    generateExpression(node->returnStatement.expression, outputFile);
    fprintf(outputFile, ";\n");
}

// generate code for an expression
void generateExpression(ASTNode* node, FILE* outputFile) {
    if (node == NULL) {
        return;
    }

    if (node->type == NODE_EXPRESSION) {
        generateTerm(node->expression.term, outputFile);
        fprintf(outputFile, " %c ", node->expression.operator);
        generateExpression(node->expression.expression, outputFile);
    } else if (node->type == NODE_TERM) {
        generateTerm(node, outputFile);
    } else {
        // handle other cases
        generateTerm(node, outputFile);
    }
}

// generate code for a term
void generateTerm(ASTNode* node, FILE* outputFile) {
    if (node == NULL) {
        return;
    }

    if (node->type == NODE_TERM) {
        generateFactor(node->term.factor, outputFile);
        fprintf(outputFile, " %c ", node->term.operator);
        generateTerm(node->term.term, outputFile);
    } else if (node->type == NODE_FACTOR) {
        generateFactor(node, outputFile);
    } else {
        // Handle other cases
        generateFactor(node, outputFile);
    }
}

// generate code for a factor same deal as the statments, acts liek a reverse proxy
void generateFactor(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_FACTOR) {
        return;
    }

    switch (node->factor.factorType) {
        case FACTOR_CONSTANT:
            fprintf(outputFile, "%f", atof(node->factor.constantValue)); // convert the char to a float, kinda not needed
            break;

        case FACTOR_IDENTIFIER:
            fprintf(outputFile, "%s", node->factor.identifierName);
            break;

        case FACTOR_FUNCTION_CALL:
            generateFunctionCallInExpression(node, outputFile);
            break;

        case FACTOR_EXPRESSION:
            fprintf(outputFile, "(");
            generateExpression(node->factor.expression, outputFile);
            fprintf(outputFile, ")");
            break;

        default:
            fprintf(stderr, "! Unknown factor type in code generation.\n");
            break;
    }
}

// generate code for a function call used within an expression
void generateFunctionCallInExpression(ASTNode* factor, FILE* outputFile) {
    if (factor == NULL) {
        return;
    }
    
    // adds arguments to the function call
    fprintf(outputFile, "%s(", factor->functionCall.expressions);
    ASTNode* arg = factor->factor.functionCall.args;
    while (arg != NULL) {
        generateExpression(arg, outputFile);
        if (arg->next != NULL) {
            fprintf(outputFile, ", ");
        }
        arg = arg->next;
    }

    fprintf(outputFile, ")");
}

// generate code for a function call statement
void generateFunctionCall(ASTNode* node, FILE* outputFile) {
    if (node == NULL || node->type != NODE_FUNCTION_CALL) {
        return;
    }


    fprintf(outputFile, "%s (", node->functionCall.identifierName);
    // same as the function call
    ASTNode* arg = node->functionCall.expressions;
    while (arg != NULL) {
        generateExpression(arg, outputFile);
        if (arg->next != NULL) {
            fprintf(outputFile, ", ");
        }
        arg = arg->next;
    }

    fprintf(outputFile, ")");
}

void generateDeclarations(FILE* outputFile) {
    // iterate over the symbol list and generate declarations
    for (int i = 0; i < MAX_IDENTIFIERS; i++) {
        Symbol* symbol = symbolList[i];
        if (symbol == NULL) {
            // reached the end of the symbol list
            fprintf(outputFile, "\n");
            break;
        }

        if (symbol->type == SYMBOL_VARIABLE) {
            // declare the variable at the top of main
            fprintf(outputFile, "double %s;\n", symbol->name);
        } else if (symbol->type == SYMBOL_FUNCTION) {
        }
    }
}

//end code generation functions

int validateFileExt(const char *fileName){
    char *ext  = strrchr(fileName, '.'); // grab the mem address of where the extention begins in the file name

    if(!ext || strcmp(ext, ".ml") != 0){    // check if the file dosent have an extention or has the incorrect extention 
        fprintf(stderr, "! Provided file is not a .ml file\n");
        return 1;
    }
    return 0;
}


Token createToken(TokenType type, const char *lexeme, int line, int position) {
    Token token;    // instantiates a new token struct and fills out the values with the ones provided
    token.type = type;
    token.lexeme = strdup(lexeme);//dup
    token.line = line;
    token.position = position;
    return token;
}

void addToken(Token *tokens, int *amountOfTokens, Token token) {
    tokens[*amountOfTokens] = token;    // adds the provided token to a provided array of tokens
    (*amountOfTokens)++;    
}

void advanceCharacter(FILE *file, int *currentCharacter, int *currentLine, int *currentPosition) {
    *currentCharacter = fgetc(file);    //gets the next character in the file
    (*currentPosition)++;               // if the next character is a new line then reset the position and increment the line
    if (*currentCharacter == '\n') {    //else just increment the position
        //(*currentLine++);
        *currentPosition = 0;
    }
}

TokenType getTokenType(char* identifier){
    if(strcmp(identifier, "function") == 0) return TOKEN_FUNCTION;
    if(strcmp(identifier, "print") == 0) return TOKEN_PRINT;
    if(strcmp(identifier, "return") == 0) return TOKEN_RETURN;
    // to do fill out all tokens
    return TOKEN_IDENTIFIER;
}

Token* lexer(FILE* file){   //this entire function is pretty much adapted from llvm Kaleidoscope
    static int currentPosition = 0;
    static int currentLine = 1;
    Token* tokens = malloc(sizeof(Token) * INITIAL_ARRAY_SIZE); //create an array of tokens in memory, todo: dynamic mem allocation
    static int amountOfTokens = 0;

    int currentCharacter = fgetc(file);
    currentPosition++;

    while (currentCharacter != EOF){
        // can optimise this, todo
        if (currentCharacter == '\n' || currentCharacter == '\r') {
            currentLine++;
            currentPosition = 0;
            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == ' ') { // skip tokeniasation of spaces, new lines, and carrigae returns
            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        
        //check for identifiers to do add new line stuff from other function s
        if(isalpha(currentCharacter)){

            char identifier[MAX_IDENTIFIER_LENGTH];
            int identifierLegnth = 0;

            while(isalpha(currentCharacter)){ //store the entire identifier in a string
                identifier[identifierLegnth] = currentCharacter;  
                identifierLegnth++;
                
                if (identifierLegnth >= MAX_IDENTIFIER_LENGTH){
                    fprintf(stderr, "! Identifier name is too long (12 letters) on line %d at position %d\n", currentLine, currentPosition);
                    syntaxErrorFlag = 1;    //throws and error and sets a flag if the identifier name is too long
                    break;
                }
                currentCharacter = fgetc(file);
                currentPosition++;
            }
            identifier[identifierLegnth] = '\0';

            TokenType tokenType = getTokenType(identifier);

            Token identifierToken = createToken(tokenType, identifier, currentLine, currentPosition-identifierLegnth);

            addToken(tokens, &amountOfTokens, identifierToken);

            continue;
        }
        if (isdigit(currentCharacter)) {
            char numberBuffer[MAX_REAL_LENGTH];
            int numberLength = 0;
            int hasDecimalPoint = 0;

            // Store the first digit
            numberBuffer[numberLength] = currentCharacter;
            numberLength++;

            currentCharacter = fgetc(file);
            currentPosition++;

            // Continue reading digits and check for: a number or a decimal point, given that we havent already seen a decimal point
            while (isdigit(currentCharacter) || (currentCharacter == '.' && !hasDecimalPoint)) {

                if (currentCharacter == '.') {
                    hasDecimalPoint = 1;
                }

                if (numberLength < MAX_REAL_LENGTH ){ // if the real is too long, truncate it
                numberBuffer[numberLength++] = currentCharacter; //store the real constant in the buffer
                }

                currentCharacter = fgetc(file);
                currentPosition++;
            }

            numberBuffer[numberLength] = '\0'; // Null-terminate the number string

            // If a valid real number is found (it contains a decimal point)
            if (currentCharacter != '.') {
                Token realNumberToken = createToken(TOKEN_REAL, numberBuffer, currentLine, currentPosition - numberLength);
                addToken(tokens, &amountOfTokens, realNumberToken);
            } else {
                fprintf(stderr, "! Real constants can only have one decimal place. on line %d at position %d\n", currentLine, currentPosition - numberLength);
                syntaxErrorFlag = 1;
                continue;
            }

            continue;
        }
        if (currentCharacter == '+') {
            Token plusToken = createToken(TOKEN_PLUS, "+", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, plusToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == '-') {
            Token minusToken = createToken(TOKEN_MINUS, "-", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, minusToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == '*') {
            Token multToken = createToken(TOKEN_MULT, "*", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, multToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == '/') {
            Token divToken = createToken(TOKEN_DIV, "/", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, divToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == '(') {
            Token lParenToken = createToken(TOKEN_LPAREN, "(", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, lParenToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == ')') {
            Token rParenToken = createToken(TOKEN_RPAREN, ")", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, rParenToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == ',') {
            Token commaToken = createToken(TOKEN_COMMA, ",", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, commaToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        if (currentCharacter == '<') {
            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition); // if the char is a < check if the next char is a -
            if(currentCharacter == '-'){    // if it is, then create the assignment token as normal
            Token plusToken = createToken(TOKEN_ASSIGN, "<-", currentLine, currentPosition-1);
            addToken(tokens, &amountOfTokens, plusToken);

            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;

            }else{  // if a - does not follow a < then throw an error 
                fprintf(stderr, "! Incorrect assignment format (<-) on line %d at position %d\n", currentLine, currentPosition - 1);
                syntaxErrorFlag = 1;
                continue;
            }
        }
        if (currentCharacter == '#') {
            //int startPosition = currentPosition;  // Save the position of the first #

            // Continue reading until the end of the line or EOF
            while (currentCharacter != '\n' && currentCharacter != EOF ) {
                advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            }

            // do not add comments to the token list, completely ignore comments

            //just incase there is an EOF at the end of a comment
            if (currentCharacter == '\n') {
                advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            }

            continue;
        }
        if (currentCharacter == '\t') {
            Token tabToken = createToken(TOKEN_TAB, "\t", currentLine, currentPosition);
            addToken(tokens, &amountOfTokens, tabToken);

            currentCharacter = fgetc(file);
            currentPosition++;

            continue;
        }
        fprintf(stderr, "! unexpected token \"%c\" on line %d at position %d\n", currentLine, currentPosition, currentCharacter);
        advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
        continue;
    }
    Token eof_token = createToken(TOKEN_EOF, "EOF", currentLine, currentPosition);
    addToken(tokens, &amountOfTokens, eof_token);

    return tokens;
}

// don't need these for funtionality, they act as debug statements
void printIndent(int indentLevel) {
    for (int i = 0; i < indentLevel; i++) {
        fprintf(stdout,"    ");
    }
    fprintf(stdout,"@ |_");
}
void printAST(ASTNode* node, int indentLevel) {
    if (node == NULL) {
        return;
    }
    if(node->type != NODE_PROGRAM){
    printIndent(indentLevel);
    }
    switch (node->type) {
        case NODE_PROGRAM:
            fprintf(stdout,"@ Program:\n");
            for (int i = 0; node->program.programItems[i] != NULL; i++) {
                printAST(node->program.programItems[i], indentLevel + 1);
            }
            break;

        case NODE_PROGRAM_ITEMS:
            if (node->programItems.programType == PROGRAM_FUNCTION) {
                fprintf(stdout,"Function Declaration:\n");
                printAST(node->programItems.function, indentLevel + 1);
            } else if (node->programItems.programType == PROGRAM_STATEMENT) {
                fprintf(stdout,"Statement:\n");
                printAST(node->programItems.statement, indentLevel + 1);
            }
            break;

        case NODE_FUNCTION:
            fprintf(stdout,"Function '%s' with parameters:\n", node->function.functionIdentifier);
            // print parameters or none
            if (node->function.parameterIdentifiers != NULL) {
                for (int i = 0; node->function.parameterIdentifiers[i] != NULL; i++) {
                    printIndent(indentLevel + 1);
                    fprintf(stdout,"Parameter: %s\n", node->function.parameterIdentifiers[i]);
                }
            }
            // print function body
            printAST(node->function.statements, indentLevel + 1);
            break;

        case NODE_STATEMENT:
            switch (node->statement.statementType) {
                case STATEMENT_ASSIGNMENT:
                    fprintf(stdout,"Assignment Statement:\n");
                    printAST(node->statement.statement, indentLevel + 1);
                    break;

                case STATEMENT_PRINT:
                    fprintf(stdout,"Print Statement:\n");
                    printAST(node->statement.statement, indentLevel + 1);
                    break;

                case STATEMENT_RETURN:
                    fprintf(stdout,"Return Statement:\n");
                    printAST(node->statement.statement, indentLevel + 1);
                    break;

                case STATEMENT_FUNCTION_CALL:
                    fprintf(stdout,"Function Call Statement:\n");
                    printAST(node->statement.statement, indentLevel + 1);
                    break;

                default:
                    fprintf(stdout,"Unknown Statement Type\n");
                    break;
            }
            break;

        case NODE_ASSIGNMENT:
            fprintf(stdout,"Assignment to '%s':\n", node->assignment.identifierName);
            printAST(node->assignment.expression, indentLevel + 1);
            break;

        case NODE_PRINT:
            fprintf(stdout,"Print Expression:\n");
            printAST(node->printStatement.expression, indentLevel + 1);
            break;

        case NODE_RETURN:
            fprintf(stdout,"Return Expression:\n");
            printAST(node->returnStatement.expression, indentLevel + 1);
            break;

        case NODE_EXPRESSION:
            fprintf(stdout,"Expression '%c':\n", node->expression.operator);
            printAST(node->expression.term, indentLevel + 1);
            if (node->expression.expression != NULL) {
                printAST(node->expression.expression, indentLevel + 1);
            }
            break;

        case NODE_TERM:
            fprintf(stdout,"Term '%c':\n", node->term.operator);
            printAST(node->term.factor, indentLevel + 1);
            if (node->term.term != NULL) {
                printAST(node->term.term, indentLevel + 1);
            }
            break;

        case NODE_FACTOR:
            switch (node->factor.factorType) {
                case FACTOR_CONSTANT:
                    fprintf(stdout,"Constant: %s\n", node->factor.constantValue);
                    break;

                case FACTOR_IDENTIFIER:
                    fprintf(stdout,"Identifier: %s\n", node->factor.identifierName);
                    break;

                case FACTOR_FUNCTION_CALL:
                    fprintf(stdout,"Function Call:\n");
                    printIndent(indentLevel + 1);
                    fprintf(stdout,"Function Name: %s\n", node->factor.functionCall.function_name);
                    printIndent(indentLevel + 1);
                    fprintf(stdout,"Arguments:\n");
                    printAST(node->factor.functionCall.args, indentLevel + 2);
                    break;

                case FACTOR_EXPRESSION:
                    fprintf(stdout,"Nested Expression:\n");
                    printAST(node->factor.expression, indentLevel + 1);
                    break;

                default:
                    fprintf(stdout,"Unknown Factor Type\n");
                    break;
            }
            break;

        case NODE_FUNCTION_CALL:
            fprintf(stdout,"Function Call '%s' with arguments:\n", node->functionCall.identifierName);
            printAST(node->functionCall.expressions, indentLevel + 1);
            break;

        default:
            fprintf(stdout,"Unknown Node Type\n");
            break;
    }

    // ff the node has a next pointer like in the linked list nodes, loop through them
    if (node->next != NULL) {
        printAST(node->next, indentLevel);
    }
}
void printSymbolList(Symbol** symbols, int symbolCount) {
    fprintf(stdout,"@ Symbol List:\n");
    for (int i = 0; i < symbolCount; i++) {
        if (symbols[i] != NULL) {
            fprintf(stdout,"@ Name: %s, Type: %s\n",
                   symbols[i]->name,
                   symbols[i]->type == SYMBOL_VARIABLE ? "Variable" : "Function");
        }
    }
}
void printUsage() {
    fprintf(stderr, "Usage: runml <input_file.ml> [arguments...]\n");
    fprintf(stderr, "  <input_file.ml>   Path to the .ml source file to be compiled.\n");
    fprintf(stderr, "  [arguments...]     Optional arguments that the .ml program can use.\n\n");
    fprintf(stderr, "Example:\n");
    fprintf(stderr, "runml program.ml arg1 arg2\n");
}

int main(int argc, char *argv[]){
    if(argc < 2){  //check that the user provided cli arguments
        fprintf(stderr, "! At least one command line argument is required\n");
        printUsage();
        return EXIT_FAILURE;
    }

    if(validateFileExt(argv[1])){  // validate the file extention
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");   // open the file and make sure it exists
    if (file == NULL){
        fprintf(stderr, "! File does not exist");
        return EXIT_FAILURE;
    }

    Token* tokens = lexer(file);

    fclose(file);

    // this is a debug/ informational loop, it jsut prints all of the tokens in order
    // this loop is not required for functionality
    for (int i = 0; ; i++) {
        fprintf(stdout,"@ Token: %-20s Lexeme: %s (Line %d) (Position %d)\n",
            tokens[i].type == TOKEN_IDENTIFIER ? "IDENTIFIER" :
            tokens[i].type == TOKEN_REAL ? "REAL NUMBER" :
            tokens[i].type == TOKEN_FUNCTION ? "FUNCTION" :
            tokens[i].type == TOKEN_PRINT ? "PRINT" :
            tokens[i].type == TOKEN_RETURN ? "RETURN" :
            tokens[i].type == TOKEN_PLUS ? "PLUS" :
            tokens[i].type == TOKEN_MINUS ? "MINUS" :
            tokens[i].type == TOKEN_MULT ? "MULTIPLY" :
            tokens[i].type == TOKEN_DIV ? "DIVIDE" :
            tokens[i].type == TOKEN_ASSIGN ? "ASSIGNMENT OPERATOR" :
            tokens[i].type == TOKEN_LPAREN ? "LEFT PARENTHESIS" :
            tokens[i].type == TOKEN_RPAREN ? "RIGHT PARENTHESIS" :
            tokens[i].type == TOKEN_COMMA ? "COMMA" :
            tokens[i].type == TOKEN_TAB ? "TAB" :
            tokens[i].type == TOKEN_COMMENT ? "COMMENT" :
            tokens[i].type == TOKEN_EOF ? "END OF FILE" :
            tokens[i].type == TOKEN_UNKNOWN ? "UNKNOWN" :
            "UNKNOWN",
            tokens[i].lexeme, tokens[i].line, tokens[i].position);

        if (tokens[i].type == TOKEN_EOF) {
            break;
        }
    }

    ASTNode* AST = constructAST(tokens);

    printAST(AST, 0);// info/debug

    printSymbolList(symbolList, 10);

    generateCode(AST, "output.c");
   
   
   
   
    //complile the output.c file into output
    int compileResult = system("cc -std=c11 -o output output.c");
    if (compileResult != 0){
        fprintf(stderr, "! Output failed to compile, failed with error code %d\n", compileResult);
        return 1;
    } 

    //run the output file 
    int runResult = system("./output");
    if (runResult != 0){
        fprintf(stderr, "! Output failed to run, failed with error code %d\n", runResult);
        return 1;
    } 
    
    //delete the output.c file
    int removeResult = remove("output.c");
    if (removeResult != 0){
        fprintf(stderr, "! System not able to delete 'output.c' file , failed with error code %d\n", removeResult);
        return 1;
    } 
    
    //delete the compiled output file
    int removeCompiledResult = remove("output");
    if (removeCompiledResult != 0){
        fprintf(stderr, "! System not able to delete compiled 'output' file , failed with error code %d\n", removeCompiledResult);
        return 1;
    } 
    return EXIT_SUCCESS;
}