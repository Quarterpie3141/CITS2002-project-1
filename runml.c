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

typedef enum {
        FACTOR_CONSTANT,
        FACTOR_IDENTIFIER,
        FACTOR_FUNCTION_CALL,
        FACTOR_EXPRESSION
} FactorType;

typedef enum {
        PROGRAM_STATEMENT,
        PROGRAM_FUNCTION
} ProgramItemType;

typedef enum {
        STATEMENT_ASSIGNMENT,
        STATEMENT_PRINT,
        STATEMENT_RETURN,
        STATEMENT_FUNCTION_CALL
} StatementType;

typedef struct ASTNode {
    ASTNodeType type;
    struct ASTNode* next;   // adress of the next node in an expression list

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
            char* parameterIdentifiers; // (optional)
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
            struct ASTNode* expresssion; // right hand side of expression (optional)
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
                int constantValue;             // for when the factor is a real constant
                char* identifierName;          // For when the factor is a variable
                struct {
                    char* function_name;        // for when the factor is a function call
                    struct ASTNode* args;       // list of expressions for the function
                } functionCall;                
                struct ASTNode* expression;     // for when the function is another expression
            };
        } factor;

        //add(2,4), this node has an identifier and a list of expressions
        struct {
            char* identifierName;
            struct ASTNode* expressions;
        } functionCall;



    };
} ASTNode;

// end of AST types

ASTNode* parseProgramItems(Token* tokenList, int currentToken){

    ASTNode* programItem = malloc(sizeof(ASTNode)); // alloc space for a root node.
    // what the fuck is going on here????
    if (ast != NULL) {
        ast->type = NODE_PROGRAM;
        ast->next = NULL;

        parseProgramItems(tokenList, &currentToken);

    
    }

    if(tokenList[currentToken].type == TOKEN_FUNCTION){
        
    }


}

ASTNode* constructAST(Token* tokenList) {
    int currentToken = 0;
    int capacity = INITAL_ROOT_NODE_CAPACITY;
    int amountOfProgramItems = 0;

    // Allocate space for the root node
    ASTNode* ast = malloc(sizeof(ASTNode));

    if (ast == NULL) {
        perror("failed to allocate memory for root node"); // cahnge to stderror
        exit(EXIT_FAILURE);
    }

    (*ast).type = NODE_PROGRAM; //have to dereference these to change the value, idk how to do it otherwise
    (*ast).next = NULL;

    
    (*ast).program.programItems = malloc(capacity * sizeof(ASTNode*)); // Initial allocation for program items
    if ((*ast).program.programItems == NULL) {
        error("failed to allocate memory for program items");  // cahnge to stderror
        exit(EXIT_FAILURE);
    }

    while (tokenList[currentToken].type != TOKEN_EOF) {
        // eeallocate memory for more program items if needed
        if (amountOfProgramItems >= capacity) {
            capacity *= 2;
            (*ast).program.programItems = realloc((*ast).program.programItems, capacity * sizeof(ASTNode*));
            if ((*ast).program.programItems == NULL) {
                error("failed to reallocate memory for program items");
                exit(EXIT_FAILURE);
            }
        }

        //parse the next program item and add it to the array of nodes.
        (*ast).program.programItems[amountOfProgramItems++] = parseProgramItems(tokenList, &currentToken);
    }
    return ast;
}




int validateFileExt(const char *fileName){
    char *ext  = strrchr(fileName, '.'); // grab the mem address of where the extention begins in the file name

    if(!ext || strcmp(ext, ".ml") != 0){    // check if the file dosent have an extention or has the incorrect extention 
        fprintf(stderr, "Provided file is not a .ml file\n");
        return 1;
    }
    return 0;
}

char *duplicateString(const char *string) {
    int legnth = strlen(string) + 1; // get the length of the string including the null byte
    char *copy = malloc(legnth);   // malloc enough  mem for the copy
    if (copy) {
        memcpy(copy, string, legnth);   //copy the sting into mem
    }
    return copy;    //
}

Token createToken(TokenType type, const char *lexeme, int line, int position) {
    Token token;    // instantiates a new token struct and fills out the values with the ones provided
    token.type = type;
    token.lexeme = duplicateString(lexeme);
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
        (*currentLine++);
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

Token* lexer(FILE* file){   //this entire function is pretty much adapted from llvm Kaleidoscope (1.2)
    static int currentPosition = 0;
    static int currentLine = 1;
    Token* tokens = malloc(sizeof(Token) * INITIAL_ARRAY_SIZE); //create an array of tokens in memory, todo: dynamic mem allocation
    static int amountOfTokens = 0;

    int currentCharacter = fgetc(file);
    currentPosition++;

    while (currentCharacter != EOF){
        //check for identifiers to do add new line stuff from other function s
        if (currentCharacter == ' ' || currentCharacter == '\n' || currentCharacter == '\r') { // skip tokeniasation of spaces, new lines, and carrigae returns
            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
        // can optimise this, todo
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
            int startPosition = currentPosition;  // Save the position of the first #

            // Continue reading until the end of the line or EOF
            while (currentCharacter != '\n' && currentCharacter != EOF ) {
                advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            }

            Token commentToken = createToken(TOKEN_COMMENT, "comment", currentLine, startPosition);
            addToken(tokens, &amountOfTokens, commentToken);

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

int main(int argc, char *argv[]){
    if(argc < 2){  //check that the user provided cli arguments
        fprintf(stderr, "At least one command line argument is required\n");
        return EXIT_FAILURE;
    }

    if(validateFileExt(argv[1])){  // validate the file extention
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");   // open the file and make sure it exists
    if (file == NULL){
        fprintf(stderr, "File does not exist");
        return EXIT_FAILURE;
    }

    Token* tokens = lexer(file);

    fclose(file);

    for (int i = 0; ; i++) {
        printf("Token: %-12s Lexeme: %s (Line %d) (Position %d)\n",
               tokens[i].type == TOKEN_IDENTIFIER ? "IDENTIFIER" :
               tokens[i].type == TOKEN_COMMENT ? "COMMENT" :
               tokens[i].type == TOKEN_REAL ? "REAL NUMBER" :
               tokens[i].type == TOKEN_PLUS ? "PLUS" :
               tokens[i].type == TOKEN_PRINT ? "PRINT" :
               tokens[i].type == TOKEN_ASSIGN ? "ASSIGNMET OP" :
               tokens[i].type == TOKEN_TAB ? "TAB" :
               tokens[i].type == TOKEN_EOF ? "END OF FILE" :
               "UNKNOWN",
               tokens[i].lexeme, tokens[i].line, tokens[i].position);
        if (tokens[i].type == TOKEN_EOF){break;}
    }

    return EXIT_SUCCESS;
}

