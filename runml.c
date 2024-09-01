#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define MAX_IDENTIFIER_LENGTH 12
#define MAX_REAL_LENGTH 16
#define MAX_IDENTIFIERS 50
#define INITIAL_ARRAY_SIZE 1024

int syntaxErrorFlag = 0;

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

int validateFile(const char *fileName){
    char *ext  = strrchr(fileName, '.'); // grab the mem address of where the extention begins in the file name

    if(!ext || strcmp(ext, ".ml") != 0){    // check if the file dosent have an extention or has the incorrect extention 
        fprintf(stderr, "Provided file is not a .ml file\n");
        return 1;
    }
    return 0;
}



Token createToken(TokenType type, const char *lexeme, int line, int position) {
    Token token;    // instantiates a new token struct and fills out the values with the ones provided
    token.type = type;
    token.lexeme = strdup(lexeme);
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
        (*currentLine)++;
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

Token* lexer(FILE* file){
    static int currentPosition = 0;
    static int currentLine = 1;
    Token* tokens = malloc(sizeof(Token) * INITIAL_ARRAY_SIZE); //create an array of tokens in memory 
    static int amountOfTokens = 0;

    int currentCharacter = fgetc(file);
    currentPosition++;



    while (currentCharacter != EOF){

        //check for identifiers to do add new line stuff from other function s
        if (currentCharacter == ' ') {
            advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
            continue;
        }
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
            while (currentCharacter != '\n' && currentCharacter != EOF) {
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
        advanceCharacter(file, &currentCharacter, &currentLine, &currentPosition);
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

    if(validateFile(argv[1])){  // validate the file extention
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

