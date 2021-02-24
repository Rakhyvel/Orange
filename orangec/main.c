/*  main.c

    1. Simple components, complex combinations
    2. Lower level control

    Todo:
    x indexing operator
    x array literals
    x else statements
    x return statements
    x array initialize
    x character literal
    x string literal
    x real vs integer literal
    x struct definitions
    x struct extension
    x global variable declarations/definitions
    x static modifier for modules
    x constants
    x private modifier for higher levels
    x module access ":"
    x syntax validation

    - type validation
    - identifier validation
    - access validation
    - static module validation
    - constant modify validation
    - constant define & no declare validation

    Author: Joseph Shimel
    Date: 2/2/21
*/

#include <stdio.h>
#include <stdlib.h>

#include "./main.h"
#include "./lexer.h"
#include "./parser.h"
#include "./validator.h"

#include "../util/debug.h"
#include "../util/list.h"
#include "../util/map.h"

/*
 * Takes in an array of files to compile
 * 
 * 1. Lex: Read in files specified to compiler
 * 2.      Create token list from file, remove the comments
 * 3. Parse: Look through each file, add code to functions to modules to the program
 * 4. Validation: Look through AST's, validate type, struct members, module members, state access, etc.
 * 5. Generate code (compile, release) or evaluate tree (interpret, debug)
 */
int main(int argn, char** argv)
{
    struct program* program = parser_initProgram();

    for(int i = 1; i < argn; i++)
    {
        LOG("Reading file %s", argv[i]);
        char* file = lexer_readFile(argv[i]);

        LOG("\n\nBegin Tokenization.");
        struct list* tokenQueue = lexer_tokenize(file);
        LOG("\nEnd Tokenization\n");

        LOG("\n\nBegin Parsing.");
        parser_removeComments(tokenQueue);
        parser_addModules(program, tokenQueue);
        LOG("\nEnd Parsing.\n");

        LOG("\nBegin Validating.");
        validator_validate(program);
        LOG("\nEnd Validating.\n");
    }
    printf("Done.\n");
    return 0;
}

void error(const char *message, ...) {
    va_list args;

    fprintf(stderr, "error: ");

    va_start (args, message);
    vfprintf (stderr, message, args);
    fprintf (stderr, "\n");
    va_end(args);
  
    exit (1);
}