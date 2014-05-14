#include <stdio.h>
#include "header.h"
#include "semanticError.h"
extern int g_anyErrorOccur;
/* 1.a */
void printErrorMissingDecl(AST_NODE* node, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("ID %s undeclared.\n", name);
}

/* 1.b */
void printErrorRedeclaredVar(AST_NODE* node, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("ID %s redeclared.\n", name);
}

/* 2.a */
void printErrorTooFewArgs(AST_NODE* node, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("too few arguments to function %s\n", name);
}

void printErrorTooManyArgs(AST_NODE* node, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("too many arguments to function %s\n", name);
}

/* 2.b */
void printWarningReturnTypeMismatch(AST_NODE* node){
    printf("Error found in line %d\n", node->linenumber);
    printf("Warning: Incompatible return type.\n");
}

/* 3.a */
void printErrorDimMismatch(AST_NODE* node ){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Incompatible array dimensions.\n");
}

/* 3.b */
void printErrorArraySubNotInt( AST_NODE* node ){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Array subscript is not an integer\n");
}

/* 3.c */
void printErrorArrayPassToScal(AST_NODE* node, char* IDName, char* parameterName){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Array %s passed to scalar parameter %s.\n", IDName, parameterName);
}

void printErrorScalPassToArray(AST_NODE* node, char* IDName, char* parameterName){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Scalar %s passed to array parameter %s.\n", IDName, parameterName);
}

/* extra */
void printErrorInvalidExpr(AST_NODE* node){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Invalid expression.\n");
}

