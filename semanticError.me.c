void printErrorInvalidExpr( returnNode->child ){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Invalid expression.\n");
}

void printWarningReturnTypeMismatch(AST_NODE* node){
    printf("Error found in line %d\n", node->linenumber);
    printf("Warning: Incompatible return type.\n");
}

void printErrorMissingDecl(AST_NODE* node, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("ID %s undeclared.\n", name);
}

void printErrorRedeclaredVar(AST_NODE* idNode, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("ID %s redeclared.\n", name);
}

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

void printErrorDimMismatch(AST_NODE* node ){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Incompatible array dimensions.\n");
}

void printErrorArraySubNotInt( AST_NODE* node ){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("Array subscript is not an integer\n");
}
