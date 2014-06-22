/* 1.a */
void printErrorMissingDecl(AST_NODE* node, char* name);
/* 1.b */
void printErrorRedeclaredVar(AST_NODE* idNode, char* name);
/* 2.a */
void printErrorTooFewArgs(AST_NODE* node, char* name);
void printErrorTooManyArgs(AST_NODE* node, char* name);
/* 2.b */
void printWarningReturnTypeMismatch(AST_NODE* node);
/* 3.a */
void printErrorDimMismatch(AST_NODE* node );
/* 3.b */
void printErrorArraySubNotInt( AST_NODE* node );
/* 3.c */
void printErrorArrayPassToScal(AST_NODE* node, char* IDName, char* parameterName);
void printErrorScalPassToArray(AST_NODE* node, char* IDName, char* parameterName);
/* extra */
void printErrorInvalidExpr(AST_NODE* node);
void printErrorNonIdScalPassToArray(AST_NODE* node, char* parameterName);
