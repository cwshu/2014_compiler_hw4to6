void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    switch(errorMsgKind){
        /* UNFINISH */
    }
}

void printErrorRedeclaredVar(AST_NODE* idNode, char* name){
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    printf("ID %s redeclared.", name);
}
