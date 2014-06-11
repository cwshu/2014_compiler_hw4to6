void codeGen(AST_NODE* prog, STT* symbolTable);

void genVariableDeclList(FILE* targetFile, STT* symbolTable, AST_NODE* variableDeclListNode, int* pLocalVarSize);
void genVariableDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode, int kind, int* pLocalVarSizeNow);
int genStmtList(FILE* targetFile, STT* symbolTable, AST_NODE* StmtListNode, char* funcName);
int genStmt(FILE* targetFile, STT* symbolTable, AST_NODE* StmtNode,char* funcName);
void genFuncDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode);

void genFuncHead(FILE* targetFile, char* funcName);
// void genFuncParaList(FILE* targetFile, char* funcName);
void genPrologue(FILE* targetFile, char* funcName);
void genEpilogue(FILE* targetFile, char* funcName, int localVarSize);


