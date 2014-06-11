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

/* Data Resourse */
#define MAX_REG_NUM 8
#define MAX_FP_REG_NUM 32
typedef struct RegisterManager RegisterManager;

struct RegisterManager {
    int regFull[MAX_REG_NUM];
    AST_NODE* regUser[MAX_REG_NUM];
    int lastReg;

    int FPregFull[MAX_FP_REG_NUM];
    AST_NODE* FPregUser[MAX_REG_NUM];
    int lastFPReg;
};

void RMinit(RegisterManager* pThis);

int getReg(RegisterManager* pThis, FILE* targetFile);
/* get empty register to use, return register Number (16 ~ 23 for s0 ~ s7(r16 ~ r23) ) */
void releaseReg(RegisterManager* pThis, int regNum);
/* release register */
void spillReg(RegisterManager* pThis, int regIndex, FILE* targetFile);
int findEmptyReg(RegisterManager* pThis);
int findEarlestUsedReg(RegisterManager* pThis);

int getFPReg(RegisterManager* pThis, FILE* targetFile);
/* get empty floating-point register to use, return register Number (0 ~ 31 for f0 ~ f31) */
void releaseFPReg(FPRegisterManager* pThis, int regNum);
/* release floating-point register */
void spillFPReg(FPRegisterManager* pThis, int regIndex, FILE* targetFile);
int findEmptyFPReg(FPRegisterManager* pThis);
int findEarlestUsedFPReg(FPRegisterManager* pThis);
