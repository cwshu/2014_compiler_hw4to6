#ifndef __CODEGEN_H__
#define __CODEGEN_H__
void codeGen(AST_NODE* prog, STT* symbolTable);

/*** Declarations ***/
void genVariableDeclList(FILE* targetFile, STT* symbolTable, AST_NODE* variableDeclListNode, int* pLocalVarSize);
void genVariableDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode, int kind, int* pLocalVarSizeNow);
void genFuncDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode);
    /* inner function */
void genFuncHead(FILE* targetFile, char* funcName);
// void genFuncParaList(FILE* targetFile, char* funcName);
void genPrologue(FILE* targetFile, char* funcName);
void genEpilogue(FILE* targetFile, char* funcName, int localVarSize);

/*** Statement generation ***/
void genStmtList(FILE* targetFile, STT* symbolTable, AST_NODE* StmtListNode, char* funcName);
void genStmt(FILE* targetFile, STT* symbolTable, AST_NODE* StmtNode,char* funcName);

void genBlock(FILE* targetFile, STT *symbolTable, AST_NODE* blockNode);
void genIfStmt(FILE* targetFile, AST_NODE* ifStmtNode, STT* symbolTable){
void genWhileStmt(FILE* targetFile, AST_NODE* whileStmtNode, STT* symbolTable){
// void genForStmt(FILE* targetFile, STT* symbolTable, AST_NODE* ifStmtNode);
// void genFuncCallStmt(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode);
void genReturnStmt(FILE* targetFile, STT* symbolTable, AST_NODE* returnNode);

void genAssignmentStmt(FILE* targetFile, STT* symbolTable, AST_NODE* assignmentNode);
void genExpr(FILE* targetFile, STT* symbolTable, AST_NODE* expressionNode);
void genAssignExpr(FILE targetFile, STT* symbolTable, AST_NODE* exprNode);
    /* wrapper for AssignmentStmt and Expr*/
void genFunctionCall(targetFile, symbolTable, exprNode);
void genProcessFuncReturnValue(FILE* targetFile, STT* SymbolTable, AST_NODE* exprNode);

int getExprNodeReg(FILE* targetFile, AST_NODE* exprNode);

/* spec-dependent constant */
#define INT_RETURN_REG v0
#define FLOAT_RETURN_REG f0

typedef struct RegisterManager RegisterManager;

struct RegisterManager {
    int regFull[256];
    AST_NODE* regUser[256];
    int lastReg;
    /* const value after constructor */
    int numOfReg;
    int firstRegNum;
};

void RMinit(RegisterManager* pThis, int numOfReg, int firstRegNum); 
/* constructor */

int getReg(RegisterManager* pThis, FILE* targetFile);
/* get empty register to use, return register Number (16 ~ 23 for s0 ~ s7(r16 ~ r23) ) */
void useReg(RegisterManager* pThis, int regNum, AST_NODE* nodeUseThisReg);
void releaseReg(RegisterManager* pThis, int regNum);
/* release register */
void spillReg(RegisterManager* pThis, int regIndex, FILE* targetFile);
int findEmptyReg(RegisterManager* pThis);
int findEarlestUsedReg(RegisterManager* pThis);

/*** MIPS instruction generation ***/
void genIntUnaryOpInstr(FILE* targetFile, UNARY_OPERATOR op, int destRegNum, int srcRegNum);
void genFloatUnaryOpInstr(FILE* targetFile, UNARY_OPERATOR op, int destRegNum, int srcRegNum);
void genIntBinaryOpInstr(FILE* targetFile, BINARY_OPERATOR op, 
  int destRegNum, int src1RegNum, int src2RegNum);
void genFloatBinaryOpInstr(FILE* targetFile, BINARY_OPERATOR op, 
  int destRegNum, int src1RegNum, int src2RegNum);
/* int instruction */
void genAddOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genSubOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genMulOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genDivOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);

void genEQExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genNEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genLTExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genGTExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genLEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genGEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);

void genANDExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genORExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2);
void genNOTExpr(FILE* targetFile, int distReg, int srcReg);

void genPosOpInstr(FILE* targetFile, int destRegNum, int srcRegNum);
void genNegOpInstr(FILE* targetFile, int destRegNum, int srcRegNum);
/* float instruction */
void genFPAddOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPSubOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPMulOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPDivOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);

void genFPEQInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);
void genFPNEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);
void genFPLTInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);
void genFPGTInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);
void genFPGEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);
void genFPLEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum);

void genFPPosOpInstr(FILE* targetFile, int distRegNum, int srcRegNum);
void genFPNegOpInstr(FILE* targetFile, int distRegNum, int srcRegNum);
/* casting */
void genFloatToInt(FILE* targetFile, int destRegNum, int floatRegNum);
void genIntToFloat(FILE* targetFile, int destRegNum, int intRegNum);
#endif
