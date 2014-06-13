#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include "header.h"

/*** Declarations ***/
void genVariableDeclList(FILE* targetFile, STT* symbolTable, AST_NODE* variableDeclListNode);
void genVariableDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode, int kind);
void genFuncDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode);
    /* inner function */
void genFuncHead(FILE* targetFile, char* funcName);
// void genFuncParaList(FILE* targetFile, char* funcName);
void genPrologue(FILE* targetFile, char* funcName);
void genEpilogue(FILE* targetFile, char* funcName, int localVarSize);

/*** Statement generation ***/
void genStmtList(FILE* targetFile, STT* symbolTable, AST_NODE* stmtListNode, char* funcName);
void genStmt(FILE* targetFile, STT* symbolTable, AST_NODE* stmtNode, char* funcName);

void genBlock(FILE* targetFile, STT *symbolTable, AST_NODE* blockNode, char* funcName);
void genIfStmt(FILE* targetFile, STT* symbolTable, AST_NODE* ifStmtNode, char* funcName){
void genWhileStmt(FILE* targetFile, STT* symbolTable, AST_NODE* whileStmtNode, char* funcName){
// void genForStmt(FILE* targetFile, STT* symbolTable, AST_NODE* ifStmtNode, char* funcName);
void genFuncCallStmt(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode, char* funcName);
void genReturnStmt(FILE* targetFile, STT* symbolTable, AST_NODE* returnNode, char* funcName);

void genAssignmentStmt(FILE* targetFile, STT* symbolTable, AST_NODE* assignmentNode);
void genExpr(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode);
void genAssignExpr(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode);
    /* wrapper for AssignmentStmt and Expr*/
void genFuncCall(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode);
void genProcessFuncReturnValue(FILE* targetFile, STT* SymbolTable, AST_NODE* exprNode);
void genProcessIntReturnValue(FILE* targetFile, AST_NODE* exprNode);
void genProcessFloatReturnValue(FILE* targetFile, AST_NODE* exprNode);

int getExprNodeReg(FILE* targetFile, AST_NODE* exprNode);

/* spec-dependent constant */
#define INT_RETURN_REG "v0"
#define FLOAT_RETURN_REG "f0"

/*** Data Resourse, RegisterManager Implementation ***/
// typedef struct RegisterManager RegisterManager;

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

/*** Constant String Implementation ***/
#define MAX_CON_STRING 2048

typedef struct ConstStringSet ConstStringSet;
typedef struct ConstStringPair ConstStringPair;

struct ConstStringPair{
    int labelNum;
    char* string;
};

struct ConstStringSet{
    int numOfConstString;
    ConstStringPair constStrings[MAX_CON_STRING];
};

void initConstStringSet(ConstStringSet* pThis);
void addConstString(ConstStringSet* pThis, int labelNum, char* string);
void genConstStrings(ConstStringSet* pThis, FILE* targetFile);

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

void genFPEQInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPNEInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPLTInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPGTInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPGEInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);
void genFPLEInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum);

void genFPPosOpInstr(FILE* targetFile, int destRegNum, int srcRegNum);
void genFPNegOpInstr(FILE* targetFile, int destRegNum, int srcRegNum);
/* casting */
void genFloatToInt(FILE* targetFile, int destRegNum, int floatRegNum);
void genIntToFloat(FILE* targetFile, int destRegNum, int intRegNum);
/* IO system call */
void genRead(FILE *targetFile);
void genFRead(FILE *targetFile);
void genWrite(FILE *targetFile, STT* symbolTable, AST_NODE* funcCallNode);
#endif
