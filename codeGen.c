#include <stdio.h>
#include "codeGen.h"
#include "semanticAnalysis.h"
#include "header.h"

#define GLOBAL 1
#define LOCAL 2

void codeGen(FILE* targetFile, AST_NODE* prog, STT* symbolTable){
    AST_NODE* child = prog->child;
    while(child){
        if(child->nodeType == VARIABLE_DECL_LIST_NODE)
            // pass
            // processVariableDeclList(symbolTable, child);
        else if(child->nodeType == DECLARATION_NODE)
            genFuncDecl(targetFile, symbolTable, child);
        child = child->rightSibling;
    }
}

/*** variable declaration ***/
void genVariableDeclList(FILE* targetFile, STT* symbolTable, AST_NODE* variableDeclListNode, int* pLocalVarSize){
    /* codegen Declaration List */
    AST_NODE* child = variableDeclListNode->child;

    int kind = LOCAL;
    if(symbolTable->currentLevel == 0)
        kind = GLOBAL;

    if(kind == GLOBAL){
        fprintf(targetFile, ".data\n");
    }

    while(child){
        genVariableDecl(targetFile, symbolTable, child, kind, pLocalVarSize);
        child = child->rightSibling; 
    }

    if(kind == GLOBAL)
        return 0;
}

void genVariableDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode, 
  int kind, int* pLocalVarSizeNow){
    /* kind: VARIABLE_DECL
     * Notice: it can more than one variable declaration, like int a, b, c;
     */
    /* no initial value now */
    DECL_KIND declKind = declarationNode->semantic_value.declSemanticValue.kind;
    if(declKind != VARIABLE_DECL)
        return 0;

    AST_NODE* variableNode = declarationNode->child->rightSibling;

    while(variableNode){
        char* varName = variableNode->semantic_value.identifierSemanticValue->identifierName;
        SymbolTableEntry* entry = lookupSymbol(symbolTable, varName);
        TypeDescriptor* type = entry->type;

        int varSize = 4; // int and float
        for(int i=0; i<type->dimension; i++){
            varSize *= type->sizeOfEachDimension[i]; 
        }

        if(kind == GLOBAL){
            if(type->dimension != 0)
                fprintf(targetFile, "%s: .space %d", entry->name, varSize);
            else if(type->primitiveType == INT_TYPE)
                fprintf(targetFile, "%s: .word", entry->name);
            else if(type->primitiveType == FLOAT_TYPE)
                fprintf(targetFile, "%s: .float", entry->name);
        }
        else if(kind == LOCAL){
            entry->stackOffset = *pLocalVarSizeNow + 4;    
            *pLocalVarSizeNow += entry->stackOffset;
        }

        variableNode = variableNode->rightSibling;
    }
}

/*** statement generation ***/
int genStmtList(FILE* targetFile, STT* symbolTable, AST_NODE* StmtListNode, char* funcName);
int genStmt(FILE* targetFile, STT* symbolTable, AST_NODE* StmtNode,char* funcName);

void genIfStmt(FILE* targetFile, AST_NODE* ifStmtNode, STT* symbolTable){
    
    // condition
    genAssignExpr(ifStmtNode->child);
    
    // jump to else if condition not match
    int elseLabel = GR.labelCounter++;
    int exitLabel = GR.labelCounter++;
    fprintf(targetFile, "beqz $r%d L%d\n", ifStmtNode->child->valPlace.place.regNum, elseLabel);
    
    // then block
    genStmt(ifStmtNode->child->rightSibling);
    
    // jump over else
    fprintf(targetFile, "j L%d\n", exitLabel);
    fprintf(targetFile, "L%d:\n", elseLabel);

    // else block
    if( ifStmtNode->child->rightSibling->rightSibling->AST_TYPE != NUL_NODE )
        genStmt(ifStmtNode->child->rightSibling->rightSibling);

    // exit
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genWhileStmt(FILE* targetFile, AST_NODE* whileStmtNode, STT* symbolTable){
    
    // Test Label
    int testLabel = GR.labelCounter++;
    int exitLabel = GR.labelCounter++;
    fprintf(targetFile, "L%d:\n", testLabel);
    
    // condition
    genAssignExpr(whileStmtNode->child);
    
    // check condition
    fprintf(targetFile, "beqz $r%d L%d\n", whileStmtNode->child->valPlace.place.regNum, exitLabel);
    
    // Stmt
    genStmt(whileStmtNode->child->rightSibling);

    // loop back
    fprintf(targetFile, "j L%d\n", testLabel);

    // exit
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genAssignmentStmt(FILE* targetFile, STT* symbolTable, AST_NODE* assignmentNode){
    /* code generation for assignment node */
    /* lvalue */
    char *lvalueName = assignmentNode->child->semantic_value.identifierSemanticValue.identifierName;
    int lvalueScope = LOCAL;
    SymbolTableEntry *lvalueEntry = lookupSymbolCurrentScope(symbolTable, lvalueName);
    if(!entry){
        lvalueScope = GLOBAL;
        lvalueEntry = lookupSymbol(symbolTable, lvalueName);
    }
    DATA_TYPE lvalueType = entry->type->primitiveType;
    /* rvalue */
    AST_NODE* rvalueNode = assignmentNode->child->rightSibling;
    genExpr(targetFile, symbolTable, rvalueNode); // 0 means not function parameter
    DATA_TYPE rvalueType = getTypeOfExpr(symbolTable, rvalueNode);
    int rvalueRegNum = getExprNodeReg(rvalueNode);
    /* type conversion */
    if(lvalueType == INT_TYPE && rvalueType == FLOAT_TYPE){
        int tmp = genFloatToInt(targetFile, rvalueRegNum);
        releaseFPReg(rvalueRegNum);
        rvalueRegNum = tmp;
    }
    else if(lvalueType == FLOAT_TYPE && rvalueType == INT_TYPE){
        int tmp = genIntToFloat(targetFile, rvalueRegNum);
        releaseReg(rvalueRegNum);
        rvalueRegNum = tmp;
    }
    /* assignment */
    if(lvalueType == INT_TYPE){
        if(lvalueScope == LOCAL)
            fprintf(targetFile, "sw $r%d, %d($fp)\n", rvalueRegNum, lvalueEntry->stackOffset);
        else if(lvalueScope == GLOBAL)
            fprintf(targetFile, "sw $r%d, %s\n", rvalueRegNum, lvalueEntry->name);
        releaseReg(rvalueRegNum);
    }
    if(lvalueType == FLOAT_TYPE){
        if(lvalueScope == LOCAL)
            fprintf(targetFile, "s.s $r%d, %d($fp)\n", rvalueRegNum, lvalueEntry->stackOffset);
        else if(lvalueScope == GLOBAL)
            fprintf(targetFile, "s.s $f%d, %s\n", rvalueRegNum, lvalueEntry->name);
        releaseFPReg(rvalueRegNum);
    }
}

void genExpr(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode){
    /* code generation for expression */
    if( exprNode->nodeType == CONST_VALUE_NODE ){
        if( exprNode->semantic_value.const1->const_type == INTEGERC){
            int value = exprNode->semantic_value.const1->const_u.intval;
            int intRegNum = getReg(&GR.regManager, targetFile);
            fprintf(targetFile, "li $r%d, %d", intRegNum, value);

            exprNode->valPlace.kind = INT_REG_TYPE;
            exprNode->valPlace.place.regNum = intRegNum;
        }
        else if ( exprNode->semantic_value.const1->const_type == FLOATC ){
            float value = exprNode->semantic_value.const1->const_u.fval;
            int floatRegNum = getFPReg(&GR.regManager, targetFile);
            fprintf(targetFile, "li.s $f%d, %d", floatRegNum, value);

            exprNode->valPlace.kind = FLOAT_REG_TYPE;
            exprNode->valPlace.place.regNum = floatRegNum;
        }    
    }
    else if( exprNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT ){
        AST_NODE* funcNameNode = exprNode->child;
        char* funcName = funcNameNode->semantic_value.identifierSemanticValue->identifierName;
        SymbolTableEntry* funcEntry = lookupSymbol(symbolTable, funcName);
        DATA_TYPE returnType = funcEntry->type->primitiveType;

        if(returnType == INT_TYPE){
            int intRegNum = getReg(&GR.regManager, targetFile);
            fprintf(targetFile, "add $r%d, $%s, $r0\n", intRegNum, INT_RETURN_REG);

            exprNode->valPlace.kind = INT_REG_TYPE;
            exprNode->valPlace.place.regNum = intRegNum;
        }
        else if(returnType == FLOAT_TYPE){
            int floatRegNum = getFPReg(&GR.regManager, targetFile);
            fprintf(targetFile, "mov.s $f%d, $%s\n", intRegNum, INT_RETURN_REG);

            exprNode->valPlace.kind = FLOAT_REG_TYPE;
            exprNode->valPlace.place.regNum = floatRegNum;
        }
    }
    else if( exprNode->nodeType == IDENTIFIER_NODE ){
        char* name = exprNode->semantic_value.identifierSemanticValue->identifierName;
        int scope = LOCAL;
        SymbolTableEntry* entry = lookupSymbolCurrentScope(symbolTable, name);
        if(!entry){
            scope = GLOBAL;
            entry = lookupSymbol(symbolTable, name);
        }
        
        if(scope == LOCAL){
            exprNode->valPlace.kind = STACK_TYPE;
            exprNode->valPlace.place.stackOffset = entry->stackOffset;
        }
        if(scope == GLOBAL){
            exprNode->valPlace.kind = LABEL_TYPE;
            exprNode->valPlace.place.label = entry->name;
        }
    }
    else if( exprNode->nodeType == EXPR_NODE ){
        if( exprNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION ){
            genExpr(exprNode->child);
            DATA_TYPE type = checkExpr(exprNode->child);
            int childRegNum = getExprNodeReg(targetFile, exprNode);

            UNARY_OPERATION op = exprNode->semantic_value.exprSemanticValue.op.unaryOp;
            if(type == INT_TYPE){
                int regNum = getReg(GR.regManager, targetFile);
                switch(op){
                    UNARY_OP_POSITIVE: genPosOpInstr(targetFile, regNum, childRegNum); break;
                    UNARY_OP_NEGATIVE: genNegOpInstr(targetFile, regNum, childRegNum); break;
                    UNARY_OP_LOGICAL_NEGATION: genNOTExpr(targetFile, regNum, childRegNum); break;
                }
                exprNode->valPlace.kind = INT_REG_TYPE;
                exprNode->valPlace.place.regNum = regNum; 
                releaseReg(childRegNum);
            }
            else if(type == FLOAT_TYPE){
                int regNum = getFPReg(GR.regManager, targetFile);
                switch(op){
                    UNARY_OP_POSITIVE: break;
                    UNARY_OP_NEGATIVE: break;
                    UNARY_OP_LOGICAL_NEGATION: break;
                }
                exprNode->valPlace.kind = FLOAT_REG_TYPE;
                exprNode->valPlace.place.regNum = regNum; 
                releaseFPReg(childRegNum);
            }
        }
        else if( exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION ){
            genExpr(exprNode->child);
            genExpr(exprNode->child->rightSibling);
            DATA_TYPE type = checkExpr(exprNode->child);
            int child1RegNum = getExprNodeReg(targetFile, exprNode);
            int child2RegNum = getExprNodeReg(targetFile, exprNode);

            BINARY_OPERATION op = exprNode->semantic_value.exprSemanticValue.op.binaryOp;
            if(type == INT_TYPE){
                int regNum = getReg(GR.regManager, targetFile);
                switch(op){
                    BINARY_OP_ADD: genAddOpInstr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_SUB: genSubOpInstr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_MUL: genMulOpInstr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_DIV: genDivOpInstr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_EQ: genEQExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_GE: genGEExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_LE: genLEExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_NE: genNEExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_GT: genGTExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_LT: genLTExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_AND: genANDExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                    BINARY_OP_OR: genORExpr(targetFile, regNum, child1RegNum, child2RegNum); break;
                }
                exprNode->valPlace.kind = INT_REG_TYPE;
                exprNode->valPlace.place.regNum = regNum; 
                releaseReg(child1RegNum);
                releaseReg(child2RegNum);
            }
            else if(type == FLOAT_TYPE){
                int regNum = getReg(GR.regManager, targetFile);
                switch(op){
                    BINARY_OP_ADD: break;
                    BINARY_OP_SUB: break;
                    BINARY_OP_MUL: break;
                    BINARY_OP_DIV: break;
                    BINARY_OP_EQ: break;
                    BINARY_OP_GE: break;
                    BINARY_OP_LE: break;
                    BINARY_OP_NE: break;
                    BINARY_OP_GT: break;
                    BINARY_OP_LT: break;
                    BINARY_OP_AND: break;
                    BINARY_OP_OR: break;
                }
                exprNode->valPlace.kind = INT_REG_TYPE;
                exprNode->valPlace.place.regNum = regNum; 
                releaseReg(child1RegNum);
                releaseReg(child2RegNum);
            }
        }
    }
}

int getExprNodeReg(FILE* targetFile, AST_NODE* exprNode);
/* return register or FP register of expression value.
 * For value in memory, load it to register */
int genFloatToInt(FILE* targetFile, int floatRegNum);
int genIntToFloat(FILE* targetFile, int intRegNum);


/*** function implementation ***/
void genFuncDecl(STT* symbolTable, AST_NODE* funcDeclarationNode){
    /* codegen for function definition */

    AST_NODE* funcNameNode = returnTypeNode->rightSibling;
    char* funcName = funcNameNode->semantic_value.identifierSemanticValue.identifierName;
    genFuncHead(targetFile, funcName);

    /*
     * parameter list
     */

    /* into block: openScope & prologue
     *             processing Decl_list + Stmt_list
     *             epilogue & closeScope
     */
    AST_NODE* blockNode = paraListNode->rightSibling;
    openScope(symbolTable, USE);
    genPrologue(targetFile, funcName);

    AST_NODE* blockChild = blockNode->child;
    int localVarSize = 0;
    while(blockChild){
        if(blockChild->nodeType == VARIABLE_DECL_LIST_NODE)
            genVariableDeclList(targetFile, symbolTable, blockChild, &localVarSize);
        if(blockChild->nodeType == STMT_LIST_NODE){
            GR.stackTop += localVarSize;
            genStmtList(targetFile, symbolTable, blockChild, funcName);
        }
        blockChild = blockChild->rightSibling;
    }

    genEpilogue(targetFile, funcName, GR.stackTop);
    GR.stackTop = 36;
    closeScope(symbolTable);
}

void genFuncHead(FILE* targetFile, char* funcName){
    fprintf(targetFile, ".text\n"                     );
    fprintf(targetFile, "%s:\n"                       , funcName);
}

void genPrologue(FILE* targetFile, char* funcName){
    fprintf(targetFile, "    sw $ra, 0($sp)\n"        );
    fprintf(targetFile, "    sw $fp, -4($sp)\n"       );
    fprintf(targetFile, "    add $fp, $sp, -4\n"      );
    fprintf(targetFile, "    add $sp, $fp, -4\n"      );
    fprintf(targetFile, "    lw  $2, _framesize_%s\n" , funcName);
    fprintf(targetFile, "    sub $sp, $sp, $2\n"      );
    fprintf(targetFile, "    # Saved register\n"      );
    fprintf(targetFile, "    sw  $s0, 36($sp)\n"      );
    fprintf(targetFile, "    sw  $s1, 32($sp)\n"      );
    fprintf(targetFile, "    sw  $s2, 28($sp)\n"      );
    fprintf(targetFile, "    sw  $s3, 24($sp)\n"      );
    fprintf(targetFile, "    sw  $s4, 20($sp)\n"      );
    fprintf(targetFile, "    sw  $s5, 16($sp)\n"      );
    fprintf(targetFile, "    sw  $s6, 12($sp)\n"      );
    fprintf(targetFile, "    sw  $s7, 8($sp)\n"       );
    fprintf(targetFile, "    sw  $gp, 4($sp)\n"       ); 
    fprintf(targetFile, "_begin_%s:\n"                , funcName);
}                                               

void genEpilogue(FILE* targetFile, char* funcName, int localVarSize){
    int frameSize = 36 + localVarSize;
    fprintf("# epilogue\n"               );
    fprintf("_end_%s:\n"                 , funcName);
    fprintf("    # Load Saved register\n");
    fprintf("    lw  $s0, 36($sp)\n"     );
    fprintf("    lw  $s1, 32($sp)\n"     );
    fprintf("    lw  $s2, 28($sp)\n"     );
    fprintf("    lw  $s3, 24($sp)\n"     );
    fprintf("    lw  $s4, 20($sp)\n"     );
    fprintf("    lw  $s5, 16($sp)\n"     );
    fprintf("    lw  $s6, 12($sp)\n"     );
    fprintf("    lw  $s7, 8($sp)\n"      );
    fprintf("    lw  $gp, 4($sp)\n"      );
    fprintf("\n"                         );
    fprintf("    lw  $ra, 4($fp)\n"      );
    fprintf("    add $sp, $fp, 4\n"      );
    fprintf("    lw  $fp, 0($fp)\n"      );
    fprintf("    jr  $ra\n"              );
    fprintf(".data\n"                    );
    fprintf("    _framesize_%s: .word %d\n", funcName, frameSize);
}

/*
 # prologue
.text
{funcName}:
    sw $ra, 0($sp)
    sw $fp, -4($sp)
    add $fp, $sp, -4
    add $sp, $fp, -4
    lw  $2, _framesize_{funcName}
    sub $sp, $sp, $2
    # Saved register
    sw  $s0, 36($sp)
    sw  $s1, 32($sp)
    sw  $s2, 28($sp)
    sw  $s3, 24($sp)
    sw  $s4, 20($sp)
    sw  $s5, 16($sp)
    sw  $s6, 12($sp)
    sw  $s7, 8($sp)
    sw  $gp, 4($sp)
_begin_{funcName}:
 
    ... # function body 

# epilogue
_end_{funcName}:
    # Load Saved register
    lw  $s0, 36($sp)
    lw  $s1, 32($sp)
    lw  $s2, 28($sp)
    lw  $s3, 24($sp)
    lw  $s4, 20($sp)
    lw  $s5, 16($sp)
    lw  $s6, 12($sp)
    lw  $s7, 8($sp)
    lw  $gp, 4($sp)

    lw  $ra, 4($fp)
    add $sp, $fp, 4
    lw  $fp, 0($fp)
    jr  $ra
.data
    _framesize_{funcName}: .word 36 + {localVarSize}
 */

/* Data Resourse, RegisterManager */
void RMinit(RegisterManager* pThis){
    for(int i=0; i<MAX_REG_NUM; i++){
        pThis->regFull[i] = 0;
        pThis->regUser[i] = NULL;
    }
    pThis->lastReg = 0;
    for(int i=0; i<MAX_FP_REG_NUM; i++){
        pThis->FPregFull[i] = 0;
        pThis->FPregUser[i] = NULL;
    }
    pThis->lastFPReg = 0;
}

int getReg(RegisterManager* pThis, FILE* targetFile){
    /* get empty register to use, return register Number (16 ~ 23 for s0 ~ s7, r16 ~ r23 ) */
    int regIndex = findEmptyReg(pThis);
    if(regIndex != -1){
        pThis->regFull[index] = 1;
        return regIndex;
    }
    
    regIndex = findEarlestUsedReg(pThis);
    spillReg(pThis, regIndex, targetFile);
    return regIndex + 16; // s0 = r16 in mips
}

void releaseReg(RegisterManager* pThis, int regNum){
    pThis->regFull[regNum - 16] = 0;
}

int findEmptyReg(RegisterManager* pThis){
    int index = pThis->lastReg+1;
    while(index != pThis->lastReg){
        if(index == NULL)
            return index;
        index = (index+1)%MAX_REG_NUM;
    }
    return -1;
}

int findEarlestUsedReg(RegisterManager* pThis){
    return pThis->lastReg + 1;
}

void spillReg(RegisterManager* pThis, int regIndex, FILE* targetFile){
    ExpValPlace* place = pThis->regUser[regIndex]->valPlace

    fprintf(targetFile, "sw $r%d, %d($fp)\n", regIndex+16, GR.stackTop + 4);
    place->kind = STACK_TYPE;
    place->place.stackOffset = GR.stackTop + 4;
    GR.stackTop += 4;
}

/* floating point */
int getFPReg(RegisterManager* pThis, FILE* targetFile){
    /* get empty floating-point register to use, return register Number (0 ~ 31 for f0 ~ f31) */
    int regIndex = findEmptyReg(pThis);
    if(regIndex != -1){
        pThis->FPregFull[index] = 1;
        return regIndex;
    }
    
    regIndex = findEarlestUsedFPReg(pThis);
    spillFPReg(pThis, regIndex, targetFile);
    return regIndex;
}

void releaseFPReg(RegisterManager* pThis, int regNum){
    pThis->regFPFull[regNum] = 0;
}

int findEmptyFPReg(RegisterManager* pThis){
    int index = pThis->lastFPReg+1;
    while(index != pThis->lastFPReg){
        if(index == NULL)
            return index;
        index = (index+1)%MAX_FP_REG_NUM;
    }
    return -1;
}

int findEarlestUsedFPReg(RegisterManager* pThis){
    return pThis->lastFPReg + 1;
}

void spillFPReg(RegisterManager* pThis, int regIndex, FILE* targetFile){
    ExpValPlace* place = pThis->regUser[regIndex]->valPlace

    fprintf(targetFile, "l.s $f%d, %d($fp)\n", regIndex+16, GR.stackTop + 4);
    place->kind = STACK_TYPE;
    place->place.stackOffset = GR.stackTop + 4;
    GR.stackTop += 4;
}

/*** MIPS instruction generation ***/
void genAddOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "add $r%d, $r%d, $r%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genSubOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "sub $r%d, $r%d, $r%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genMulOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "mult $r%d, $r%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "mflo $r%d\n", destRegNum);
}

void genDivOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "div $r%d, $r%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "mflo $r%d\n", destRegNum);
}

void genEQExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "seq $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genNEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "sne $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genLTExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "slt $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genGTExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "sgt $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genLEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "sle $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genGEExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "sge $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genANDExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "and $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genORExpr(FILE* targetFile, int distReg, int srcReg1, int srcReg2){
    fprintf(targetFile, "or $r%d, $r%d, $r%d\n", distReg, srcReg1, srcReg2);
}

void genNOTExpr(FILE* targetFile, int distReg, int srcReg){
    fprintf(targetFile, "seq $r%d, $r%d, $r%d\n", distReg, srcReg, 0);
}

void genPosOpInstr(FILE* targetFile, int destRegNum, int srcRegNum){
    fprintf(targetFile, "add $r%d, $r%d, $r0\n", destRegNum, srcRegNum);
}

void genNegOpInstr(FILE* targetFile, int destRegNum, int srcRegNum){
    fprintf(targetFile, "sub $r%d, $r0, $r%d\n", destRegNum, srcRegNum);
}


