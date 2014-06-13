#include <stdio.h>
#include <assert.h>
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
void genVariableDeclList(FILE* targetFile, STT* symbolTable, AST_NODE* variableDeclListNode){
    /* codegen Declaration List */
    AST_NODE* child = variableDeclListNode->child;

    int kind = LOCAL;
    if(symbolTable->currentLevel == 0)
        kind = GLOBAL;

    if(kind == GLOBAL){
        fprintf(targetFile, ".data\n");
    }

    while(child){
        genVariableDecl(targetFile, symbolTable, child, kind);
        child = child->rightSibling; 
    }

    if(kind == GLOBAL)
        return 0;
}

void genVariableDecl(FILE* targetFile, STT* symbolTable, AST_NODE* declarationNode, 
  int kind){
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
            entry->stackOffset = GR.stackTop + 4;
            GR.stackTop += 4;
        }

        variableNode = variableNode->rightSibling;
    }
}

/*** function implementation ***/
void genFuncDecl(STT* symbolTable, AST_NODE* funcDeclarationNode){
    /* codegen for function definition */
    AST_NODE* returnTypeNode = funcDeclarationNode->child;
    AST_NODE* funcNameNode = returnTypeNode->rightSibling;
    AST_NODE* paraListNode = funcNameNode->rightSibling;
    AST_NODE* blockNode = paraListNode->rightSibling;

    char* funcName = funcNameNode->semantic_value.identifierSemanticValue.identifierName;
    genFuncHead(targetFile, funcName);

    /*
     * parameter list
     */

    /* into block: openScope & prologue
     *             processing Decl_list + Stmt_list
     *             epilogue & closeScope
     */

    openScope(symbolTable, USE);
    genPrologue(targetFile, funcName);

    AST_NODE* blockChild = blockNode->child;

    while(blockChild){
        if(blockChild->nodeType == VARIABLE_DECL_LIST_NODE)
            genVariableDeclList(targetFile, symbolTable, blockChild);
        if(blockChild->nodeType == STMT_LIST_NODE)
            genStmtList(targetFile, symbolTable, blockChild, funcName);
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

/*** statement generation ***/
void genStmtList(FILE* targetFile, STT* symbolTable, AST_NODE* StmtListNode){
    
    AST_NODE* child = StmtListNode->child;

    while( child ){
        genStmt(targetFile, symbolTable, child);
        child = child -> rightSibling;
    }
}

void genStmt(FILE* targetFile, STT* symbolTable, AST_NODE* StmtNode){
    
    if( StmtNode->nodeType == BLOCK_NODE )
        genBlock(targetFile, symbolTable, StmtNode);
    else if( StmtNode->nodeType == STMT_NODE ){
        
        STMT_KIND stmtKind = StmtNode->semantic_value.stmtSemanticValue.kind;
        switch( stmtKind ){
            case WHILE_STMT: genWhileStmt(targetFile, symbolTable, StmtNode); break;
            case FOR_STMT: assert(0); break;
            case IF_STMT: genIfStmt(targetFile, symbolTable, StmtNode) break;
            case ASSIGN_STMT: genAssignmentStmt(targetFile, symbolTable, StmtNode); break;
            case FUNCTION_CALL_STMT: genFuncCallStmt(targetFile, symbolTable, StmtNode); break;
            case RETURN_STMT: genReturnStmt(targetFile, symbolTable, StmtNode, funcName); break;
        }
    }
}

void genBlock(FILE* targetFile, STT* symbolTable, AST_NODE* blockNode){
    openScope(symbolTable, USE);

    AST_NODE* blockChild = blockNode->child;
    while(blockChild){
        if(blockChild->nodeType == VARIABLE_DECL_LIST_NODE)
            genVariableDeclList(targetFile, symbolTable, blockChild);
        if(blockChild->nodeType == STMT_LIST_NODE)
            genStmtList(targetFile, symbolTable, blockChild, funcName);
        blockChild = blockChild->rightSibling;
    }

    GR.stackTop = 36;
    closeScope(symbolTable);
}

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

// return stmt
void genReturnStmt(FILE* targetFile, STT* symbolTable, AST_NODE* returnNode, char* funcName){
    
    // genExpr
    genExpr(targetFile, symbolTable, returnNode->child);

    SymbolTableEntry* funcEntry = lookupSymbol(symbolTable, funcName);
    DATA_TYPE returnType = funcEntry->type->primitiveType;

    if(returnType == INT_TYPE){
        int retRegNum = getExprNodeReg(returnNode->child);
        fprinf(targetFile, "move $r%d, $r%d\n", INT_RETURN_REG, retRegNum);
        releaseReg(&GR.regManager, retRegNum);
    }
    else if(returnType == FLOAT_TYPE){
        int retRegNum = getExprNodeReg(returnNode->child);
        fprinf(targetFile, "mov.s $f%d, $f%d\n", FLOAT_RETURN_REG, retRegNum);
        releaseReg(&GR.FPRegManager, retRegNum);
    }

    fprintf(targetFile, "j _end_%s\n", funcName);
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
        int intRegNum = getReg(&(GR.regManager));
        genFloatToInt(targetFile, intRegNum, rvalueRegNum);
        releaseReg(&(GR.FPRegManager), rvalueRegNum);
        rvalueRegNum = intRegNum;

        setPlaceOfASTNodeToReg(rvalueNode, INT_TYPE, intRegNum);
        useReg(GR.regManager, rvalueNode, intRegNum);
    }
    else if(lvalueType == FLOAT_TYPE && rvalueType == INT_TYPE){
        int floatRegNum = getReg(&GR.FPRegManager);
        genIntToFloat(targetFile, floatRegNum, rvalueRegNum);
        releaseReg(&GR.regManager, rvalueRegNum);
        rvalueRegNum = floatRegNum;

        setPlaceOfASTNodeToReg(rvalueNode, FLOAT_TYPE, floatRegNum);
        useReg(GR.FPRegManager, rvalueNode, floatRegNum);
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

            setPlaceOfASTNodeToReg(exprNode, INT_TYPE, intRegNum);
            useReg(&GR.regManager, intRegNum, exprNode);
        }
        else if ( exprNode->semantic_value.const1->const_type == FLOATC ){
            float value = exprNode->semantic_value.const1->const_u.fval;
            int floatRegNum = getReg(&GR.FPRegManager, targetFile);
            fprintf(targetFile, "li.s $f%d, %d", floatRegNum, value);

            setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, floatRegNum);
            useReg(&GR.FPRegManager, floatRegNum, exprNode);
        }    
    }
    else if( exprNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT ){
        genFuncCall(targetFile, symbolTable, exprNode);
        genProcessFuncReturnValue(targetFile, symbolTable, exprNode);
    }
    else if( exprNode->nodeType == IDENTIFIER_NODE ){
        char* name = exprNode->semantic_value.identifierSemanticValue->identifierName;
        int scope = LOCAL;
        SymbolTableEntry* entry = lookupSymbolCurrentScope(symbolTable, name);
        if(!entry){
            scope = GLOBAL;
            entry = lookupSymbol(symbolTable, name);
        }
        DATA_TYPE type = entry->type->primitiveType;

        if(scope == LOCAL){
            if(type == INT_TYPE)
                setPlaceOfASTNodeToStack(exprNode, INT_TYPE, entry->stackOffset);
            else if(type == FLOAT_TYPE)
                setPlaceOfASTNodeToStack(exprNode, FLOAT_TYPE, entry->stackOffset);
            /* UNFINISH: write("hello world") */
        }
        if(scope == GLOBAL){
            if(type == INT_TYPE)
                setPlaceOfASTNodeToLabel(exprNode, INT_TYPE, entry->name);
            else if(type == FLOAT_TYPE)
                setPlaceOfASTNodeToLabel(exprNode, FLOAT_TYPE, entry->name);
            /* UNFINISH: write("hello world") */
        }
    }
    else if( exprNode->nodeType == EXPR_NODE ){
        if( exprNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION ){
            genExpr(exprNode->child);
            DATA_TYPE type = checkExpr(exprNode->child);
            int childRegNum = getExprNodeReg(targetFile, exprNode);

            UNARY_OPERATOR op = exprNode->semantic_value.exprSemanticValue.op.unaryOp;
            if(type == INT_TYPE){
                int regNum = getReg(GR.regManager, targetFile);

                genIntUnaryOpInstr(targetFile, op, regNum, childRegNum);

                setPlaceOfASTNodeToReg(exprNode, INT_TYPE, regNum);
                useReg(GR.regManager, exprNode, regNum);
                releaseReg(GR.regManager, childRegNum);
            }
            else if(type == FLOAT_TYPE){
                int regNum = getReg(GR.FPRegManager, targetFile);

                genFloatUnaryOpInstr(targetFile, op, regNum, childRegNum);

                setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, regNum);
                useReg(GR.FPRegManager, exprNode, regNum);
                releaseReg(GR.FPRegManager, childRegNum);
            }
        }
        else if( exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION ){
            genExpr(exprNode->child);
            genExpr(exprNode->child->rightSibling);
            DATA_TYPE type = checkExpr(exprNode->child);
            int child1RegNum = getExprNodeReg(targetFile, exprNode);
            int child2RegNum = getExprNodeReg(targetFile, exprNode);

            BINARY_OPERATOR op = exprNode->semantic_value.exprSemanticValue.op.binaryOp;
            if(type == INT_TYPE){
                int regNum = getReg(GR.regManager, targetFile);

                genIntBinaryOpInstr(targetFile, op, regNum, child1RegNum, child2RegNum);

                setPlaceOfASTNodeToReg(exprNode, INT_TYPE, regNum);
                useReg(GR.regManager, exprNode, regNum);
                releaseReg(GR.regManager, child1RegNum);
                releaseReg(GR.regManager, child2RegNum);
            }
            else if(type == FLOAT_TYPE){
                int regNum = getReg(GR.FPRegManager, targetFile);

                genFloatBinaryOpInstr(targetFile, op, regNum, child1RegNum, child2RegNum);

                setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, regNum);
                useReg(GR.FPRegManager, exprNode, regNum);
                releaseReg(GR.FPRegManager, child1RegNum);
                releaseReg(GR.FPRegManager, child2RegNum);
            }
        }
    }
}

void genAssignExpr(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode){
    /* test, assign_expr(grammar): checkAssignmentStmt or checkExpr */
    if(exprNode->nodeType == STMT_NODE){
        if(exprNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
            genAssignmentStmt(targetFile, symbolTable, exprNode);
    }
    else 
        genExpr(targetFile, symbolTable, exprNode);
}

void genFuncCall(FILE* targetFile, STT* symbolTable, AST_NODE* funcCallNode){
    /* codegen for jumping to the function(label)
     * HW6 Extension: with Parameter function call */
     char *funcName = funcCallNode->child->IdentifierSemanticValue.identifierName;
     fprintf(targetFile, "j %s\n",funcName);
}

void genProcessFuncReturnValue(FILE* targetFile, STT* symbolTable, AST_NODE* exprNode){
    /* after function call, process function return value, and store in exprNode->place */
    AST_NODE* funcNameNode = exprNode->child;
    char* funcName = funcNameNode->semantic_value.identifierSemanticValue->identifierName;
    SymbolTableEntry* funcEntry = lookupSymbol(symbolTable, funcName);
    DATA_TYPE returnType = funcEntry->type->primitiveType;

    if(returnType == INT_TYPE){
        int intRegNum = getReg(&GR.regManager, targetFile);
        fprintf(targetFile, "add $r%d, $%s, $r0\n", intRegNum, INT_RETURN_REG); /* equal to move */

        setPlaceOfASTNodeToReg(exprNode, INT_TYPE, intRegNum);
        useReg(&GR.regManager, intRegNum)
    }
    else if(returnType == FLOAT_TYPE){
        int floatRegNum = getReg(&GR.FPRegManager, targetFile);
        fprintf(targetFile, "mov.s $f%d, $%s\n", floatRegNum, FLOAT_RETURN_REG);

        setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, floatRegNum);
        useReg(&GR.FPRegManager, floatRegNum);
    }
}

int getExprNodeReg(FILE* targetFile, AST_NODE* exprNode){
    /* return register or FP register of expression value.
     * For value in memory, load it to register */
    if(exprNode->place.kind == REG_TYPE){
        return exprNode->place.regNum;
    }
    else if(exprNode->valPlace.kind == STACK_TYPE){
        int stackOffset = exprNode->valPlace.place.stackOffset;
        if(exprNode->place.dataType == INT_TYPE){
            int regNum = getReg(&GR.regManager, targetFile);
            fprintf("lw $r%d, %s($fp)\n", regNum, stackOffset);
            useReg(&GR.regManager, exprNode, regNum);
            setPlaceOfASTNodeToReg(exprNode, INT_TYPE, regNum);
            return regNum;
        }
        else if(exprNode->place.dataType == FLOAT_TYPE){
            int regNum = getReg(&GR.FPRegManager, targetFile);
            fprintf("lw $r%d, %s($fp)\n", regNum, stackOffset);
            useReg(&GR.FPRegManager, exprNode, regNum);
            setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, regNum);
            return regNum;
        }
    }
    else if(exprNode->valPlace.kind == LABEL_TYPE){
        char* label = exprNode->valPlace.place.label;
        if(exprNode->place.dataType == INT_TYPE){
            int regNum = getReg(&GR.regManager, targetFile);
            fprintf("lw $r%d, %s\n", regNum, label);
            useReg(&GR.regManager, exprNode, regNum);
            setPlaceOfASTNodeToReg(exprNode, INT_TYPE, regNum);
            return regNum;
        }
        else if(exprNode->place.dataType == FLOAT_TYPE){
            int regNum = getReg(&GR.FPRegManager, targetFile);
            fprintf("lw $r%d, %s\n", regNum, label);
            useReg(&GR.FPRegManager, exprNode, regNum);
            setPlaceOfASTNodeToReg(exprNode, FLOAT_TYPE, regNum);
            return regNum;
        }
    }

    return -1;
}

/*** Data Resourse, RegisterManager Implementation ***/
void RMinit(RegisterManager* pThis, int numOfReg, int firstRegNum){
    pThis->numOfReg = numOfReg;
    pThis->firstRegNum = firstRegNum;

    for(int i=0; i<pThis->numOfReg; i++){
        pThis->regFull[i] = 0;
        pThis->regUser[i] = NULL;
    }
    pThis->lastReg = 0;
}

int getReg(RegisterManager* pThis, FILE* targetFile){
    /* get empty register to use, return register Number (16 ~ 23 for s0 ~ s7, r16 ~ r23 ) */

    /* find empty register first */
    int regIndex = findEmptyReg(pThis);
    if(regIndex != -1){
        pThis->regFull[index] = 1;
        int regNum = regIndex + pThis->firstRegNum;
        return regNum;
    }
    
    /* if no empty register, spill one register out */
    regIndex = findEarlestUsedReg(pThis);
    spillReg(pThis, regIndex, targetFile);
    int regNum = regIndex + pThis->firstRegNum; // s0 = r16 in mips
    return regNum;
}

int useReg(RegisterManager* pThis, int regNum, AST_NODE* nodeUseThisReg){
    /* AST_NODE use register, build link from register Manager to AST_NODE */
    int regIndex = regNum - pThis->firstRegNum;
    pThis->regUser[regIndex] = nodeUseThisReg;
}

void releaseReg(RegisterManager* pThis, int regNum){
    /* release used register to free register.
     * break link from Register Manager to AST_NODE */
    int regIndex = regNum - pThis->firstRegNum;
    pThis->regFull[regIndex] = 0;
    pThis->regUser[regIndex] = NULL;
}

int findEmptyReg(RegisterManager* pThis){
    /* find the nearest(compare to lastReg) empty register.
     * if no empty register. return -1 */
    int index = pThis->lastReg+1;
    while(index != pThis->lastReg){
        if(!pThis->regFull[index]){
            pThis->lastReg = index;
            return index;
        }

        index = (index+1) % pThis->numOfReg;
    }
    return -1;
}

int findEarlestUsedReg(RegisterManager* pThis){
    /* find the earlest allocated register.
     * the function used when all register is full. */
    pThis->lastReg = (pThis->lastReg + 1) % pThis->numOfReg;
    return pThis->lastReg;
}

void spillReg(RegisterManager* pThis, int regIndex, FILE* targetFile){
    /* spill value of register to the runtime stack, then release this register */
    if(pThis->regUser[regIndex]){
        /* if AST_NODE (register user) exist */
        ExpValPlace* place = pThis->regUser[regIndex]->valPlace;
        int regNum = regIndex + pThis->firstRegNum;

        /* store value of register into stack */
        fprintf(targetFile, "sw $r%d, %d($fp)\n", regNum, GR.stackTop + 4);
        place->data_type = INT_TYPE;
        place->kind = STACK_TYPE;
        place->place.stackOffset = GR.stackTop + 4;
        GR.stackTop += 4;
    }

    releaseReg(pThis, regNum);
}

/*** MIPS instruction generation ***/
void genIntUnaryOpInstr(FILE* targetFile, UNARY_OPERATOR op, int destRegNum, int srcRegNum){
    switch(op){
        UNARY_OP_POSITIVE: genPosOpInstr(targetFile, destRegNum, srcRegNum); break;
        UNARY_OP_NEGATIVE: genNegOpInstr(targetFile, destRegNum, srcRegNum); break;
        UNARY_OP_LOGICAL_NEGATION: genNOTExpr(targetFile, destRegNum, srcRegNum); break;
    }
}

void genFloatUnaryOpInstr(FILE* targetFile, UNARY_OPERATOR op, int destRegNum, int srcRegNum){
    switch(op){
        UNARY_OP_POSITIVE: genFPPosOpInstr(targetFile, distRegNum, srcRegNum); break;
        UNARY_OP_NEGATIVE: genFPNegOpInstr(targetFile, distRegNum, srcRegNum); break;
        UNARY_OP_LOGICAL_NEGATION: assert(0); break;
    }
}

void genIntBinaryOpInstr(FILE* targetFile, BINARY_OPERATOR op, 
  int destRegNum, int src1RegNum, int src2RegNum){
    switch(op){
        BINARY_OP_ADD: genAddOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_SUB: genSubOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_MUL: genMulOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_DIV: genDivOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_EQ: genEQExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_GE: genGEExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_LE: genLEExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_NE: genNEExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_GT: genGTExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_LT: genLTExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_AND: genANDExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_OR: genORExpr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
    }
}

void genFloatBinaryOpInstr(FILE* targetFile, BINARY_OPERATOR op, 
  int destRegNum, int src1RegNum, int src2RegNum){
    switch(op){
        BINARY_OP_ADD: genFPAddOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_SUB: genFPSubOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_MUL: genFPMulOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_DIV: genFPDivOpInstr(targetFile, destRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_EQ: genFPEQInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_GE: genFPNEInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_LE: genFPLTInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_NE: genFPGTInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_GT: genFPGEInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_LT: genFPLEInstr(targetFile, distRegNum, src1RegNum, src2RegNum); break;
        BINARY_OP_AND: assert(0); break;
        BINARY_OP_OR: assert(0); break;
    }
}

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

// floating arithmetic operation
void genFPAddOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "add.s $f%d, $f%d, $f%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genFPSubOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "sub.s $f%d, $f%d, $f%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genFPMulOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "mul.s %f%d, $f%d, $f%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genFPDivOpInstr(FILE* targetFile, int destRegNum, int src1RegNum, int src2RegNum){
    fprintf(targetFile, "div.s %f%d, $f%d, $f%d\n", destRegNum, src1RegNum, src2RegNum);
}

void genFPEQInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.eq.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPNEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.eq.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPLTInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.lt.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPGTInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.le.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPGEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.lt.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPLEInstr(FILE* targetFile, int distRegNum, int src1RegNum, int src2RegNum){
    int falseLabel = GR.labelCounter++;
    int exitLabel  = GR.labelCounter++;
    
    fprintf(targetFile, "c.le.s $f%d, $f%d\n", src1RegNum, src2RegNum);
    fprintf(targetFile, "bc1f L%d\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 1.0\n", distRegNum);
    fprintf(targetFile, "j L%d\n", exitLabel)
    fprintf(targetFile, "L%d:\n", falseLabel);
    fprintf(targetFile, "li.s $f%d, 0.0\n", distRegNum);
    fprintf(targetFile, "L%d:\n", exitLabel);
}

void genFPPosOpInstr(FILE* targetFile, int distRegNum, int srcRegNum){
    fprintf(targetFile, "mov.s $f%d, $f%d\n", distRegNum, srcRegNum);
}

void genFPNegOpInstr(FILE* targetFile, int distRegNum, int srcRegNum){
    fprintf(targetFile, "neg.s $f%d, $f%d\n", distRegNum, srcRegNum);
}

// casting
void genFloatToInt(FILE* targetFile, int destRegNum, int floatRegNum){
    fprintf(targetFile, "cvt.w.s $f%d\n", floatRegNum, floatRegNum);
    fprintf(targetFile, "mfc1 %r%d, $f%d\n", destRegNum, floatRegNum);
}

void genIntToFloat(FILE* targetFile, int destRegNum, int intRegNum){
    fprintf(targetFile, "mtc1 $r%d, $f%d\n", intRegNum, destRegNum);
    fprintf(targetFile, "cvt.s.w $f%d, $f%d\n", destRegNum, destRegNum);
}
