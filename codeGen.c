#include <stdio.h>
#include "codeGen.h"

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

/*** variable declare ***/
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
        if(blockChild->nodeType == STMT_LIST_NODE)
            localVarSize += genStmtList(targetFile, symbolTable, blockChild, funcName);
        blockChild = blockChild->rightSibling;
    }

    genEpilogue(targetFile, funcName, localVarSize);
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
