#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
#include "semanticError.h"

int isIgnoreFunctionName(char* name){
    if(strncmp(name, "read", 4) == 0) return 1;
    if(strncmp(name, "fread", 5) == 0) return 1;
    if(strncmp(name, "write", 5) == 0) return 1;
    return 0;
}

int g_anyErrorOccur = 0;
/* void semanticAnalysis(AST_NODE* prog, STT* symbolTable); 
 * declared in header.h 
 */

/* Declarations(build symbolTable) */
void processVariableDeclList(STT* symbolTable, AST_NODE* variableDeclListNode);
void processVariableDecl(STT* symbolTable, AST_NODE* declarationNode);
void processFunctionDecl(STT* symbolTable, AST_NODE* declarationNode);
    /* usage inner functions */
DATA_TYPE typeNameToType(STT* symbolTable, char* typeName, int allowTypeDef);
void declareTypeID(STT* symbolTable, AST_NODE* idNode, DATA_TYPE primitiveType);
void declareScalarArrayID(STT* symbolTable, AST_NODE* idNode, DATA_TYPE primitiveType);
int isDeclaredCurScope(STT* symbolTable, char* name);
TypeDescriptor* idNodeToTypeDescriptor(AST_NODE* idNode, DECL_KIND declKind, DATA_TYPE primitiveType);
int idNodeIsArray(AST_NODE* idNode, DECL_KIND declKind);
int constExprEvaluation(AST_NODE* cexprNode, int* isError);

int countRightSibling(AST_NODE* ASTNode);
ParameterNode* GenParameterNodeList(STT* symbolTable, AST_NODE* paraListNode);

/* Statement checking */
void processStmtList(STT* symbolTable, AST_NODE* StmtListNode, char* funcName);
void checkStmt(STT* symbolTable, AST_NODE* StmtNode,char* funcName);

void checkBlock(STT *symbolTable, AST_NODE* blockNode, char* funcName);
void checkWhileStmt(STT* symbolTable, AST_NODE* whileStmtNode, char* funcName);
void checkForStmt(STT* symbolTable, AST_NODE* ifStmtNode, char* funcName);
void checkIfStmt(STT* symbolTable, AST_NODE* forStmtNode, char* funcName);
void checkAssignmentStmt(STT* symbolTable, AST_NODE* assignmentNode);
void checkFuncCallStmt(STT* symbolTable, AST_NODE* expressionNode);
void checkReturnStmt(STT* symbolTable, AST_NODE* returnNode, char* funcName);

void checkAssignExpr(STT* symbolTable, AST_NODE* exprNode);
DATA_TYPE getTypeOfExpr(STT* symbolTable, AST_NODE* exprNode);
void checkDimension(STT *symbolTable, AST_NODE* dimensionNode, int isFuncPara);
void checkExpr(STT* symbolTable, AST_NODE* expressionNode, int isFuncPara);

/* --- Function Definition --- */
void semanticAnalysis(AST_NODE* prog, STT* symbolTable){
    AST_NODE* child = prog->child;
    while(child){
        if(child->nodeType == VARIABLE_DECL_LIST_NODE)
            processVariableDeclList(symbolTable, child);
        else if(child->nodeType == DECLARATION_NODE)
            processFunctionDecl(symbolTable, child);
        child = child->rightSibling;
    }
}

/* Declarations(build symbolTable) */
void processVariableDeclList(STT* symbolTable, AST_NODE* variableDeclListNode){
    /* process Declaration List, build Symbol Table */
    AST_NODE* child = variableDeclListNode->child;
    while(child){
        processVariableDecl(symbolTable, child);
        child = child->rightSibling; 
    }
}

void processVariableDecl(STT* symbolTable, AST_NODE* declarationNode){
    /* kind: VARIABLE_DECL, TYPE_DECL, FUNCTION_PARAMETER_DECL
     * Notice: it can more than one variable declaration, like int a, b, c;
     */
    DECL_KIND declKind = declarationNode->semantic_value.declSemanticValue.kind;
    AST_NODE* typeNode = declarationNode->child;
    char* primitiveTypeName = typeNode->semantic_value.identifierSemanticValue.identifierName;
    if(declKind == TYPE_DECL){
        DATA_TYPE primitiveType = typeNameToType(symbolTable, primitiveTypeName, 0);
        if(primitiveType == NONE_TYPE)
            return; /* grammar error */
        
        AST_NODE* idNode = typeNode->rightSibling;
        while(idNode){
            declareTypeID(symbolTable, idNode, primitiveType);
            idNode = idNode->rightSibling;
        }
    }
    else if(declKind == VARIABLE_DECL || declKind == FUNCTION_PARAMETER_DECL){
        DATA_TYPE primitiveType = typeNameToType(symbolTable, primitiveTypeName, 1);
        if(primitiveType == NONE_TYPE){
            printErrorMissingDecl(typeNode, primitiveTypeName);
            return;
        }

        AST_NODE* idNode = typeNode->rightSibling;
        while(idNode){
            declareScalarArrayID(symbolTable, idNode, primitiveType);
            idNode = idNode->rightSibling;
        }
    }
}

void processFunctionDecl(STT* symbolTable, AST_NODE* funcDeclarationNode){
    /* process function definition */
    /* first: add function into symbol table */
    AST_NODE* returnTypeNode = funcDeclarationNode->child;
    AST_NODE* funcNameNode = returnTypeNode->rightSibling;
    AST_NODE* paraListNode = funcNameNode->rightSibling;

    char* funcName = funcNameNode->semantic_value.identifierSemanticValue.identifierName;
    if(isDeclaredCurScope(symbolTable, funcName)){
        printErrorRedeclaredVar(funcNameNode, funcName);
        return;
    }
    
    char* returnTypeName = returnTypeNode->semantic_value.identifierSemanticValue.identifierName;
    DATA_TYPE returnType = typeNameToType(symbolTable, returnTypeName, 1);
    TypeDescriptor* returnTypeDescriptor = NULL;
    if(returnType == NONE_TYPE){
        printErrorMissingDecl(returnTypeNode, returnTypeName);
        return;
    }
    else
        returnTypeDescriptor = createScalarTypeDescriptor(returnType);
        
    SymbolTableEntryKind kind = FUNC_ENTRY;

    int paraNum = countRightSibling(paraListNode->child);
    ParameterNode* paraList = GenParameterNodeList(symbolTable, paraListNode);

    SymbolTableEntry* entry = createSymbolTableEntry(funcName, kind, returnTypeDescriptor, paraNum, paraList);
    addSymbolByEntry(symbolTable, entry);
    /* second - into block: openscope and add function parameter into symbolTable 
     *                      and processing Decl_list + Stmt_list
     */
    AST_NODE* blockNode = paraListNode->rightSibling;
    openScope(symbolTable, BUILD);
    /* add function parameter into symboltable*/
    AST_NODE* funcParaNode = paraListNode->child;
    while(funcParaNode){
        processVariableDecl(symbolTable, funcParaNode);
        funcParaNode = funcParaNode->rightSibling;
    }
    AST_NODE* blockChild = blockNode->child;
    while(blockChild){
        if(blockChild->nodeType == VARIABLE_DECL_LIST_NODE)
            processVariableDeclList(symbolTable, blockChild);
        if(blockChild->nodeType == STMT_LIST_NODE)
            processStmtList(symbolTable, blockChild, funcName);
        blockChild = blockChild->rightSibling;
    }
    closeScope(symbolTable);
}

int constExprEvaluation(AST_NODE* cexprNode, int* pIsError){
    /* processing const expression
     * used in array declaration to compute size of each dimension
     * cexpr -> mcexpr -> cfactor -> (cexpr) 
     *
     * if expr isn't an integer, print error message.
     */
    int alloc = 0;
    if(!pIsError){
        pIsError = malloc(sizeof(int));
        *pIsError = 0;
        alloc = 1;
    }

    if(cexprNode->nodeType == CONST_VALUE_NODE){
        CON_Type* constValue = cexprNode->semantic_value.const1;
        if(constValue->const_type == FLOATC){
            if(!(*pIsError)){
                printErrorArraySubNotInt(cexprNode);
                *pIsError = 1;
            }
            if(alloc) free(pIsError);
            return constValue->const_u.fval;
        }
        if(alloc) free(pIsError);
        return constValue->const_u.intval;
    }
    else if(cexprNode->nodeType == EXPR_NODE){
        /* only binary op + - * / */
        AST_NODE* child1 = cexprNode->child;
        AST_NODE* child2 = child1->rightSibling;
        int val1 = constExprEvaluation(child1, pIsError);
        int val2 = constExprEvaluation(child2, pIsError);
        int retVal;
        switch(cexprNode->semantic_value.exprSemanticValue.op.binaryOp){
            case BINARY_OP_ADD: retVal = val1 + val2; break;
            case BINARY_OP_SUB: retVal = val1 - val2; break;
            case BINARY_OP_MUL: retVal = val1 * val2; break;
            case BINARY_OP_DIV: retVal = val1 / val2; break;
        }
        if(alloc) free(pIsError);
        return retVal;
    }
    if(alloc) free(pIsError);
    return 0; /* grammar error */
}

int idNodeIsArray(AST_NODE* idNode, DECL_KIND declKind){
    /* just check variable, typedef, array now
     * no processing function and function parameter
     */
    IDENTIFIER_KIND idKind = idNode->semantic_value.identifierSemanticValue.kind;
    if(declKind == TYPE_DECL)
        return 0;
    if(idKind == NORMAL_ID || idKind == WITH_INIT_ID)
        return 0;
    return 1;
}

TypeDescriptor* idNodeToTypeDescriptor(AST_NODE* idNode, DECL_KIND declKind, DATA_TYPE primitiveType){
    /* variable(scalar)    declKind == VARIABLE_DECL, idKind == NORMAL_ID | WITH_INIT_ID
     * typedef             declKind == TYPE_DECL
     * array               declKind == VARIABLE_DECL, idKind == ARRAY_ID
     * function parameter
     * function
     */
    IDENTIFIER_KIND idKind = idNode->semantic_value.identifierSemanticValue.kind;
    char* idName = idNode->semantic_value.identifierSemanticValue.identifierName;

    if(declKind == TYPE_DECL || idKind == NORMAL_ID || idKind == WITH_INIT_ID){
        /* typedef or scalar */
        TypeDescriptor* pType = createScalarTypeDescriptor(primitiveType);
        return pType;
    }
    else if(idKind == ARRAY_ID){
        /* array */
        int dimension = 0;
        AST_NODE* pThisDimensionNode = idNode->child;
        int sizeInEachDimension[MAX_ARRAY_DIMENSION];
        while(pThisDimensionNode){
            sizeInEachDimension[dimension] = constExprEvaluation(pThisDimensionNode, NULL);
            dimension += 1;
            pThisDimensionNode = pThisDimensionNode->rightSibling;
        }
        TypeDescriptor* pType = createArrayTypeDescriptor(primitiveType,
          dimension, sizeInEachDimension);
        return pType;
    }
    return NULL; 
}

int isDeclaredCurScope(STT* symbolTable, char* name){
    /* check whether name is declared in current scope */
    if(lookupSymbolCurrentScope(symbolTable, name) != NULL)
        return 1;
    return 0;
}

void declareTypeID(STT* symbolTable, AST_NODE* idNode, DATA_TYPE primitiveType){
    /* Declare typedef idNode and check redeclaration.
     * If redeclaration, the latter declaration will be ignored.
     */
    char* name = idNode->semantic_value.identifierSemanticValue.identifierName;
    if(isDeclaredCurScope(symbolTable, name)){
        printErrorRedeclaredVar(idNode, name);
        return;
    }
    
    SymbolTableEntryKind kind = TYPE_ENTRY;
    TypeDescriptor* type = idNodeToTypeDescriptor(idNode, TYPE_DECL, primitiveType);

    SymbolTableEntry* entry = createSymbolTableEntry(name, kind, type, 0, NULL);
    addSymbolByEntry(symbolTable, entry);
}

void declareScalarArrayID(STT* symbolTable, AST_NODE* idNode, DATA_TYPE primitiveType){
    /* Declare Scalar or Array idNode and check redeclaration.
     * If redeclaration, the latter declaration will be ignored.
     */
    char* name = idNode->semantic_value.identifierSemanticValue.identifierName;
    if(isDeclaredCurScope(symbolTable, name)){
        printErrorRedeclaredVar(idNode, name);
        return;
    }

    SymbolTableEntryKind kind;
    if(idNodeIsArray(idNode, VARIABLE_DECL))
        kind = ARRAY_ENTRY;
    else
        kind = VAR_ENTRY;

    TypeDescriptor* type = idNodeToTypeDescriptor(idNode, VARIABLE_DECL, primitiveType);

    SymbolTableEntry* entry = createSymbolTableEntry(name, kind, type, 0, NULL);
    addSymbolByEntry(symbolTable, entry);
    idNode = idNode->rightSibling;
}

DATA_TYPE typeNameToType(STT* symbolTable, char* typeName, int allowTypeDef){
    /* check type name exist(ex. "int" ), and transform to DATA_TYPE enum type 
     * return NONE_TYPE means typeName doesn't exist
     */
    int len = strlen(typeName);
    if(len == 3 && strncmp(typeName, "int", 3) == 0)
        return INT_TYPE;
    if(len == 5 && strncmp(typeName, "float", 5) == 0)
        return FLOAT_TYPE;
    if(len == 4 && strncmp(typeName, "void", 4) == 0)
        return VOID_TYPE;
    if(!allowTypeDef)
        return NONE_TYPE;

    SymbolTableEntry* entry = lookupSymbol(symbolTable, typeName);
    if(!entry) 
        return NONE_TYPE;
    if(entry->kind != TYPE_ENTRY) 
        return NONE_TYPE; /* UNFINISH: */
    return entry->type->primitiveType;
}

int countRightSibling(AST_NODE* ASTNode){
    /* count the num of rightSibling node, include self */
    int counter = 0;
    while(ASTNode){
        counter += 1;
        ASTNode = ASTNode->rightSibling;
    }
    return counter;
}

ParameterNode* GenParameterNodeList(STT* symbolTable, AST_NODE* paraListNode){
    /* use AST parameter list node to generate ParameterNodeList for symbolTableEntry */
    int paraNum = countRightSibling(paraListNode->child);
    TypeDescriptor** typeOfParas = NULL; 
    char** nameOfParas = NULL;
    /* array of TypeDescriptor*, each point to one TypeDescriptor of one parameter */
    if(paraNum){
        typeOfParas = malloc(sizeof(TypeDescriptor*) * paraNum);
        nameOfParas = malloc(sizeof(char*) * paraNum);
        int i;
        AST_NODE* funcParaNode = paraListNode->child;
        for(i=0; i<paraNum; i++){
            AST_NODE* typeNode = funcParaNode->child;
            char* typeName = typeNode->semantic_value.identifierSemanticValue.identifierName;
            DATA_TYPE primitiveType = typeNameToType(symbolTable, typeName, 1);
            typeOfParas[i] = idNodeToTypeDescriptor(typeNode->rightSibling, VARIABLE_DECL, primitiveType);

            AST_NODE* paraIdNode = typeNode->rightSibling;
            nameOfParas[i] = paraIdNode->semantic_value.identifierSemanticValue.identifierName;

            funcParaNode = funcParaNode->rightSibling;
        }
    }
    ParameterNode* paraList = createParameterList(paraNum, typeOfParas, nameOfParas);
    free(typeOfParas);
    free(nameOfParas);
    return paraList;
}

/* Statement checking */
void processStmtList(STT* symbolTable, AST_NODE* StmtListNode, char* funcName){
    
    AST_NODE* child = StmtListNode->child;

    while( child ){
        checkStmt(symbolTable, child, funcName);
        child = child -> rightSibling;
    }
}

void checkStmt(STT* symbolTable, AST_NODE* StmtNode, char* funcName){
    
    if( StmtNode->nodeType == BLOCK_NODE )
        checkBlock( symbolTable, StmtNode, funcName );
    else if( StmtNode->nodeType == STMT_NODE ){
        
        STMT_KIND stmtKind = StmtNode->semantic_value.stmtSemanticValue.kind;
        switch( stmtKind ){
            case WHILE_STMT: checkWhileStmt(symbolTable, StmtNode, funcName); break;
            case FOR_STMT: checkForStmt(symbolTable, StmtNode, funcName); break;
            case IF_STMT: checkIfStmt(symbolTable, StmtNode, funcName); break;
            case ASSIGN_STMT: checkAssignmentStmt(symbolTable, StmtNode); break;
            case FUNCTION_CALL_STMT: checkFuncCallStmt(symbolTable, StmtNode); break;
            case RETURN_STMT: checkReturnStmt(symbolTable, StmtNode, funcName); break;
        }
    }
}


void checkBlock(STT *symbolTable, AST_NODE* blockNode, char* funcName){

    openScope(symbolTable, BUILD);

    AST_NODE* child = blockNode->child;
    while( child ){
    
        if( child->nodeType == VARIABLE_DECL_LIST_NODE )
            processVariableDeclList(symbolTable, child);
        else if( child->nodeType == STMT_LIST_NODE )
            processStmtList(symbolTable, child, funcName);

        child = child->rightSibling;
    }

    closeScope(symbolTable);
}

void checkWhileStmt(STT* symbolTable, AST_NODE* whileStmtNode, char* funcName){
    AST_NODE* testNode = whileStmtNode->child;
    AST_NODE* subStmtNode = testNode->rightSibling;
    AST_TYPE testType = testNode->nodeType;

    checkAssignExpr(symbolTable, testNode);

    checkStmt(symbolTable, subStmtNode, funcName);
}

void checkForStmt(STT* symbolTable, AST_NODE* forStmtNode, char* funcName){
    AST_NODE* initCondNode = forStmtNode->child;
    AST_NODE* terminateCondNode = initCondNode->rightSibling;
    AST_NODE* incrementNode = terminateCondNode->rightSibling;
    AST_NODE* subStmtNode = incrementNode->rightSibling;

    if(initCondNode){
        AST_NODE* exprNode = initCondNode->child;
        while(exprNode){
            checkAssignExpr(symbolTable, exprNode);
            exprNode = exprNode->rightSibling;
        }
    }
    if(terminateCondNode){
        AST_NODE* exprNode = terminateCondNode->child;
        while(exprNode){
            checkExpr(symbolTable, exprNode, 0);
            exprNode = exprNode->rightSibling;
        }
    }
    if(incrementNode){
        AST_NODE* exprNode = incrementNode->child;
        while(exprNode){
            checkAssignExpr(symbolTable, exprNode);
            exprNode = exprNode->rightSibling;
        }
    }

    checkStmt(symbolTable, subStmtNode, funcName);
}

void checkIfStmt(STT* symbolTable, AST_NODE* ifStmtNode, char* funcName){
    AST_NODE* testNode = ifStmtNode->child;
    AST_NODE* subStmtNode = testNode->rightSibling;
    AST_NODE* elseStmtNode = subStmtNode->rightSibling;
    AST_TYPE testType = testNode->nodeType;

    checkAssignExpr(symbolTable, testNode);

    checkStmt(symbolTable, subStmtNode, funcName);

    if(elseStmtNode)
        checkStmt(symbolTable, elseStmtNode, funcName);
}

void checkAssignmentStmt(STT* symbolTable, AST_NODE* assignmentNode){
    
    char *name = assignmentNode->child->semantic_value.identifierSemanticValue.identifierName;
    SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                               
    if( !Entry ){
        printErrorMissingDecl(assignmentNode, name);
        return;
    }
    
    if( Entry->type->dimension > 0 )
        checkDimension(symbolTable, assignmentNode->child, 0); // 0 means not function parameter

    checkExpr(symbolTable, assignmentNode->child->rightSibling, 0); // 0 means not function parameter

}

void checkFuncCallStmt(STT* symbolTable, AST_NODE* expressionNode){
    char *name = expressionNode->child->semantic_value.identifierSemanticValue.identifierName;
    
    if(isIgnoreFunctionName(name))
        return;

    SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                           
    if( !Entry ){
        printErrorMissingDecl(expressionNode, name);
        return;
    }
    
    AST_NODE* parametersParent = expressionNode->child->rightSibling;
    int counter = 0;
    if(parametersParent)
        counter = countRightSibling(parametersParent->child);

    if( counter < Entry->numOfParameters )
        printErrorTooFewArgs(expressionNode, name);

    else if( counter > Entry->numOfParameters )
        printErrorTooManyArgs(expressionNode, name);

    if(counter == 0)
        /* no parameter checking */
        return;
    // scalar != array type

    ParameterNode* cursor = Entry->functionParameterList;

    AST_NODE* AST_cursor = parametersParent->child;
    while( cursor ){
        SymbolTableEntryKind Label = VAR_ENTRY;

        if( AST_cursor->nodeType == IDENTIFIER_NODE ){
            char* ASTIdName = AST_cursor->semantic_value.identifierSemanticValue.identifierName;
            SymbolTableEntry* AST_entry = lookupSymbol(symbolTable, ASTIdName);
            if( AST_entry->type->dimension > 0){
                /* array */
                AST_NODE* dim = AST_cursor->child;
                int counter = 0;
                while( dim ){
                    counter++;
                    dim = dim->rightSibling;
                }
                if( AST_entry->type->dimension != counter )
                    Label = ARRAY_ENTRY;
            }
        }

        if( cursor->type->dimension == 0 && Label == ARRAY_ENTRY )
            printErrorArrayPassToScal(AST_cursor, cursor->name,
                                      AST_cursor->semantic_value.identifierSemanticValue.identifierName);

        else if( cursor->type->dimension > 0 && Label == VAR_ENTRY ){
            if(AST_cursor->nodeType != IDENTIFIER_NODE)
                printErrorNonIdScalPassToArray(AST_cursor, 
                                               AST_cursor->semantic_value.identifierSemanticValue.identifierName);
            else 
                printErrorScalPassToArray(AST_cursor, cursor->name, 
                                          AST_cursor->semantic_value.identifierSemanticValue.identifierName);
        }

        cursor = cursor -> next;
        AST_cursor = AST_cursor -> rightSibling;
    }

    AST_cursor = expressionNode->child->rightSibling->child;
    while( AST_cursor ){
        
        checkExpr(symbolTable, AST_cursor, 1);
        AST_cursor = AST_cursor -> rightSibling;
    }
}

void checkReturnStmt(STT* symbolTable, AST_NODE* returnNode, char* funcName){
    
    // check expr valid
    checkExpr(symbolTable, returnNode->child, 0);
    // get the expr type 
    SymbolTableEntry *Entry = lookupSymbol(symbolTable, funcName);                                             
    
    DATA_TYPE type = getTypeOfExpr(symbolTable, returnNode->child);
    
    if( type == NONE_TYPE )
        printErrorInvalidExpr( returnNode->child );
    if( type != Entry->type->primitiveType )
        printWarningReturnTypeMismatch( returnNode );

}

void checkAssignExpr(STT* symbolTable, AST_NODE* exprNode){
    /* test, assign_expr(grammar): checkAssignmentStmt or checkExpr */
    if(exprNode->nodeType == STMT_NODE){
        if(exprNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
            checkAssignmentStmt(symbolTable, exprNode);
    }
    else 
        checkExpr(symbolTable, exprNode, 0);
}

void checkExpr(STT* symbolTable, AST_NODE* expressionNode, int isFuncPara){
    
    if( expressionNode->nodeType == IDENTIFIER_NODE ){
            
        char *name = expressionNode->semantic_value.identifierSemanticValue.identifierName;
        SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                                   
        if( !Entry ){
            printErrorMissingDecl(expressionNode, name);
            return;
        }

        if( Entry->type->dimension > 0 ) // dimension > 0 means array
            checkDimension(symbolTable, expressionNode, isFuncPara);
    }

    else if( expressionNode->nodeType == EXPR_NODE ){
        
        // check expr valid

        if( expressionNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION )
            checkExpr(symbolTable, expressionNode->child, 0);
        
        else if( expressionNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION ){
        
            checkExpr(symbolTable, expressionNode->child, 0);
            checkExpr(symbolTable, expressionNode->child->rightSibling, 0);
        }

        else if( getTypeOfExpr( symbolTable, expressionNode ) == NONE_TYPE )
            printErrorInvalidExpr( expressionNode );
    }
    
    else if( expressionNode->nodeType == STMT_NODE ){
        checkFuncCallStmt(symbolTable, expressionNode);
    }
}

void checkDimension(STT *symbolTable, AST_NODE* dimensionNode, int isFuncPara){
    
    char *name = dimensionNode->semantic_value.identifierSemanticValue.identifierName;
    SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);

    // check dimension match
    AST_NODE* child = dimensionNode->child;
    int dimCounter = 0;
    while( child ){
        
        dimCounter++;
        child = child->rightSibling;
    }

    if( dimCounter > Entry->type->dimension )
        printErrorDimMismatch( dimensionNode );

    if( dimCounter < Entry->type->dimension && !isFuncPara )
        printErrorDimMismatch( dimensionNode );

    // check if array subscript is int
    child = dimensionNode->child;
    while( child ){
        DATA_TYPE exprType = getTypeOfExpr(symbolTable, child);
        if(exprType != INT_TYPE)
            printErrorArraySubNotInt( child );

        child = child->rightSibling;
    }
}


DATA_TYPE getTypeOfExpr(STT* symbolTable, AST_NODE* exprNode){
    /*
     * NUL_NODE -> VOID_TYPE
     * CONST_NODE -> const_type: (INTEGERC, FLOATC) -> INT_TYPE, FLOAT_TYPE
     * FUNCTION -> returnType: (int, float, void) -> INT_TYPE, FLOAT_TYPE, VOID_TYPE
     * ID -> (SYM_TABLE dimension = 0) int, float. -> INT_TYPE, FLOAT_TYPE
             (SYM_TABLE dimension > 0) 
                SYM_TABLE.dimension > AST_NODE.dimension: array -> Invalid(NONE_TYPE)
                SYM_TABLE.dimension = AST_NODE.dimension: int, float -> INT_TYPE, FLOAT_TYPE
     * EXPR -> childs:
                if NONE_TYPE exist or VOID_TYPE exist -> Invalid(NONE_TYPE)
                else if FLOAT_TYPE exist -> FLOAT_TYPE
                else -> INT_TYPE
     */
    if( exprNode->nodeType == NUL_NODE )
        return VOID_TYPE;
    else if( exprNode->nodeType == CONST_VALUE_NODE ){
        
        if( exprNode->semantic_value.const1->const_type == INTEGERC)
            return INT_TYPE;
        else if ( exprNode->semantic_value.const1->const_type == FLOATC )
            return FLOAT_TYPE;

    }
    else if( exprNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT ){
        
        char *name = exprNode->child->semantic_value.identifierSemanticValue.identifierName;
        SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);
        
        if( Entry->type->primitiveType == INT_TYPE )
            return INT_TYPE;
        else if( Entry->type->primitiveType == FLOAT_TYPE )
            return FLOAT_TYPE;
        if( Entry->type->primitiveType == VOID_TYPE )
            return VOID_TYPE;

    }
    else if( exprNode->nodeType == IDENTIFIER_NODE ){
       
        char *name = exprNode->semantic_value.identifierSemanticValue.identifierName;
        SymbolTableEntry *Entry = lookupSymbol(symbolTable, name);
        
        if( Entry->type->dimension == 0 ){
            if( Entry->type->primitiveType == INT_TYPE )
                return INT_TYPE;
            else if( Entry->type->primitiveType == FLOAT_TYPE )
                return FLOAT_TYPE;
        }
        else if( Entry->type->dimension > 0 ){
            
            AST_NODE* child = exprNode->child;
            int dimCounter = 0;
            while( child ){
                
                dimCounter++;
                child = child -> rightSibling;
            }

            if( Entry->type->dimension > dimCounter )   
                return NONE_TYPE;
            else if( Entry->type->dimension == dimCounter ){
                
                if( Entry->type->primitiveType == INT_TYPE )
                    return INT_TYPE;
                else if( Entry->type->primitiveType == FLOAT_TYPE )
                    return FLOAT_TYPE;
            }
        }
    }
    else if( exprNode->nodeType == EXPR_NODE ){
        AST_NODE* child = exprNode->child;
        
        DATA_TYPE tempType = INT_TYPE;
        while( child ){
            
            DATA_TYPE type = getTypeOfExpr(symbolTable, child);

            if( type == NONE_TYPE || type == VOID_TYPE )
                return NONE_TYPE;
            else if( type == FLOAT_TYPE )
                tempType = FLOAT_TYPE;

            child = child->rightSibling;
        }

        return tempType;
    }
}
