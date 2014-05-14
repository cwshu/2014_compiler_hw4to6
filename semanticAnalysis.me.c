#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.me.h"
#include "semanticError.me.h"

int g_anyErrorOccur = 0;
/* void semanticAnalysis(AST_NODE* prog, STT* symbolTable); 
 * declared in header.h 
 */

/* Declarations(build symbolTable) */
void processVariableDeclList(STT* symbolTable, AST_NODE* variableDeclListNode);
void processVariableDecl(STT* symbolTable, AST_NODE* declarationNode);
void processFunctionDecl(AST_NODE* declarationNode);
    /* usage inner functions */
DATA_TYPE typeNameToType(STT* symbolTable, char* typeName, int allowTypeDef);
void declareTypeID(STT* symbolTable, AST_NODE* idNode);
void declareScalarArrayID(STT* symbolTable, AST_NODE* idNode);
int isDeclaredCurScope(STT* symbolTable, char* name);
TypeDescriptor* idNodeToTypeDescriptor(AST_NODE* idNode, DECL_KIND declKind, DATA_TYPE primitiveType);
int idNodeIsArray(AST_NODE* idNode, DECL_KIND declKind);
int constExprEvaluation(AST_NODE* cexprNode);

int countRightSibling(AST_NODE* ASTNode);
ParameterNode* GenParameterNodeList(STT* symbolTable, AST_NODE* paraListNode){

/* Statement checking */
void processStmtList(STT* symbolTable, AST_NODE* StmtListNode);
void checkStmt(STT* symbolTable, AST_NODE* StmtNode);

void checkBlock(STT *symbolTable, AST_NODE* blockNode);
void checkWhileStmt(STT* symbolTable, AST_NODE* whileStmtNode);
void checkForStmt(STT* symbolTable, AST_NODE* ifStmtNode);
void checkIfStmt(STT* symbolTable, AST_NODE* forStmtNode);
void checkAssignmentStmt(STT* symbolTable, AST_NODE* assignmentNode);
void checkFuncCallStmt(STT* symbolTable, AST_NODE* expressionNode);
void checkReturnStmt(STT* symbolTable, char* funcName, AST_NODE* returnNode);



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
        processVariableDecl(child):
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
            declareTypeID(symbolTable, idNode);
            idNode = idNode->rightSibling;
        }
    }
    else if(declKind == VARIABLE_DECL || declKind == FUNCTION_PARAMETER_DECL){
        DATA_TYPE primitiveType = typeNameToType(symbolTable, primitiveTypeName, 0);
        if(primitiveType == NONE_TYPE){
            printErrorMissingDecl(typeNode, primitiveTypeName);
        }

        AST_NODE* idNode = typeNode->rightSibling;
        while(idNode){
            declareScalarArrayID(symbolTable, idNode);
            idNode = idNode->rightSibling;
        }
    }
}

void processFunctionDecl(STT* symbolTable, AST_NODE* funcDeclarationNode){
    /* process function definition */
    /* first: add function into symbol table */
    AST_NODE* returnTypeNode = funcDeclarationNode->child;
    AST_NODE* funcNameNode = returnTypeNode->rightSibling;
    AST_NODE* paraListNode = funcNamenode->rightSibling;

    char* funcName = funcNameNode->semantic_value.identifierSemanticValue.identifierName;
    
    char* returnTypeName = returnTypeNode->semantic_value.identifierSemanticValue.identifierName;
    TypeDescriptor* returnType = typeNameToType(symbolTable, returnTypeName, 1);
    if(returnType == NULL){
        printErrorMissingDecl(returnTypeNode, returnTypeName);
    }
        
    SymbolTableEntryKind kind = FUNC_ENTRY;

    int paraNum = countRightSibling(paraListNode->child);
    ParameterNode* paraList = GenParameterNodeList(symbolTable, paraListNode);

    createSymbolTableEntry(funcName, kind, returnType, paraNum, paraList);
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
            processVariableDeclList(symbolTable, varListNode);
        if(blockChild->nodeType == STMT_LIST_NODE)
            /* checkStmtList(symbolTable, stmtListNode); */
        blockChild = blockChild->rightSibling;
    }
    closeScope(symbolTable);
}

int constExprEvaluation(AST_NODE* cexprNode, int* isInteger){
    /* processing const expression
     * used in array declaration to compute size of each dimension
     * cexpr -> mcexpr -> cfactor -> (cexpr) 
     *
     * if expr isn't an integer, print error message.
     */
    if(cexprNode->nodeType == CONST_VALUE_NODE){
        CON_Type* constValue = cexprNode->semantic_value.const1;
        if(constValue->const_type == FLOATC){
            printErrorArraySubNotInt(node);
            return constValue->const_u.fval;
        }
        return constValue->const_u.ival;
    }
    else if(cexprNode->nodeType == EXPR_NODE){
        /* only binary op + - * / */
        AST_NODE* child1 = cexprNode->child;
        AST_NODE* child2 = child1->rightSibling;
        int val1 = constExprEvaluation(child1);
        int val2 = constExprEvaluation(child2);
        int retVal;
        switch(cexprNode->semantic_value.exprSemanticValue.op.binaryOp){
            case BINARY_OP_ADD: retVal = val1 + val2; break;
            case BINARY_OP_SUB: retVal = val1 - val2; break;
            case BINARY_OP_MUL: retVal = val1 * val2; break;
            case BINARY_OP_DIV: retVal = val1 / val2; break;
        }
        return retVal;
    }
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
    }
    else if(idKind == ARRAY_ID){
        /* array */
        int dimension = 0;
        AST_NODE* pThisDimensionNode = idNode->child;
        int sizeInEachDimension[MAX_ARRAY_DIMENSION];
        while(pThisDimensionNode){
            dimension += 1;
            sizeInEachDimension = constExprEvaluation(pThisDimensionNode);
        }
        TypeDescriptor* pType = createArrayTypeDescriptor(primitiveType,
          dimension, sizeInEachDimension);
    }
   
    return pType;
}

int isDeclaredCurScope(STT* symbolTable, char* name){
    /* check whether name is declared in current scope */
    if(lookupSymbolCurrentScope(symbolTable, name) != NULL)
        return 1;
    return 0;
}

void declareTypeID(STT* symbolTable, AST_NODE* idNode){
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

void declareScalarArrayID(STT* symbolTable, AST_NODE* idNode){
    /* Declare Scalar or Array idNode and check redeclaration.
     * If redeclaration, the latter declaration will be ignored.
     */
    char* name = idNode->semantic_value.identifierSemanticValue.identifierName;
    if(isDeclaredCurScope(symbolTable, name)){
        printErrorRedeclaredVar(idNode, name);
        return;
    }

    SymbolTableEntryKind kind;
    if(idNodeIsArray(idNode, declKind))
        kind = ARRAY_ENTRY;
    else
        kind = VAR_ENTRY;

    TypeDescriptor* type = idNodeToTypeDescriptor(idNode, TYPE_DECL, primitiveType);

    SymbolTableEntry* entry = createSymbolTableEntry(name, kind, type, 0, NULL);
    addSymbolByEntry(symbolTable, entry);
    idNode = idNode->rightSibling;
}

DATA_TYPE typeNameToType(STT* symbolTable, char* typeName, int allowTypeDef){
    /* check type name exist(ex. "int" ), and transform to DATA_TYPE enum type 
     * return NONE_TYPE means typeName doesn't exist
     */
    if(strncpy(primitiveTypeName, "int", 3) == 0)
        return INT_TYPE;
    if(strncpy(primitiveTypeName, "float", 5) == 0)
        return FLOAT_TYPE;
    if(strncpy(primitiveTypeName, "void", 4) == 0)
        return VOID_TYPE;
    if(!allowTypeDef)
        return NONE_TYPE;

    SymbolTableEntry* entry = lookupSymbolInTable(symbolTable, primitiveTypeName);
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
    /* array of TypeDescriptor*, each point to one TypeDescriptor of one parameter */
    if(paraNum){
        typeOfParas = malloc(sizeof(TypeDescriptor*) * paraNum);
        int i;
        AST_NODE* funcParaNode = paraListNode->child;
        for(i=0; i<paraNum; i++){
            AST_NODE* typeNode = funcParaNode->child;
            char* typeName = typeNode->semantic_value.identifierSemanticValue.identifierName;
            DATA_TYPE primitiveType = typeNameToType(symbolTable, typeName, 1);
            typeOfParas[i] = idNodeToTypeDescriptor(typeNode->rightSibling, VARIABLE_DECL, primitiveType);

            funcParaNode = funcParaNode->rightSibling;
        }
    }
    ParameterNode* paraList = createParameterList(paraNum, typeOfParas);
    free(typeOfParas);
    return paraList;
}

/* Statement checking */
void processStmtList(STT* symbolTable, AST_NODE* StmtListNode){
    
    AST_NODE* child = StmtListNode->child;

    while( child ){
        checkStmt(symbolTable, child);
        child = child -> rightSibling;
    }
}

void checkStmt(STT* symbolTable, AST_NODE* StmtNode){
    
    if( StmtNode->nodeType == BLOCK_NODE )
        checkBlock( symbolTable, StmtNode );
    else if( StmtNode->nodeType == STMT_NODE ){
        
        STMT_KIND stmtKind = StmtNode->semantic_value.stmtSemanticValue.kind;
        switch( stmtKind ){
            case WHILE_STMT: checkWhileStmt(symbolTable, StmtNode); break;
            case FOR_STMT: checkForStmt(symbolTable, StmtNode); break;
            case IF_STMT: checkIfStmt(symbolTable, StmtNode); break;
            case ASSIGN_STMT: checkAssignmentStmt(symbolTable, StmtNode); break;
            case FUNCTION_CALL_STMT: checkFuncCallStmt(symbolTable, StmtNode); break;
            case RETURN_STMT: checkReturnStmt(symbolTable, StmtNode); break;
        }
    }
}


void checkBlock(STT *symbolTable, AST_NODE* blockNode){
    
    AST_NODE* child = blockNode->child;
    while( child ){
    
        if( child->nodeType == DECLARATION_NODE )
            processVariableDeclList(symbolTable, child);
        else if( child->nodeType == STMT_LIST_NODE )
            processStmtList(symbolTable, child);
    }

}

void checkWhileStmt(STT* symbolTable, AST_NODE* whileStmtNode){
    AST_NODE* testNode = whileStmtNode->child;
    AST_NODE* subStmtNode = testNode->rightSibling;
    AST_TYPE testType = testNode->nodeType;

    if(testType == STMT_NODE)
        checkAssignmentStmt(symbolTable, testNode);
    else
        checkExpr(symbolTable, testNode);

    checkStmt(symbolTable, subStmtNode);
}

void checkForStmt(STT* symbolTable, AST_NODE* forStmtNode){
    AST_NODE* initCondNode = forStmtNode->child;
    AST_NODE* terminateCondNode = initCondNode->rightSibling;
    AST_NODE* incrementNode = terminateCondNode->rightSibling;
    AST_NODE* subStmtNode = incrementNode->rightSibling;

    if(initCondNode){
        AST_NODE* exprNode = initCondNode->child;
        while(exprNode){
            checkExpr(symbolTable, exprNode);
            exprNode = exprNode->rightSibling;
        }
    }
    if(terminateCondNode){
        AST_NODE* exprNode = terminateCondNode->child;
        while(exprNode){
            checkExpr(symbolTable, exprNode);
            exprNode = exprNode->rightSibling;
        }
    }
    if(incrementNode){
        AST_NODE* exprNode = incrementNode->child;
        while(exprNode){
            checkExpr(symbolTable, exprNode);
            exprNode = exprNode->rightSibling;
        }
    }

    checkStmt(symbolTable, subStmtNode);
}

void checkIfStmt(STT* symbolTable, AST_NODE* ifStmtNode){
    AST_NODE* testNode = ifStmtNode->child;
    AST_NODE* subStmtNode = testNode->rightSibling;
    AST_NODE* elseStmtNode = subStmtNode->rightSibling;
    AST_TYPE testType = testNode->nodeType;

    if(testType == STMT_NODE)
        checkAssignmentStmt(symbolTable, testNode);
    else
        checkExpr(symbolTable, testNode);

    checkStmt(symbolTable, subStmtNode);

    if(elseStmtNode)
        checkStmt(symbolTable, subStmtNode);
}

void checkAssignmentStmt(STT* symbolTable, AST_NODE* assignmentNode){
    
    char *name = assignmentNode->child->semantic_value.identifierSemanticValue.identifierName;
    symbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                               
    if( !Entry )
        printErrorMissingDecl(assignmentNode, name);
    
    if( Entry->type->dimension > 0 )
        checkDimension(symbolTable, assignmentNode->child, 0); // 0 means not function parameter

    checkExpr(symbolTable, assignmentNode->child->rightSibling, 0); // 0 means not function parameter

}

void checkFuncCallStmt(STT* symbolTable, AST_NODE* expressionNode){
    char *name = expressionNode->child->semantic_value.identifierSemanticValue.identifierName;
    symbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                           
    if( !Entry )
        printErrorMissingDecl(expressionNode, name);
    
    AST_NODE* child = expressionNode->child->rightSibling->child;
    int counter = 0;
    while( child ){
        counter++;
        child = child->rightSibling;
    }

    if( Entry->numOfParameters < counter)
        printErrorTooFewArgs(expressionNode, name);

    else if( Entry->numOfParameters > counter)
        printErrorTooManyArgs(expressionNode, name);

    // scalar != array type

    ParameterNode* cursor = Entry->functionParameterList;

    AST_NODE* AST_cursor = expressionNode->child;
    while( cursor ){
        
        SymbolTableEntryKind Label = VAR_ENTRY;

        if( AST_cursor->AST_TYPE == IDENTIFIER_NODE ){
            if( AST_cursor->semantic_value.identifierSemanticValue.kind == ARRAY_ID ){
                AST_NODE* dim = AST_cursor->child;
                int counter = 0;
                while( dim ){
                    counter++;
                    dim = dim->rightSibling;
                }
                if( cursor->type->dimension != counter )
                    Label = ARRAY_ENTRY;
            }
        }

        if( cursor->type->dimension == 0 && Label == ARRAY_ENTRY )
            printErrorArrayPassToScal(AST_cursor, Entry->name, 
                                      AST_cursor->semantic_value.identifierSemanticValue.identifierName);

        else if( cursor->type->dimension > 0 && Label == VAR_ENTRY )
            printErrorScalPassToArray(AST_cursor, Entry->name, 
                                      AST_cursor->semantic_value.identifierSemanticValue.identifierName);
        

        cursor = cursor -> next;
        AST_cursor = AST_cursor -> rightSibling;
    }

    AST_NODE* AST_cursor = expressionNode->child->rightSibling->child;
    while( AST_cursor ){
        
        checkExpr(SymbolTable, AST_cursor, 1);
        AST_cursor = AST_cursor -> rightSibling;
    }
}

void checkReturnStmt(STT* symbolTable, char* funcName, AST_NODE* returnNode){
    
    // check expr valid
    checkExpr(returnNode->child);
    // get the expr type 
    symbolTableEntry *Entry = lookupSymbol(symbolTable, funcName);                                             
    
    DATA_TYPE type = getTypeOfExpr(returnNode->child);
    
    if( type == NONE_TYPE )
        printErrorInvalidExpr( returnNode->child );
    if( type != Entry->type->primitiveType )
        printWarningReturnTypeMismatch( returnNode );

}

void checkExpr(STT* symbolTable, AST_NODE* expressionNode, int isFuncPara){
    
    if( expressionNode->nodeType == IDENTIFIER_NODE ){
            
        char *name = expressionNode->semantic_value.identifierSemanticValue.identifierName;
        symbolTableEntry *Entry = lookupSymbol(symbolTable, name);
                                                   
        if( !Entry )
            printErrorMissingDecl(expressionNode, name);

        if( Entry->type->dimension > 0 ) // dimension > 0 means array
            checkDimension(symbolTable, expressionNode, isFuncPara);
    }

    else if( expressionNode->nodeType == EXPR_NODE ){
        
        // check expr valid
        if( getTypeOfExpr( expressionNode ) == NONE_TYPE )
            printErrorInvalidExpr( expressionNode );

        if( expressionNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION )
            checkExpr(symbolTable, expressionNode->child, 0);
        
        else if( expressionNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION ){
        
            checkExpr(symbolTable, expressionNode->child, 0);
            checkExpr(symbolTable, expressionNode->child->rightSibling, 0);
        }
    }
    
    else if( expressionNode->nodeType == STMT_NODE ){
        checkFuncCallStmt(symbolTable, expressionNode);
    }
}

void checkDimension(STT *symbolTable, AST_NODE* dimensionNode, int isFuncPara){
    
    char *name = dimensionNode->semantic_value.identifierSemanticValue.identifierName;
    symbolTableEntry *Entry = lookupSymbol(symbolTable, name);

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
        
        if( child->semantic_value.const1->const_type == FLOATC )
            printErrorArraySubNotInt( child );

        child = child->rightSibling;
    }
}


DATA_TYPE getTypeOfExpr(AST_NODE* exprNode){
    /*
     * NUL_NODE -> VOID_TYPE
     * CONST_NODE -> const_type: (INTC, FLOATC) -> INT_TYPE, FLOAT_TYPE
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
        
        if( exprNode->semantic_value.const1->const_type == INTC )
            return INT_TYPE;
        else if ( exprNode->semantic_value.const1->const_type == FLOATC )
            return FLOAT_TYPE;

    }
    else if( exprNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT ){
        
        char *name = exprNode->child->semantic_value.identifierSemanticValue.identifierName;
        symbolTableEntry *Entry = lookupSymbol(symbolTable, name);
        
        if( Entry->type->primitiveType == INT_TYPE )
            return INT_TYPE;
        else if( Entry->type->primitiveType == FLOAT_TYPE )
            return FLOAT_TYPE;
        if( Entry->type->primitiveType == VOID_TYPE )
            return VOID_TYPE;

    }
    else if( exprNode->nodeType == IDENTIFIER_NODE ){
       
        char *name = exprNode->child->semantic_value.identifierSemanticValue.identifierName;
        symbolTableEntry *Entry = lookupSymbol(symbolTable, name);
        
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
            
            DATA_TYPE type = getTypeOfExpr(child);

            if( type == NONE_TYPE || type == VOID_TYPE )
                return NONE_TYPE;
            else if( type == FLOAT_TYPE )
                tempType = FLOAT_TYPE;
        }

        return tempType;
    }
}
