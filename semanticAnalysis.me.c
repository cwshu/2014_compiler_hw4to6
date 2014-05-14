#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.me.h"
#include "semanticError.me.h"

int g_anyErrorOccur = 0;
void semanticAnalysis(AST_NODE* prog, STT* symbolTable);

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
void constExprEvaluation(AST_NODE* cexprNode);

int countRightSibling(AST_NODE* ASTNode){
    /* count the num of rightSibling node, include self */
    int counter = 0;
    while(ASTNode){
        counter += 1;
        ASTNode = ASTNode->rightSibling;
    }
    return counter;
}

void constExprEvaluation(AST_NODE* cexprNode){
    /* processing const expression
     * used in array declaration to compute size of each dimension
     * cexpr -> mcexpr -> cfactor -> (cexpr) 
     */
    /* UNFINISH */
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

    SymbolTableEntry* entry = createSymbolTableEntry(name, kind, type, NULL);
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

    SymbolTableEntry* entry = createSymbolTableEntry(name, kind, type, NULL);
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
    else{
        if(!allowTypeDef)
            return NONE_TYPE;
        /* UNFINISH */
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

void processVariableDecl(STT* symbolTable, AST_NODE* funcDeclarationNode){
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
    else if(declKind == VARIABLE_DECL){
        DATA_TYPE primitiveType = typeNameToType(symbolTable, primitiveTypeName, 0);
        if(primitiveType == NONE_TYPE){
            printErrorUndeclaredVar; /* UNFINISH */
        }

        AST_NODE* idNode = typeNode->rightSibling;
        while(idNode){
            declareScalarArrayID(symbolTable, idNode);
            idNode = idNode->rightSibling;
        }
    }
}

void processFunctionDecl(AST_NODE* declarationNode){

}

void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclarationNode(AST_NODE* declarationNode);

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);
