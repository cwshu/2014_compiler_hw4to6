#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "header.h"
#include "symbolTable.h"

/* inner function prototype */
int hashFunction(char* str);

/* some useful function */
void malloc_strncpy(char** a, char* b, int len){
    int lenB = strlen(b); 
    if(lenB < len)
        len = lenB;

    *a = malloc((len+1)*sizeof(char));
    strncpy(*a, b, len);
    (*a)[len] = '\0';
}

/* SymbolTableTree method definition */
SymbolTableTree* createSymbolTableTree(){
    /* constructor of SymbolTableTree, initial global symbolTable */
    SymbolTableTree* tree = malloc(sizeof(SymbolTableTree));
    tree->root = createSymbolTableNode(0, NULL);
    tree->currentLevel = 0; // global
    tree->currentInnerScope = tree->root;
    tree->lastChildScope = NULL;
    return tree;
}

void openScope(SymbolTableTree* pThis, int phase, char* funcName){
    /* Entering new scope(new block), add new symbol table. */
    if(phase == BUILD){
        SymbolTableNode* newTable = NULL;
        if(pThis->currentLevel == 0){
            /* is global scope now, open a function scope */
            assert(funcName != NULL);
            newTable = createSymbolTableNode(1, funcName);
        }
        else{
            newTable = createSymbolTableNode(0, NULL);
        }

        if(pThis->lastChildScope == NULL)
            pThis->currentInnerScope->child = newTable;
        else
            pThis->lastChildScope->rightSibling = newTable;

        newTable->parent = pThis->currentInnerScope;
    }

    pThis->currentLevel += 1;
    if(pThis->lastChildScope == NULL){
        pThis->currentInnerScope = pThis->currentInnerScope->child;
        pThis->lastChildScope = NULL;
    }
    else{
        pThis->currentInnerScope = pThis->lastChildScope->rightSibling;
        pThis->lastChildScope = NULL;
    }

    if(pThis->currentLevel == 1){
        /* entering function, add function name */
        assert(pThis->currentInnerScope->funcName != NULL);
        pThis->currentInsideFuncName = pThis->currentInnerScope->funcName;
    }
}

void closeScope(SymbolTableTree* pThis){
    /* exit one scope, jump to previous symbol table */
    pThis->currentLevel -= 1;
    if(pThis->currentLevel == 0){
        /* leave function scope, return to global scope */
        pThis->currentInsideFuncName = NULL;
    }
    pThis->lastChildScope = pThis->currentInnerScope;
    pThis->currentInnerScope = pThis->currentInnerScope->parent;
}

void closeGlobalScope(SymbolTableTree* pThis){
    /* exit traversing AST tree one time, wait for next time traversing */
    // pThis->currentLevel = 0;
    pThis->lastChildScope = NULL;
    // pThis->currentInnerScope = pThis->root;
}

void addSymbolByEntry(SymbolTableTree* pThis, SymbolTableEntry* entry){
    addSymbolInTableByEntry(pThis->currentInnerScope, entry);
}

char* insideFuncName(SymbolTableTree* pThis){
    return pThis->currentInsideFuncName;
}

SymbolTableEntry* lookupSymbol(SymbolTableTree* pThis, char* name){
    SymbolTableNode* scope = pThis->currentInnerScope;
    SymbolTableEntry* entry = NULL;
    while(scope){
        entry = lookupSymbolInTable(scope, name);
        if(entry)
            return entry;
        scope = scope->parent;
    }
    return NULL;
}

SymbolTableEntry* lookupSymbolWithLevel(SymbolTableTree* pThis, char* name, int* pLevel){
    SymbolTableNode* scope = pThis->currentInnerScope;
    SymbolTableEntry* entry = NULL;
    *pLevel = pThis->currentLevel;
    while(scope){
        entry = lookupSymbolInTable(scope, name);
        if(entry)
            return entry;
        scope = scope->parent;
        *pLevel -= 1;
    }
    return NULL;
}

SymbolTableEntry* lookupSymbolCurrentScope(SymbolTableTree* pThis, char* name){
    return lookupSymbolInTable(pThis->currentInnerScope, name);
}

/* SymbolTableNode method definition */
SymbolTableNode* createSymbolTableNode(int isFuncScope, char* funcName){
    /* constructor of SymbolTable */
    SymbolTableNode* symTable = malloc(sizeof(SymbolTableNode));
    symTable->parent = NULL;
    symTable->child = NULL;
    symTable->rightSibling = NULL;
    int i;
    for(i=0; i<TABLE_SIZE; i++){
        symTable->symbolTable[i] = NULL;
    }
    symTable->isFuncScope = isFuncScope;
    symTable->funcName = funcName;
}

void addSymbolInTableByEntry(SymbolTableNode* pThis, SymbolTableEntry* entry){
    char* name = entry->name;
    int term = hashFunction(name);

    if(pThis->symbolTable[term] == NULL){
        pThis->symbolTable[term] = entry;
    }
    else{
        entry->next = pThis->symbolTable[term];
        pThis->symbolTable[term] = entry;
    }
}

SymbolTableEntry* lookupSymbolInTable(SymbolTableNode* pThis, char* name){
    int term = hashFunction(name);
    SymbolTableEntry* entry = pThis->symbolTable[term];
    while(entry){
        if(strncmp(entry->name, name, ID_MAX_LEN) == 0)
            return entry;
        entry = entry->next;
    }
    return NULL;
}

int hashFunction(char* str){
    int idx = 0;
    while(*str){
        idx = idx << 1;
        idx += *str;
        str++;
    }    
    return (idx & (TABLE_SIZE-1));
}

/* SymbolTableEntry method definition */
SymbolTableEntry* createSymbolTableEntry(char* name, SymbolTableEntryKind kind, 
  TypeDescriptor* type, int numOfPara, ParameterNode* functionParameterList){
    /* constructor of SymbolTableEntry */
    SymbolTableEntry* entry = malloc(sizeof(SymbolTableEntry));
    malloc_strncpy(&(entry->name), name, ID_MAX_LEN); 
    entry->kind = kind;
    entry->type = type;
    entry->numOfParameters = numOfPara;
    entry->functionParameterList = functionParameterList;
    entry->next = NULL;
}

/* TypeDescriptor method definition */
TypeDescriptor* createScalarTypeDescriptor(DATA_TYPE primitiveType){
    /* constructor of (scalar, typedef) SymbolTableEntry */
    TypeDescriptor* pThis = malloc(sizeof(TypeDescriptor));
    pThis->primitiveType = primitiveType;
    pThis->dimension = 0;
    return pThis;
}

TypeDescriptor* createArrayTypeDescriptor(DATA_TYPE primitiveType, 
  int dimension, int* sizes){
    /* constructor of (array) SymbolTableEntry */
    TypeDescriptor* pThis = malloc(sizeof(TypeDescriptor));
    pThis->primitiveType = primitiveType;
    pThis->dimension = dimension;
    int i;
    for(i = 0; i < dimension; i++){
        pThis->sizeOfEachDimension[i] = sizes[i];
    }
    return pThis;
}

TypeDescriptor* copyTypeDescriptor(TypeDescriptor* pThis){
    /* copy constructor */
    TypeDescriptor* newType = malloc(sizeof(TypeDescriptor));
    newType->primitiveType = pThis->primitiveType;
    newType->dimension = pThis->dimension;
    int i;
    for(i=0; i<pThis->dimension; i++)
        newType->sizeOfEachDimension[i] = pThis->sizeOfEachDimension[i];

    return newType;
}

/* ParameterNode method definition */
ParameterNode* createParameterNode(TypeDescriptor* type, char* name){
    /* constructor of ParameterNode */
    ParameterNode* pThis = malloc(sizeof(ParameterNode));
    malloc_strncpy(&(pThis->name), name, ID_MAX_LEN);
    pThis->type = type;
    pThis->next = NULL;
}

ParameterNode* prependList(ParameterNode* head, ParameterNode* list){
    head->next = list;
    return head;
}

ParameterNode* createParameterList(int num, TypeDescriptor* parametersType[], char* nameOfParas[]){
    /* TypeDescriptor pointer array, every pointer points to one type*/
    ParameterNode* list = NULL;
    int i;
    for(i=num-1; i>=0; i--){
        ParameterNode* newNode = createParameterNode(parametersType[i], nameOfParas[i]);
        list = prependList(newNode, list);
    }
    return list;
}

