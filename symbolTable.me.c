#include <string.h>
#include "header.h"
#include "symbolTable.me.h"

/* some useful function */
void malloc_strncpy(char* a, char* b, int len){
    int lenB = strlen(b); 
    if(lenB < len)
        len = lenB;

    a = malloc(len*sizeof(char));
    strncpy(a, b, len);
}

/* SymbolTableTree method definition */
SymbolTableTree* createSymbolTableTree(){
/* constructor of SymbolTableTree, initial global symbolTable  */
    SymbolTableTree* tree = malloc(sizeof(SymbolTableTree));
    tree->root = createSymbolTable();
    tree->currentLevel = 0; // global
    tree->currentInnerScope = root;
    tree->lastChildScope = NULL;
    return tree;
}

/* SymbolTableNode method definition */
SymbolTableNode* createSymbolTableNode(){
/* constructor of SymbolTable */
    SymbolTableNode* symTable = malloc(sizeof(SymbolTableNode));
    symTable->parent = NULL;
    symTable->child = NULL;
    symTable->rightSibling = NULL;
    int i;
    for(i=0; i<TABLE_SIZE; i++){
        symTable->symbolTable[i] = NULL;
    }
}

/* SymbolTableEntry method definition */
SymbolTableEntry* createSymbolTableEntry(char* name, SymbolTableEntryKind kind, 
  TypeDescriptor* type, ParameterNode* functionParameterList){
/* constructor of SymbolTableEntry */
    SymbolTableEntry* entry = malloc(sizeof(SymbolTableEntry));
    malloc_strncpy(entry->name, name, ID_MAX_LEN); 
    entry->kind = kind;
    entry->type = type;
    entry->functionParameterList = functionParameterList;
}

int hashFunction(char* str){
    int idx=0;
    while(*str){
        idx = idx << 1;
        idx+=*str;
        str++;
    }    
    return (idx & (TABLE_SIZE-1));
}
