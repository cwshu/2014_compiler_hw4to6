#ifndef __SYMBOL_TABLE_H__
#define __SYMBOL_TABLE_H__
#include "header.h"
#define ID_MAX_LEN 256

/* struct type */
typedef struct SymbolTableTree SymbolTableTree, STT;
typedef struct SymbolTableNode SymbolTableNode;
typedef struct SymbolTableEntry SymbolTableEntry;
    typedef enum SymbolTableEntryKind SymbolTableEntryKind;
typedef struct TypeDescriptor TypeDescriptor;
typedef struct ParameterNode ParameterNode;

/* SymbolTableTree and method prototype */

/* define two phase of SymbolTableTree, build and usage phase */
#define BUILD 1
#define USE   2
struct SymbolTableTree{
    /* Complete Symbol Tables, structured in Tree. */
    SymbolTableNode* root;
    
    int currentLevel; /* depth of current inner symbolTable(scope), 0 means global */
    SymbolTableNode* currentInnerScope;
    SymbolTableNode* lastChildScope; /* the table of last closeScope */
};
/* methods */
SymbolTableTree* createSymbolTableTree();
void openScope(SymbolTableTree* pThis, int phase);
void closeScope(SymbolTableTree* pThis);
void addSymbol(SymbolTableTree* pThis, char* name);
SymbolTableEntry* lookupSymbol(SymbolTableTree* pThis, char* name);

/* SymbolTableNode and methods prototype */

/* the num of hash table entry */
#define TABLE_SIZE 256
struct SymbolTableNode{
    /* One symbol table 
     *   implement: hash table use chaining
     */
    /* left-child-right-sibling tree */
    SymbolTableNode* parent;
    SymbolTableNode* child;
    SymbolTableNode* rightSibling;

    /* Symbol Table */
    SymbolTableEntry* symbolTable[TABLE_SIZE];
};
/* methods */
SymbolTableNode* createSymbolTableNode();
void addSymbolInTable(SymbolTableNode* pThis, char* name);
SymbolTableEntry* lookupSymbolInTable(SymbolTableNode* pThis, char* name);
int hashFunction(char* str);

/* SymbolTableEntry and methods prototype */
struct SymbolTableEntry{
     char* name;
     SymbolTableEntryKind kind;
     /* var, typedef, array, function */
     TypeDescriptor* type; /* return_type in function */
     /* processing scalar + array type */
     ParameterNode* functionParameterList;
     /* Non-NULL if kind == FUNCTION */
};
/* enum */
enum SymbolTableEntryKind{
VAR_ENTRY, TYPE_ENTRY, ARRAY_ENTRY, FUNC_ENTRY
};
/* methods */
SymbolTableEntry* createSymbolTableEntry(char* name, SymbolTableEntryKind kind, 
  TypeDescriptor* type, ParameterNode* functionParameterList); 

/* TypeDescriptor and methods prototype */

/* the maximum array dimension */
#define MAX_ARRAY_DIMENSION 10;
struct TypeDescriptor{
    DATA_TYPE primitiveType;
    /* int, float */
    int dimension;
    /* dimension 0 means scalar type */
    int* sizeInEachDimension[MAX_ARRAY_DIMENSION];
};

/* ParameterNode and methods prototype */
struct ParameterNode{
    char* name;
    TypeDescriptor type;
    ParameterNode* next;
};
ParameterNode* createParameterNode(char* name, TypeDescriptor type);

#endif
