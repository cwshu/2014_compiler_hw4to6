#ifndef __SYMBOL_TABLE_H__
#define __SYMBOL_TABLE_H__

#include "header.h"
#define ID_MAX_LEN 256

/* struct type */
// typedef struct SymbolTableTree SymbolTableTree, STT;
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
    char* currentInsideFuncName;
};
/* methods */
SymbolTableTree* createSymbolTableTree();
void openScope(SymbolTableTree* pThis, int phase, char* funcName);
void closeScope(SymbolTableTree* pThis);
void addSymbolByEntry(SymbolTableTree* pThis, SymbolTableEntry* entry);
char* insideFuncName(SymbolTableTree* pThis);
SymbolTableEntry* lookupSymbol(SymbolTableTree* pThis, char* name);
SymbolTableEntry* lookupSymbolWithLevel(SymbolTableTree* pThis, char* name, int* pLevel);
SymbolTableEntry* lookupSymbolCurrentScope(SymbolTableTree* pThis, char* name);
    /* NULL if name doesn't exist 
     * else return Entry
     */


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
    int isFuncScope;
    char* funcName;
    SymbolTableEntry* symbolTable[TABLE_SIZE];
};
/* methods */
SymbolTableNode* createSymbolTableNode(int isFuncScope, char* funcName);
void addSymbolInTableByEntry(SymbolTableNode* pThis, SymbolTableEntry* entry);
SymbolTableEntry* lookupSymbolInTable(SymbolTableNode* pThis, char* name);
// int hashFunction(char* str);

/* SymbolTableEntry and methods prototype */
/* enum */
enum SymbolTableEntryKind{
VAR_ENTRY, TYPE_ENTRY, ARRAY_ENTRY, FUNC_ENTRY
};
struct SymbolTableEntry{
    char* name;
    SymbolTableEntryKind kind; /* var, typedef, array, function */
    TypeDescriptor* type; /* return_type in function */

    /* processing scalar + array type */
    int numOfParameters;
    ParameterNode* functionParameterList; /* Non-NULL if kind == FUNCTION */

    /* variable address */
    int stackOffset;

    /* Linked-List in Hash table */
    SymbolTableEntry* next;
};

/* methods */
SymbolTableEntry* createSymbolTableEntry(char* name, SymbolTableEntryKind kind, 
  TypeDescriptor* type, int numOfPara, ParameterNode* functionParameterList); 
// void addOffset(SymbolTableEntry entry, int offset);

/* TypeDescriptor and methods prototype */

/* the maximum array dimension */
struct TypeDescriptor{
    DATA_TYPE primitiveType;
    /* int, float */
    int dimension;
    /* dimension 0 means scalar type */
    int sizeOfEachDimension[MAX_ARRAY_DIMENSION];
};
TypeDescriptor* createScalarTypeDescriptor(DATA_TYPE primitiveType);
TypeDescriptor* createArrayTypeDescriptor(DATA_TYPE primitiveType, int dimension, int* sizes);
TypeDescriptor* copyTypeDescriptor(TypeDescriptor* pThis);

/* ParameterNode and methods prototype */
struct ParameterNode{
    char* name;
    TypeDescriptor* type;
    ParameterNode* next;
};
ParameterNode* createParameterNode(TypeDescriptor* type, char* name);
ParameterNode* prependList(ParameterNode* head, ParameterNode* list);
ParameterNode* createParameterList(int num, TypeDescriptor* parametersType[], char* nameOfParas[]);

#endif
