#ifndef __AST_PLACE_H__
#define __AST_PLACE_H__

typedef enum ExpValPlaceKind{
    NULL_TYPE,
    REG_TYPE,
    STACK_TYPE,
    LABEL_TYPE 
};

struct ExpValPlace{
    DATA_TYPE dataType;
    ExpValPlaceKind kind;
    union {
        int regNum;
        int stackOffset;
        char* label;
    } place;
};

void setPlaceOfASTNodeToReg(AST_NODE *pThis, DATA_TYPE primiType, int regNum);
void setPlaceOfASTNodeToStack(AST_NODE *pThis, DATA_TYPE primiType, int stackOffset);
void setPlaceOfASTNodeToLabel(AST_NODE *pThis, DATA_TYPE primiType, char* label);

#endif
