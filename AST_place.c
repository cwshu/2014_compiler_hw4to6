#include "header.h"
void setPlaceOfASTNodeToReg(AST_NODE *pThis, DATA_TYPE primiType, int regNum){
    /* let AST_NODE's place pointed to register */
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = REG_TYPE;
    pThis->valPlace.place.regNum = regNum;
}

void setPlaceOfASTNodeToStack(AST_NODE *pThis, DATA_TYPE primiType, int stackOffset){
    /* let AST_NODE's place pointed to stack */
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = STACK_TYPE;
    pThis->valPlace.place.stackOffset = stackOffset;
}

void setPlaceOfASTNodeToGlobalData(AST_NODE *pThis, DATA_TYPE primiType, char* label, int offset){
    /* let AST_NODE's place pointed to global memory address */
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = GLOBAL_TYPE;
    pThis->valPlace.place.data.label = label;
    pThis->valPlace.place.data.offset = offset;
}
