#include "header.h"
void setPlaceOfASTNodeToReg(AST_NODE *pThis, DATA_TYPE primiType, int regNum){
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = REG_TYPE;
    pThis->valPlace.place.regNum = regNum;
}

void setPlaceOfASTNodeToStack(AST_NODE *pThis, DATA_TYPE primiType, int stackOffset){
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = STACK_TYPE;
    pThis->valPlace.place.stackOffset = stackOffset;
}

void setPlaceOfASTNodeToLabel(AST_NODE *pThis, DATA_TYPE primiType, char* label){
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = LABEL_TYPE;
    pThis->valPlace.place.label = label;
}
