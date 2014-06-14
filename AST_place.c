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

void setPlaceOfASTNodeToGlobalData(AST_NODE *pThis, DATA_TYPE primiType, char* label, int offset){
    pThis->valPlace.dataType = primiType;
    pThis->valPlace.kind = GLOBAL_TYPE;
    pThis->valPlace.place.data.label = label;
    pThis->valPlace.place.data.offset = offset;
}
