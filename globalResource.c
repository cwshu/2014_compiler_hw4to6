#include "header.h"

void GRinit(struct GlobalResource* GR){
    GR->labelCounter = 1;
    GR->stackTop = 36;
    RMinit(&(GR.regManager), MAX_REG_NUM, FIRST_RM_REG_NUM);
    RMinit(&(GR.FPregManager), MAX_FP_REG_NUM, FIRST_RM_FP_REG_NUM);
    
    GR->constStrings = malloc(sizeof(ConstStringSet));
    initConstStringSet(GR->constStrings);
}

void GRfin(struct GlobalResource* GR){
    free(GR->constStrings);
}
