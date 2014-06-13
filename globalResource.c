#include "header.h"

void GRinit(struct GlobalResource* GR){
    GR->labelCounter = 1;
    GR->stackTop = 36;

    GR.regManager = malloc(sizeof(regManager));
    RMinit(GR.regManager, MAX_REG_NUM, FIRST_RM_REG_NUM);
    GR.FPRegManager = malloc(sizeof(regManager));
    RMinit(GR.FPregManager, MAX_FP_REG_NUM, FIRST_RM_FP_REG_NUM);
    
    GR->constStrings = malloc(sizeof(ConstStringSet));
    initConstStringSet(GR->constStrings);
}

void GRfin(struct GlobalResource* GR){
    free(GR->constStrings);
    free(GR->regManager);
    free(GR->FPRegManager);
}
