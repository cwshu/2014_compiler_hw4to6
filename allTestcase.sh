#!/bin/sh

mkdir testcase_result/
mkdir testcase_result/hw5_codeGenTA/
mkdir testcase_result/hw5_after_demo/
mkdir testcase_result/KHLtest/
mkdir testcase_result/codeGen2/
mkdir testcase_result/hw6_codeGenTA/

# folder hw5_codeGenTA
./parser testcase/hw5_codeGenTA/assign.c
rm -f assign.s
mv output.s testcase_result/hw5_codeGenTA/assign.s

./parser testcase/hw5_codeGenTA/control.c
rm -f control.s
mv output.s testcase_result/hw5_codeGenTA/control.s

./parser testcase/hw5_codeGenTA/expr.c
rm -f expr.s
mv output.s testcase_result/hw5_codeGenTA/expr.s

./parser testcase/hw5_codeGenTA/func.c
rm -f func.s
mv output.s testcase_result/hw5_codeGenTA/func.s

./parser testcase/hw5_codeGenTA/io.c
rm -f io.s
mv output.s testcase_result/hw5_codeGenTA/io.s

# folder hw5_after_demo
./parser testcase/hw5_after_demo/expr.c
rm -f expr.s
mv output.s testcase_result/hw5_after_demo/expr.s

./parser testcase/hw5_after_demo/if.c
rm -f if.s
mv output.s testcase_result/hw5_after_demo/if.s

./parser testcase/hw5_after_demo/non_local.c
rm -f non_local.s
mv output.s testcase_result/hw5_after_demo/non_local.s

./parser testcase/hw5_after_demo/while.c
rm -f while.s
mv output.s testcase_result/hw5_after_demo/while.s

# folder KHLtest
./parser testcase/KHLtest/funcGlobalAccess.c
rm -f funcGlobalAccess.s
mv output.s testcase_result/KHLtest/funcGlobalAccess.s

./parser testcase/KHLtest/globalIfelse.c
rm -f globalIfelse.s
mv output.s testcase_result/KHLtest/globalIfelse.s

./parser testcase/KHLtest/globalLocal.c
rm -f globalLocal.s
mv output.s testcase_result/KHLtest/globalLocal.s

./parser testcase/KHLtest/whileFunCall.c
rm -f whileFunCall.s
mv output.s testcase_result/KHLtest/whileFunCall.s

# folder codeGen2
./parser testcase/codeGen2/factorial.c
rm -f factorial.s
mv output.s testcase_result/codeGen2/factorial.s

./parser testcase/codeGen2/multiStmtFor.c
rm -f multiStmtFor.s
mv output.s testcase_result/codeGen2/multiStmtFor.s

./parser testcase/codeGen2/nestedIf.c
rm -f nestedIf.s
mv output.s testcase_result/codeGen2/nestedIf.s

./parser testcase/codeGen2/nestedWhile.c
rm -f nestedWhile.s
mv output.s testcase_result/codeGen2/nestedWhile.s

./parser testcase/codeGen2/shortCircuit.c
rm -f shortCircuit.s
mv output.s testcase_result/codeGen2/shortCircuit.s

./parser testcase/codeGen2/simpleFor.c
rm -f simpleFor.s
mv output.s testcase_result/codeGen2/simpleFor.s

# ./parser testcase/codeGen2/simpleFuncCall.c
# rm -f simpleFuncCall.s
# mv output.s testcase_result/codeGen2/simpleFuncCall.s
# 
# ./parser testcase/codeGen2/typeDef.c
# rm -f typeDef.s
# mv output.s testcase_result/codeGen2/typeDef.s

# folder hw6_codeGenTA
./parser testcase/hw6_codeGenTA/complex.c
rm -f complex.s
mv output.s testcase_result/hw6_codeGenTA/complex.s

./parser testcase/hw6_codeGenTA/implicit.c
rm -f implicit.s
mv output.s testcase_result/hw6_codeGenTA/implicit.s

./parser testcase/hw6_codeGenTA/short.c
rm -f short.s
mv output.s testcase_result/hw6_codeGenTA/short.s
