#!/usr/bin/env python2
import re, sys

testcase = """
%%
/* ==== Grammar Section ==== */
/* XDD
*/
// xd
program		: global_decl_list { $$=Allocate(PROGRAM_NODE);  makeChild($$,$1); prog=$$;}
		| { $$=Allocate(PROGRAM_NODE); prog=$$;}
		;

global_decl_list: global_decl_list global_decl 
                    {
                        $$ = makeSibling($1, $2);
                    }	
                | global_decl
                    {
                        $$ = $1;
                    }
                ;
%%
"""

testcase2 = """
%%

/* ==== Grammar Section ==== */

/* Productions */               /* Semantic actions */
program		: global_decl_list { $$=Allocate(PROGRAM_NODE);  makeChild($$,$1); prog=$$;}
		| { $$=Allocate(PROGRAM_NODE); prog=$$;}
		;

global_decl_list: global_decl_list global_decl 
                    {
                        $$ = makeSibling($1, $2);
                    }	
                | global_decl
                    {
                        $$ = $1;
                    }
                ; 

global_decl	: decl_list function_decl
                {
                    $$ = makeSibling(makeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1), $2);
                }
            | function_decl 
                {
                    $$ = $1;
                }
            ;

function_decl	: type ID MK_LPAREN param_list MK_RPAREN MK_LBRACE block MK_RBRACE     
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* parameterList = Allocate(PARAM_LIST_NODE);
                        makeChild(parameterList, $4);
                        makeFamily($$, 4, $1, makeIDNode($2, NORMAL_ID), parameterList, $7);
                    }
                | VOID ID MK_LPAREN param_list MK_RPAREN MK_LBRACE block MK_RBRACE      
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* voidNode = makeIDNode("void", NORMAL_ID);
                        AST_NODE* parameterList = Allocate(PARAM_LIST_NODE);
                        makeChild(parameterList, $4);
                        makeFamily($$, 4, voidNode, makeIDNode($2, NORMAL_ID), parameterList, $7);
                    }
                | ID ID MK_LPAREN param_list MK_RPAREN MK_LBRACE block MK_RBRACE      
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* idNode = makeIDNode($1, NORMAL_ID);
                        AST_NODE* parameterList = Allocate(PARAM_LIST_NODE);
                        makeChild(parameterList, $4);
                        makeFamily($$, 4, idNode, makeIDNode($2, NORMAL_ID), parameterList, $7);
                    }
                | type ID MK_LPAREN  MK_RPAREN MK_LBRACE block MK_RBRACE 
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* emptyParameterList = Allocate(PARAM_LIST_NODE);
                        makeFamily($$, 4, $1, makeIDNode($2, NORMAL_ID), emptyParameterList, $6);
                    }
                | VOID ID MK_LPAREN  MK_RPAREN MK_LBRACE block MK_RBRACE 
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* voidNode = makeIDNode("void", NORMAL_ID);
                        AST_NODE* emptyParameterList = Allocate(PARAM_LIST_NODE);
                        makeFamily($$, 4, voidNode, makeIDNode($2, NORMAL_ID), emptyParameterList, $6);
                    }
                | ID ID MK_LPAREN  MK_RPAREN MK_LBRACE block MK_RBRACE 
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* idNode = makeIDNode($1, NORMAL_ID);
                        AST_NODE* emptyParameterList = Allocate(PARAM_LIST_NODE);
                        makeFamily($$, 4, idNode, makeIDNode($2, NORMAL_ID), emptyParameterList, $6);
                    }
                ;

param_list	: param_list MK_COMMA  param 
                {
                    $$ = makeSibling($1, $3);
                }
            | param	
                {
                    $$ = $1; 
                }
            ;
"""

def parsing(code):
    # parsing yacc file and return grammer

    # choose text in between two %% symbol
    start_idx = code.find("%%");
    final_idx = code[start_idx+2:].find("%%");
    code = code[start_idx+2:final_idx]
    # clear comment /* */ //$
    code = re.sub(r"//.*", "", code)
    code = re.sub(r"/\*([^*]|\*[^/]*[^/*])*((\*)+)?\*/", "", code)
    # clear action
    code = re.sub(r"{[^}]*}", "", code)

    code_list = [i.rstrip() for i in code.splitlines()]
    code_list = [i for i in code_list if i]
    code = "\n".join(code_list)
    code = re.sub(r";", "", code)
    return code

if __name__ == '__main__':
    yacc_code = open(sys.argv[1], "r").read()
    grammer = parsing(yacc_code)
    out_file = open(sys.argv[2], "w")
    out_file.write(grammer)
