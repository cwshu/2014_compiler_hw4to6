TARGET = parser
OBJECT = parser.tab.c parser.tab.o lex.yy.c alloc.o functions.o semanticAnalysis.o symbolTable.o
OUTPUT = parser.output parser.tab.h
CC = gcc -g
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS = -lfl 

parser: parser.tab.o alloc.o functions.o symbolTable.o semanticAnalysis.o semanticError.o
	$(CC) -o $(TARGET) parser.tab.o alloc.o functions.o symbolTable.o semanticAnalysis.o semanticError.o $(LIBS)

parser.tab.o: parser.tab.c lex.yy.c alloc.o functions.c symbolTable.o semanticAnalysis.o
	$(CC) -c parser.tab.c
    
lex.yy.c: lexer3.l
	$(LEX) lexer3.l

parser.tab.c: parser.y 
	$(YACC) $(YACCFLAG) parser.y

%.o: %.c
	$(CC) -c $< -o $@

clean:
	rm -f $(TARGET) $(OBJECT) $(OUTPUT) *.o

