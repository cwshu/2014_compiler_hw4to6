int main (){
    
    typedef int INT_ARRAY[4];
    INT_ARRAY matrix[8];
    int i;
    for( i=0 ; i<8 ; i = i + 1 ){
	for( j=0 ; j<4 ; j = j + 1 ){
            matrix[i][j] = i * 4 + j;
        }
    }

    for( i=0 ; i<8 ; i = i + 1 ){
	for( j=0 ; j<4 ; j = j + 1 ){
            write(matrix[i][j]);
            write(" ");
        }
        write("\n");
    }


    /*
    0 1 2 3
    4 5 6 7
    8 9 10 11
    12 13 14 15
    16 17 18 19
    20 21 22 23
    24 25 26 27
    28 29 30 31
    */
}
