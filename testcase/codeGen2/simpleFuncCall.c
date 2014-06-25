int f(int a, int Array[]){
    write("In function f...\n");
    write(Array[2] );
    write("\n");
    write(Array[13]);
    write("\n");
    write(Array[24]);
    write("\n");
    write(Array[35]);
    write("\n");
    write(Array[46]);
    write("\n");
    write(Array[57]);
    write("\n");
    write(Array[68]);
    write("\n");
    write(Array[79]);
    write("\n");
}

int main (){
    int A[100];
    A[2] = 3;
    A[13] = 4;
    A[24] = 5;
    A[35] = 6;
    A[46] = 7;
    A[57] = 8;
    A[68] = 9;
    A[79] = 10;

    write("function call...\n");
    f(1,A);
    write("back to main...\n");

    /*
    function call...
    In function f...
    back to main...
    */
}
