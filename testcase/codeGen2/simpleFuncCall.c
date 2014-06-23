int f(int a, int Array[]){
    write("In function f...\n");
}

int main (){
    int A[100];
    write("function call...\n");
    f(1,A);
    write("back to main...\n");

    /*
    function call...
    In function f...
    back to main...
    */
}
