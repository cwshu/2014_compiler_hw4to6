int f1(){

}
int f3(){

}
int main(){
    int b;
    int f1 = 4;
    {
        int a, b; 
        float a, c; /* a redeclared */
    }

    c = 3; /* c undeclared */
    f2(); /* f2 undeclared */

    while(1){
        int b, c[10];
        c[f1] = 2; /* No error */
    }
}
int f3(){ /* f3 redeclared */
}
int f4(int b){
    int b; /* b redeclared */
    int f4 = 2; /* No error */

}
