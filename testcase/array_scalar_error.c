int f1(int a, int b[][10]){
}
int f2(int a[], int b){
}

int main(){
    int x, y[15], z[15][10];

    f1(x, x);
    f1(y, z);
    f2(x, x);
    f2(y, y);
}
