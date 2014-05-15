int main(){
    int a[10], b = 1, c = 3;
    float d = 2.5;
    int e[3.5];

    a[1.5] = 2;
    a[b + d] = 3;
    a[b + 3.4] = 4;
    a[1.5 + d] = 5;
    a[1.5 + 3.5] = 6;
    a[5 + 6 + b] = 2.5;

    return 0;
}
