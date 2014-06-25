int main (){
    int a = 0,b = 1;
    int result;

    result = (a&&b) || (a||b);
    write(result);
    write("\n");

    result = b || b && a;
    write(result);
    write("\n");

    /*
    1
    1
    */
}
