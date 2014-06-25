int main (){

    int n,i = 0,j = 0;
    n = read();
    while( i < n / 2 ){

        j = 0;
        while( j < ( n - ( 2 * i + 1 ) ) / 2 ){
            write(" ");
            j = j + 1;
        }
        j = 0;
        while( j < ( 2 * i + 1 ) ){
            write("*");
            j = j + 1;
        }
        j = 0;
        while( j < ( n - ( 2 * i + 1 ) ) / 2 ){
            write(" ");
            j = j + 1;
        }

        write("\n");
        i = i + 1;
    }

    i=0;
    while( i < n ){
        write("*");
        i = i + 1;
    }
    write("\n");

    i = 0;
    while( i < n / 2 ){

        j = 0;
        while( j < i + 1 ){
            write(" ");
            j = j + 1;
        }
        j = 0;
        while( j < n - 2 * ( i + 1 ) ){
            write("*");
            j = j + 1;
        }
        j = 0;
        while( j < i + 1 ){
            write(" ");
            j = j + 1;
        }

        write("\n");
        i = i + 1;
    }

    /*
    odd number diamond
    */
}
