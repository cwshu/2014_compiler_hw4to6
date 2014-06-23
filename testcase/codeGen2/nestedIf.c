int main (){
    
    int a = 0;
    
    if(1){
        a = 1;
        write(a);
        write("\n");
        if(1){
            a = 2;
            write(a);
            write("\n");
            if(1){
                a = 3;
                write(a);
                write("\n");
            }
        }
        else{
            a = 689;
            write(a);
            write("\n");
        }
    }
    
    if( a = 0 ){
        write(a);
        write("\nIn If...\n");
    }
    else{
        write(a);
        write("\nIn else...\n");
        if(a = 1){
            write("In nested if...\n");
        }
        else{
            write("In nested else...\n");
        }
    }


    /* 
    1
    2
    3
    0
    In else...
    In nested if...
    */
}
