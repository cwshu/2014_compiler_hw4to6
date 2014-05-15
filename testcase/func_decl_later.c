int main()
{
    echo();
    return 0;
}

void echo()
{
   int one_int;
   float one_float;

   one_int = read();
   one_float = fread();

   write("You input: ");
   write(one_int);
   write(", ");
   write(one_float);
}
