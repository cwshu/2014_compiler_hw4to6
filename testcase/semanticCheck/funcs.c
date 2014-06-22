
/* redefine library function */

int test(float b)
{
    int p;
    /* not a function */
	return 1;
}

int ret()
{
	int i;
	test(i);
	
	return 1;
}


/* no main function */
