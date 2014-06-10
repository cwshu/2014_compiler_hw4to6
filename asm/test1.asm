.data
m1:	.asciiz "enter a number"
.text
main:
li $v0, 4    # syscall 4 means a call to print_str; 
la $a0, m1	# the address of the string is passed # by register $a0 (i.e. register $r4)
syscall

li $v0, 10
syscall