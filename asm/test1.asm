.data
m1:	.asciiz "enter a number"
m3:	.asciiz "enter a number3"
.text
f1:
    sw $ra, 0($sp)
    sw $fp, -4($sp)
    add $fp, $sp, -4
    add $sp, $fp, -4
    lw  $2, _framesize_main
    sub $sp, $sp, $2
_begin_f1:

    li $v0, 4   # syscall 4 means a call to print_str; 
    la $a0, m3	# the address of the string is passed # by register $a0 (i.e. register $r4)
    syscall

_end_f1:
    lw $ra, 4($fp)
    add $sp, $fp, 4
    lw $fp, 0($fp)
    jr $ra
.data
    _framesize_f1: .word 0

.text
main:
    sw $ra, 0($sp)
    sw $fp, -4($sp)
    add $fp, $sp, -4
    add $sp, $fp, -4
    lw  $2, _framesize_main
    sub $sp, $sp, $2
    # Saved register
    sw  $s0, 36($sp)
    sw  $s1, 32($sp)
    sw  $s2, 28($sp)
    sw  $s3, 24($sp)
    sw  $s4, 20($sp)
    sw  $s5, 16($sp)
    sw  $s6, 12($sp)
    sw  $s7, 8($sp)
    sw  $gp, 4($sp)
_begin_main:

    li $v0, 4   # syscall 4 means a call to print_str; 
    la $a0, m1	# the address of the string is passed # by register $a0 (i.e. register $r4)
    syscall

    jal f1

    li $v0, 4   # syscall 4 means a call to print_str; 
    la $a0, m1	# the address of the string is passed # by register $a0 (i.e. register $r4)
    syscall

_end_main:
    # Load Saved register
    lw  $s0, 36($sp)
    lw  $s1, 32($sp)
    lw  $s2, 28($sp)
    lw  $s3, 24($sp)
    lw  $s4, 20($sp)
    lw  $s5, 16($sp)
    lw  $s6, 12($sp)
    lw  $s7, 8($sp)
    lw  $gp, 4($sp)

    lw  $ra, 4($fp)
    add $sp, $fp, 4
    lw  $fp, 0($fp)
    jr  $ra
.data
    _framesize_main: .word 36
