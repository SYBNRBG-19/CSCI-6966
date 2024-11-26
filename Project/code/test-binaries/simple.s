.section .data
message:
    .ascii "Hello, World!\n"
len = . - message

.section .text
.global _start
_start:
    # write(1, message, len)
    mov $1, %rax            # syscall number for write (1)
    mov $1, %rdi            # file descriptor 1 (stdout)
    lea message(%rip), %rsi # pointer to the message
    mov $len, %rdx          # message length
    syscall                 # make the system call

    # exit(0)
    mov $60, %rax           # syscall number for exit (60)
    xor %rdi, %rdi          # exit code 0
    syscall                 # make the system call
