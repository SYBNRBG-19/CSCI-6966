.intel_syntax noprefix
.section .data
    buffer: .space 256                     # Buffer to store user input

.section .bss
    bytes_read: .quad 0                    # Variable to store number of bytes read

.section .text
.global _start

_start:
    # Prepare syscall parameters for sys_read (stdin)
    mov rax, 0                            # syscall: sys_read
    mov rdi, 0                            # file descriptor: stdin
    mov rsi, buffer                       # buffer to store input
    mov rdx, 256                          # maximum bytes to read
    syscall                               # invoke syscall
    mov [bytes_read], rax                 # store number of bytes read

    # Prepare for printf call
    lea rdi, buffer                       # Load address of buffer into rdi
    call printf@plt                       # Call printf

    # Exit program
    mov rax, 60                           # syscall: sys_exit
    xor rdi, rdi                          # exit code: 0
    syscall
