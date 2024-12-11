.section .data
num1: .long 5         # Initialize num1 with value 5
num2: .long 6         # Initialize num2 with value 6

.section .text
.global _start

_start:
    movl num1, %eax     # Load num1 into %eax
    addl num2, %eax     # Add num2 to %eax
    movl %eax, %ebx     # Store sum in %ebx
    cmpl $10, %ebx      # Compare sum with 10
    jle end_main        # If sum <= 10, jump to end_main

    # Inline reduce functionality
    subl $5, %ebx       # Subtract 5 from %ebx

end_main:
    movl %eax, %ebx     # Store the final sum in %ebx
    movl $1, %eax       # System call number for exit
    int $0x80           # Trigger system call
