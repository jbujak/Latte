extern printf
global printInt
global printString

section .rodata
fmt_int    db "%d", 10, 0
fmt_string db "%s", 10, 0

section .text

printInt:
    push   rbp
    mov    rbp, rsp

    mov    rsi, rdi
    mov    rdi, fmt_int
    call   printf wrt ..plt

    mov    rsp, rbp
    pop    rbp
    mov    rax, 0
    ret

printString:
    push   rbp
    mov    rbp, rsp

    mov    rsi, rdi
    mov    rdi, fmt_string
    call   printf wrt ..plt

    mov    rsp, rbp
    pop    rbp
    mov    rax, 0
    ret
