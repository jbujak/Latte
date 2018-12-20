extern printf
extern exit
global printInt
global printString
global error

section .rodata
fmt_int    db "%d", 10, 0
fmt_string db "%s", 10, 0
rt_error   db "runtime error", 10, 0

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

error:
    push   rbp
    mov    rbp, rsp

    mov    rdi, rt_error
    call   printf wrt ..plt

    mov    rdi, 1
    call   exit wrt ..plt

    mov    rsp, rbp
    pop    rbp
    mov    rax, 0
    ret
