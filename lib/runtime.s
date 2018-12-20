extern printf
extern scanf
extern exit

global printInt
global printString
global error
global readInt

section .rodata
fmt_int    db "%lld", 10, 0
fmt_int_sc db "%lld", 0
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

readInt:
    push	rbp
    mov	rbp, rsp
    sub	rsp, 16

    lea	rax, [rbp-16]
    mov	rsi, rax
    mov	rdi, fmt_int_sc
    call   scanf wrt ..plt
    mov    rax, [rbp-16]

    mov    rsp, rbp
    pop    rbp
    ret
