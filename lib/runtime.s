extern printf
extern scanf
extern exit
extern malloc
extern realloc
extern strlen
extern strcpy
extern strcat
extern getchar

global printInt
global printString
global error
global readInt
global readString
global main

global _latte_strcpy_to_new
global _latte_strcat_to_new

section .rodata

fmt_int    db "%lld", 10, 0
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
    mov	rdi, fmt_int
    call   scanf wrt ..plt

    mov    rax, [rbp-16]

    mov    rsp, rbp
    pop    rbp
    ret

readString:
    push   rbp
    push   r12
    push   r13
    push   r14
    mov    rbp, rsp

    mov    r13, 16
    mov    rdi, r13
    call   malloc
    mov    r12, rax
    mov    r14, 0
    jmp readString_readFirstChar

readString_readChar:
    inc    r14
readString_readFirstChar:
    call   getchar wrt ..plt
    lea    rdx, [r12+r14]
    mov    BYTE [rdx], al

    cmp    rax, -1
    je     readString_exit

    lea    rax, [r12+r14]
    cmp    BYTE [rax], 10
    je     readString_exit

    cmp    r13, r14
    jg     readString_readChar

    shl    r13, 1
    mov    rdi, r12
    mov    rsi, r13
    call   realloc
    mov    r12, rax
    jmp    readString_readChar

readString_exit:
    lea    rax, [r12+r14]
    mov    BYTE [rax], 0
    mov    rax, r12
    mov    rsp, rbp
    pop    r14
    pop    r13
    pop    r12
    pop    rbp
    ret

# rax = strcpy(rdi)
_latte_strcpy_to_new:
    push	rbp
    push    r12
    mov	    rbp, rsp
    mov     r12, rdi

    mov rdi, r12
    call strlen
    inc rax

# TODO check error
    mov rdi, rax
    call malloc

    mov rdi, rax
    mov rsi, r12
    call strcpy

    mov    rsp, rbp
    pop    r12
    pop    rbp
    ret

# rax = strcat(rdi, rsi)
_latte_strcat_to_new:
    push    rbp
    push    r12
    push    r13
    push    r14
    mov	    rbp, rsp
    mov     r12, rdi
    mov     r13, rsi

    mov     rdi, r12
    call    strlen
    mov     r14, rax

    mov     rdi, r13
    call    strlen
    add     rax, r14
    inc     rax

    mov     rdi, rax
    call    malloc

    mov     rdi, rax
    mov     rsi, r12
    call    strcpy

    mov     rdi, rax
    mov     rsi, r13
    call    strcat

    mov     rsp, rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbp
    ret
