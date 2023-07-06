    SYS_READ   equ     0          ; read text from stdin
    SYS_WRITE  equ     1          ; write text to stdout
    SYS_EXIT   equ     60         ; terminate the program
    STDIN      equ     0          ; standard input
    STDOUT     equ     1          ; standard output
; --------------------------------
section .bss
    uinput_len equ     24         ; 24 bytes for user input
    uinput     resb    uinput_len ; buffer for user input
; --------------------------------
section .data
    prompt     db      "Please input some text: "
    prompt_len equ     $ - prompt
    text       db      10, "You wrote: "
    text_len   equ     $ - text
; --------------------------------
section .text
    global _start

_start:
    mov     rdx, prompt_len      ; Write `prompt_len` bytes
    mov     rsi, prompt          ; Write bytes from `prompt`
    mov     rdi, STDOUT          ; Write bytes to `STDOUT`
    mov     rax, SYS_WRITE       ; Specify `write` syscall
    syscall                      ; Run syscall

    mov     rdx, uinput_len      ; Read `uinput_len` bytes
    mov     rsi, uinput          ; Read bytes into `uinput`
    mov     rdi, STDIN           ; Read bytes from `STDIN`
    mov     rax, SYS_READ        ; Specify `read` syscall
    syscall                      ; Run syscall
    push    rax                  ; Push the value in `rax` (the number of bytes read) onto the stack

    mov     rdx, text_len        ; Write `text_len` bytes
    mov     rsi, text            ; Write bytes from `text`
    mov     rdi, STDOUT          ; Write bytes to `STDOUT`
    mov     rax, SYS_WRITE       ; Specify `write` syscall
    syscall                      ; Run syscall

    pop     rdx                  ; Pop a value off the stack into `rdx` (so we write ths number of value we previously read)
    mov     rsi, uinput          ; Write bytes from `uinput`
    mov     rdi, STDOUT          ; Write bytes to `STDOUT`
    mov     rax, SYS_WRITE       ; Specify `write` syscall
    syscall                      ; Run syscall

    ; xor     edi, edi             ;
    mov     rax, SYS_EXIT        ; Specify `exit` syscall
    syscall                      ; Run syscall