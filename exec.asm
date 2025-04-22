; ***************************************************************
; *    ПОДПРОГРАММА ЗАПУСКА ДОЧЕРНЕГО ПРОЦЕССА                  *
; *                                                             *
; ***************************************************************

.model SMALL

public exec
code segment PARA PUBLIC 'code'
assume cs:code

start proc

start endp

; Вход: 
; [bp+6] - addr child process name
; [bp+8] - addr param block for child process
; [bp+10] - seg where data
; Выход: 
;       - при ошибке устанавливается флаг С
;       - Для анализа кода возврата надо проанализировать al
;       - вызвать 4Dh сразу после возврата из дочернего процесса

exec proc far
    push bp
    mov bp, sp
    push ax bx dx es ds
    mov ax, [bp+10]
    mov es, ax
    mov ds,ax
    mov ah, 4bh
    mov al, 0
    mov bx, [bp+8]
    mov dx, [bp+6]
    int 21h
    pop ds es dx bx ax
    mov sp, bp
    pop bp
    ret 6
exec endp

code ends
end