.MODEL SMALL

cr equ 0dh

public paramstr
public paramstr_array

code segment PARA public USE16 'code'
 assume cs:code, ds:data, es:data

paramstr proc
    push bp
    mov bp,sp

; Make PSP promt adress
mov ax, data
mov ds, ax
mov ax, es
mov word ptr [cmd_tail_addr+2],ax

push ds
pop es

lds si,cmd_tail_addr
xor cx,cx
xor bx,bx
mov cl, [si]
mov bl, [si]
inc si

; Trim the head
    cld
@loads_bytes_head:
    lodsb
    cmp al,' '
    je  @next_byte_head
    jmp @exit_trim_head
@next_byte_head:
    loop @loads_bytes_head
@exit_trim_head:
   ; jz @empty_str
   
; Get offset start
    mov ax,bx
    sub ax,cx
    sub bx,ax
    mov word ptr es:[supl_offset_start_at],ax

    lds si, es:[cmd_tail_addr]
    xor cx,cx
    xor bx,bx
    mov cl, [si]
    mov bl, [si]
    
    ; si = prompt length
    ; cx = prompt length - head offset start
    add si,cx
    sub cx,ax

    std
@loads_bytes_tail:
    lodsb
    cmp al,' '
    je  @next_byte_tail
    jmp @exit_trim_tail
@next_byte_tail:
    loop @loads_bytes_head
@exit_trim_tail:

    inc cx
    mov bx, cx
    mov ax, es:[supl_offset_start_at]
    sub bx,ax
    mov cx,bx
    ; cx = string length without spaces

    mov es:[tail_raw_length],cl
    lds si, es:[cmd_tail_addr]
    inc si

    mov ax, es:[supl_offset_start_at]
    add si,ax
    lea di, es:[tail_raw]
    cld
rep movsb

    mov byte ptr es:[di], 0dh
    push es
    pop ds
    lea si, tail_raw
    lea di, paramstr_array
    inc di
    cld

@split_string:
    ; Пропуск разделителей
@skip_delimiters:
    cmp al, 0Dh          ; Если конец строки (0Dh), завершаем
    je @split_done
    ;lodsb                ; Загружаем символ из строки в AL
    mov al, byte ptr ds:[si]
    inc si
    cmp al, ' '          ; Если пробел, пропускаем
    je @split_string
    cmp al, '/'          ; Если '/', пропускаем
    je @skip_delimiters
    cmp al, 0Dh          ; Если конец строки (0Dh), завершаем
    je @split_done
                         ; Начало новой подстроки
    dec si               ; Возвращаемся к первому символу подстроки
    mov dx, di           ; Сохраняем начало подстроки
    mov bx, 0            ; Счетчик длины подстроки
    inc di
    ; Копирование подстроки
@copy_substring:
    lodsb                ; Загружаем символ из строки в AL
    cmp al, 20h          ; Если пробел, завершаем подстроку
    je @end_substring
    cmp al, '/'          ; Если '/', пропускаем
    je @end_substring
    cmp al, 0Dh          ; Если конец строки, завершаем подстроку
    je @end_substring
    mov byte ptr es:[di], al
    inc di
    inc bx               ; Увеличиваем счетчик длины
    jmp @copy_substring

    ; Завершение подстроки
@end_substring:
    mov byte ptr [si-1], 0        ; Заменяем разделитель на 0 (конец строки)
    xchg di,dx 
    mov byte ptr [di], bl       ; Сохраняем длину подстроки в первый байт
    xchg di,dx 
    inc byte ptr paramstr_array ; Увеличиваем счетчик подстрок
    jmp @split_string     ; Переходим к следующей подстроке
@split_done:
    ; Завершение программы

@out_prog:
    mov sp,bp
    pop bp
    ret
paramstr endp
code ends


data segment PARA PUBLIC 'data'

cmd_tail_addr dd 00000080h
tail_raw_length db 0
tail_raw db 254 dup(0)

supl_offset_start_at dw 0
paramstr_array db 256 dup (0)

data ends

end