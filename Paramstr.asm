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
    ; �ய�� ࠧ����⥫��
@skip_delimiters:
    cmp al, 0Dh          ; �᫨ ����� ��ப� (0Dh), �����蠥�
    je @split_done
    ;lodsb                ; ����㦠�� ᨬ��� �� ��ப� � AL
    mov al, byte ptr ds:[si]
    inc si
    cmp al, ' '          ; �᫨ �஡��, �ய�᪠��
    je @split_string
    cmp al, '/'          ; �᫨ '/', �ய�᪠��
    je @skip_delimiters
    cmp al, 0Dh          ; �᫨ ����� ��ப� (0Dh), �����蠥�
    je @split_done
                         ; ��砫� ����� �����ப�
    dec si               ; �����頥��� � ��ࢮ�� ᨬ���� �����ப�
    mov dx, di           ; ���࠭塞 ��砫� �����ப�
    mov bx, 0            ; ���稪 ����� �����ப�
    inc di
    ; ����஢���� �����ப�
@copy_substring:
    lodsb                ; ����㦠�� ᨬ��� �� ��ப� � AL
    cmp al, 20h          ; �᫨ �஡��, �����蠥� �����ப�
    je @end_substring
    cmp al, '/'          ; �᫨ '/', �ய�᪠��
    je @end_substring
    cmp al, 0Dh          ; �᫨ ����� ��ப�, �����蠥� �����ப�
    je @end_substring
    mov byte ptr es:[di], al
    inc di
    inc bx               ; �����稢��� ���稪 �����
    jmp @copy_substring

    ; �����襭�� �����ப�
@end_substring:
    mov byte ptr [si-1], 0        ; �����塞 ࠧ����⥫� �� 0 (����� ��ப�)
    xchg di,dx 
    mov byte ptr [di], bl       ; ���࠭塞 ����� �����ப� � ���� ����
    xchg di,dx 
    inc byte ptr paramstr_array ; �����稢��� ���稪 �����ப
    jmp @split_string     ; ���室�� � ᫥���饩 �����ப�
@split_done:
    ; �����襭�� �ணࠬ��

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