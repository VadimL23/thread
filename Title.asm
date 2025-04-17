.model SMALL
include prc_ids.inc
include struc.inc
include int_macr.inc
include utils.inc

EXTRN gotoxy:PROC, clrscr:PROC, _getxy:PROC, paramstr:PROC, paramstr_array, atoi:PROC

print_colored macro str
    push dx
    mov ah,09h
    lea dx, str
    int 21h
    pop dx
endm

; title_process_id equ 2
is_test equ 0
installed equ 0ffh
function_2f equ 0c0h + title_process_id -1

data segment PARA PUBLIC 'DATA'
    program_length dw 0
    title_str db 'Курсовой проект. Лачков Вадим Валерьевич. Группа: ПБ-21'
    title_length equ $-title_str
    allredy_installed_err db 'Allredy installed!$'
    location dw 0
data ends

code segment PARA PUBLIC 'CODE'
    ASSUME CS:code, DS:data, SS:stack

jmp init

;------ Vectors -------------

old_2fh dd 0

color db 1

signal_stop_process dw 0
maincs dw 0
;-----------------------------

new_2fh proc
    local
    cmp ah, function_2f
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff
    mov cs:signal_stop_process,1
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp
    
    init:
        mov ax, data
        mov ds, ax
        call paramstr
        
        xor ax,ax
        mov al, byte ptr paramstr_array
        or ax,ax
        je @without_param

        lea ax, [paramstr_array+1]
        push ax
        call atoi  
        cmp ax,1
        jne @without_param
        inc location

@without_param:
        mov ax,zzz
        mov dx, es
        sub ax,dx
        mov program_length,ax
        xor ax,ax
        mov ah,function_2f
        int 2fh
        cmp al, installed
        jne @not_installed
        call do_exit
@not_installed:  
        mov ax, is_test
        or ax,ax
        jne @it_is_test
        @change_vect 2fh new_2fh old_2fh
        jmp @main
@it_is_test:
        call clrscr
@test_cycle:
        call process
        loop @test_cycle
@main:
        mov ax, 0b800h
        mov es,ax
        
       @init_process title_process_id, @cycle
     
        ;call free_not_used_mem
; jmp stdout_time
        mov ax, cs
        mov cs:maincs, ax 
        mov dx,program_length
        mov ax, 3100h
        int 21h

@cycle:
        mov ax,signal_stop_process
        or ax,ax
        jne @stop_TSR
        call process
        loop @cycle

@stop_TSR:
       
        @deactivate_process title_process_id
@rrr:
        jmp @rrr        ; This is STUB 
        
do_exit proc
        mov ax,0d23h
        push ax
        call gotoxy
        mov ax, seg allredy_installed_err
        mov ds,ax
        lea dx, allredy_installed_err
        mov ah,09h
        int 21h
        mov ax, 4Cffh
        int 21h
        ret
do_exit endp

get_offset proc
        mov di,79-title_length
        mov ax, 80*2*24
        mul word ptr ds:location
        add di,ax
        xor si,si
        ret
get_offset endp

process proc
        push bp
        mov bp, sp
        mov ax, 0b800h
        mov es,ax
        mov cx,title_length
        call get_offset
        xor ax,ax
        mov al,cs:color
        or al,08h
        mov bl,0fh
        div bl
        mov cs:color,ah

        cld
@stdout_title:        
        mov al, byte ptr ds:title_str+[si]
        stosb
        mov al,cs:color
        stosb
        inc si
        loop @stdout_title
        inc cs:color
        delay 10
        mov sp, bp
        pop bp
        ret
process endp
code ends

stack segment para stack
        dw 200h dup(0)
stack ends

zzz segment
zzz ends

end