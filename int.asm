.model SMALL

include int_macr.inc
include struc.inc

public new_09h
public old_09h
public is_all

EXTRN dump:PROC

code SEGMENT PARA PUBLIC 'code'
    assume cs:code, ds:data
 
jmp init

is_all dw 0

int_ctrl equ 20H ; Порт кправления прерыванием
eoi equ 20h      ; Сбросить команду управления прерыванием
kb_data equ 60h  ; Порт данных клавиатуры
kb_ctrl equ 61h  ; Порт управления клавиатуры

kb_f1_msg db 'F',03h,'1',03h
kb_f2_msg db 'F',04h,'2',04h
kb_right_arrow_msg db 'R',09h,'T',09h
kb_left_arrow_msg db 'L',02h,'T',02h
kb_up_arrow_msg db 'U',03h,'P',03h
kb_down_arrow_msg db 'D',04h,'N',04h

kb_esc equ 01h
kb_F1 equ 3Bh   ;зап./ост. поток чтения из файла  
kb_F2 equ 3Ch   ;зап./ост. поток вывода на экран информации из файла
kb_F3 equ 3Dh   ;зап./ост. вывод заголовка
kb_F4 equ 3Eh   ;зап./ост. движущийся обьект
kb_F5 equ 3Fh   ;зап./ост. часы
kb_F6 equ 40h
kb_F7 equ 41h
kb_F8 equ 42h
kb_F9 equ 43h

kb_right_arrow equ 4dh
kb_left_arrow  equ 4bh
kb_down_arrow  equ 50h
kb_up_arrow    equ 48h

    old_09h   dd 0

@kb_handler macro sting
        @perepere_int09
         ; обработчики нажатия клавиши
        push cs
        lea ax,  sting
        push ax
        xor ax,ax
        mov ax, 4
        push ax
        call far ptr dump

        ;  jmp @to_exit
endm

@kb_handler_start_stop_process macro process_number
        ; @perepere_int09
         ; обработчики нажатия клавиши
        @get_ptr_to_thread_data process_number
        mov ax,0
        mov al, es:[di].TThread.status
        inc al
        mov dl,2
        div dl
        mov es:[di].TThread.status, ah
        jmp @to_exit
endm

new_09h proc far
    cli
    ; pushf
    ; call cs:old_09h
    push ax bx cx dx si di ds es bp
         in      al,kb_data            ;читать ключ

         cmp     al,kb_F1             
         jne      @dont_F1           
         @kb_handler_start_stop_process 1
@dont_F1:         
         
         cmp     al,kb_F2           
         jne      @dont_F2
         @kb_handler_start_stop_process 2
@dont_F2:    

         cmp     al,kb_F3             
         jne      @dont_F3           
         @kb_handler_start_stop_process 3
@dont_F3:  

         cmp     al,kb_F4             
         jne      @dont_F4           
         @kb_handler_start_stop_process 4
@dont_F4:     
       
        cmp     al,kb_F5             
         jne      @dont_F5           
         @kb_handler_start_stop_process 5
@dont_F5: 

        cmp     al, kb_right_arrow
        jne      @dont_kb_right_arrow
        @perepere_int09
        @kb_handler kb_right_arrow_msg
        @get_ptr_to_data 1
        mov byte ptr es:[di+2], 0
        mov byte ptr es:[di+1], 1 ; mov byte ptr es:row_k,-1
@dont_kb_right_arrow:
        
        cmp     al, kb_left_arrow
        jne      @dont_kb_left_arrow
        @perepere_int09
        @kb_handler kb_left_arrow_msg
        @get_ptr_to_data 1
        mov byte ptr es:[di+2], 0
        mov byte ptr es:[di+1], -1 ; mov byte ptr es:row_k,-1

 @dont_kb_left_arrow:
        
        cmp     al, kb_down_arrow
        jne      @dont_kb_down_arrow
        @perepere_int09
        @kb_handler kb_down_arrow_msg
        @get_ptr_to_data 1
        mov byte ptr es:[di+2], 0
        mov byte ptr es:[di], 1 ; mov byte ptr es:col_k,1
@dont_kb_down_arrow:
        
        cmp     al, kb_up_arrow
        jne      @dont_kb_up_arrow
        @perepere_int09
        @kb_handler kb_up_arrow_msg
        @get_ptr_to_data 1
        mov byte ptr es:[di+2], 0
        mov byte ptr es:[di], -1 ; mov byte ptr es:col_k,-11
@dont_kb_up_arrow:
         
         cmp     al,kb_esc          ;
         jne      @dont_esc          ;
        call do_esc 
@dont_esc:
                                    ; нет, уйти на исходный обработчик
    pop bp es ds di si dx cx bx ax
         jmp     cs:[old_09h]       ;переход на первоначальный обработчик

do_esc proc
    mov is_all,1
    ret
do_esc endp

restore_09h:
    @restore_vect 09h old_09h

@to_exit:
    pop bp es ds di si dx cx bx ax
    sti
    jmp     cs:[old_09h]
new_09h endp
resize equ $-new_09h

init:
    mov ax, data
    mov ds, ax

   ; @change_vect 9 new_09h old_09h
        

@test:
    cmp is_all,0
    jne @its_all
    jmp @test

    mov dx,(resize+10fh)/16
    mov ax, 3100h
    int 21h

@its_all:
    mov ax, 4c00h
    int 21h

code ends 

data segment PARA PUBLIC 'data'
data ends


; stack segment stack
; db 100 dw dup(0)
; stack ends




zzz segment
zzz ends
end

