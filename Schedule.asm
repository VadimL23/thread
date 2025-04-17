.model SMALL
include int_macr.inc
include struc.inc

public old_08h
public scheduler

;***** variables ******
public disableHardwareEvents
public threadsRegistered
;***** variables ******

code segment PARA PUBLIC 'code'
assume cs:code, ds:code

disableHardwareEvents dw 0
threadsRegistered dw 0 
parallelStart   dw 1
threadNumber   dw 0
tsoffset_start dw 0
NumPrev dw 0
preemptiveSwitch dw 1
parallelfinished dw 0

ThreadTable dw 0 

mainss dw 0
mainsp dw 0
mainds dw 0

tsoffset dw 0

old_08h dd 0

video_addr dw 0
           dw 0b800h
str_c equ 0bh
str_c_yellow equ 0eh
str_c_white equ 0fh


pos_to_out_state equ 80*2*23
status_str db 'I',str_c,'D',str_c, ' '
           db  str_c,'а',str_c,'к',str_c,'т',str_c,'и',str_c,'в',str_c,'н',str_c
           db 'о',str_c,'г',str_c,'о',str_c,' ',str_c,'п',str_c,'о',str_c
           db 'т',str_c,'о',str_c,'к',str_c,'а',str_c,':',str_c,' ',str_c, ' ',str_c,' ',str_c     ;46

thread_str db ' ', str_c_yellow
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'0',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'1',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'2',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'3',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'4',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow     
           db 'I',str_c_yellow,'D',str_c_yellow,'#',str_c_yellow,'5',str_c_yellow,'-',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow,' ',str_c_yellow ;80
status_threads_table db 10 dup(-1)

scheduler proc far
    push ax bx cx dx si di ds es
    push bp
    mov bp,sp

; ******** call harware handler old_08h *************
    mov ax, cs:disableHardwareEvents
    cmp ax,1
    je @disableHardwareEvents
    pushf
    call cs:old_08h
@disableHardwareEvents: 

    mov ax, cs:preemptiveSwitch ; if preemptiveSwitch or DisableHardwareEvents then begin {wrap begin --- start}
    or ax,ax
    jnz @do_subrutine   
    mov ax, cs:disableHardwareEvents
    or ax,ax 
    jnz @do_subrutine
    jmp @do_not_subrutine

@do_subrutine:
    mov ax, cs:threadsRegistered   ;if (ThreadsRegistered > 0) or (parallelStart) then begin {first begin --- start}
    cmp ax, 0
    jg @go_to_save_current_process
    mov ax,cs:parallelStart
    cmp cs:parallelStart,1
    je @go_to_save_current_process
    jmp @not_change_processes
@go_to_save_current_process:
    cli
    mov ax,cs:parallelStart
    cmp cs:parallelStart,1  ;if ParallelStart then begin {start parallel}
    jne @not_parallel_start
@it_is_first_save_only_to_main_process:
    mov ax, sp
    mov cs:mainsp, ax
    mov ax, ss
    mov cs:mainss, ax
    mov cs:mainds, ds
    push ds
    mov ds, ax
    mov ax, bp
    mov si, ax

    xor ax,ax
    mov al, bios_process_linked_seg
    mov es,ax
    mov al,  bios_process_linked_offs
    mov bx, ax
    mov di,word ptr es:[bx]
    mov cs:ThreadTable, di
    mov ax,word ptr es:[bx+2]
    mov es,ax                   ; es:di -> tableThreads

    add di, 22
    mov cx, 12
@l:
    mov ax, word ptr ds:[si]
    mov word ptr es:[di], ax
    add si, 2
    sub di, 2
    loop @l
    
    mov ax, cs:ThreadTable
    add ax, 24
    mov di, ax
    mov ax, bp
    stosw
    mov ax, ss
    stosw
    pop ds
    jmp @end_subroutine

@not_parallel_start:        ;if not ParallelStart then begin
    ; call far ptr not_parallel_start_sub_routine

    mov ax, cs:threadNumber    ;tsoffset := (ThreadNumber + 1) * sizeof(TThreadStateWord) - 5; {29-7=2 <=> 29 -sizeof(word)*3 - sizeof(byte)}
    inc ax
    mov bx, size TThread
    mul bx
    sub ax,7
    mov cs:tsoffset,ax
    add ax,2
    mov cs:tsoffset_start,ax   ; tsoffset_start := tsOffset + 2;

    push ds
    mov ax, ss
    mov ds, ax
    mov ax, bp
    mov si, ax
   
    xor ax,ax
    mov al, bios_process_linked_seg
    mov es,ax
    mov al,  bios_process_linked_offs
    mov bx, ax
    mov di,word ptr es:[bx]
    mov cs:ThreadTable, di
    mov ax,word ptr es:[bx+2]
    mov es,ax
    add di, cs:tsoffset
    mov cx, 12
@l1:
    mov ax, word ptr ds:[si]
    mov word ptr es:[di], ax
    add si, 2
    sub di, 2
    loop @l1

    mov ax, cs:ThreadTable
    add ax, cs:tsoffset_start
    mov di, ax
    mov ax, bp
    stosw
    mov ax, ss
    stosw
    pop ds    ; end {store to TS}

@end_subroutine:

    mov ax, word ptr cs:threadNumber    ;NumPrev := ThreadNumber;
    mov word ptr cs:NumPrev, ax

;***************************************
    ;call make_new_thread_number
    @repeat:
    xor dx,dx
    mov ax,cs:threadNumber
    inc ax
    mov cx, cs:ThreadsRegistered
    div cx
    mov cs:threadNumber, dx
    cmp dx, cs:NumPrev
    je @until
    mov ax, dx
    mov dx, size TThread
    mul dx
    add ax, cs:ThreadTable
    mov di,ax
    push ds 
    mov ds, word ptr cs:mainds
    mov al, [di].TThread.status
    pop ds
    cmp al, 1
    je @until
    jmp @repeat
@until: ;  until (ThreadNumber = NumPrev) or TS[ThreadNumber].active;
;****************************************

    ;if ts[ThreadNumber].active and ((ThreadNumber <> NumPrev) or parallelStart) {loading from TS}
    mov ax, cs:threadNumber
    mov dx, size TThread
    mul dx
    add ax, cs:threadTable
    mov di,ax
    push ds 
    mov ds, word ptr cs:mainds
    mov al, [di].TThread.status
    pop ds
    cmp al, 1
    jne @no_load_from_TS
    mov ax, cs:threadNumber
    cmp ax, cs:NumPrev
    jne @load_from_TS
    mov ax, cs:parallelStart
    cmp ax, 0
    je @no_load_from_TS     ;begin
@load_from_TS:

    mov ax, cs:threadNumber    ;tsOffset := (ThreadNumber + 1) * sizeof(TThreadStateWord) - 3; {26}
    inc ax
    mov dx, size TThread
    mul dx
    sub ax,3
    mov cs:tsOffset,ax
    
    mov ax,cs:threadNumber     ;tsoffset_start := (ThreadNumber) * sizeof(TThreadStateWord);
    mov dx, size TThread
    mul dx
    mov cs:tsoffset_start,ax


;*****************************************
    mov dx, ds
    mov ax, cs:mainds
    mov ds, ax
    mov si, cs:threadTable
    add si, cs:tsOffset
    
    std
    lodsw
    mov ss, ax
    lodsw
    mov bp, ax
    add ax, 12 * 2
    mov sp, ax

    mov si, cs:ThreadTable
    add si, cs:tsoffset_start

    cld
    mov cx, 12

    @m1:
    lodsw
    push ax
    loop @m1 

    mov ds, dx ;end {loading from TS}

;*****************************************

@no_load_from_TS:

    ;  else if (not ts[Threadnumber].active) and (Threadnumber = NumPrev) then
    mov ax, cs:threadNumber
    mov dx, size TThread
    mul dx
    add ax, cs:ThreadTable
    mov di,ax
    push ds
    mov ds, word ptr cs:mainds 
    mov al, [di].TThread.status
    pop ds
    cmp al, 1
    je @not_active_and_equal_prev
    mov ax, cs:threadNumber
    cmp ax, cs:NumPrev
    jne @not_active_and_equal_prev

    @restore_vect 1ch, old_08h        ; setintvec($8, @OldTimerVec);
          mov ax, cs:mainss
          mov ss, ax
          mov ax, cs:mainsp
          mov bp, cs:mainsp
          sub ax, 12 * 2
          mov sp, ax
@not_active_and_equal_prev:
    mov cs:parallelstart,0
@not_change_processes: ;{first begin --- end}
    sti
@do_not_subrutine:
    mov cs:disableHardwareEvents, 0
    sti

; ************* STAUS STDOUT ***************
; This is not needed for the scheduler. It's just stdout status.
; Печатает статус активного потока
    cld
    mov cx,46
    les di, dword ptr cs:video_addr
    xor ax, ax
    mov ax,pos_to_out_state
    mov di,ax
    mov ax,cs
    mov ds,ax
    lea si, status_str
rep movsb
    sub di,1
    sub di,1 
    mov cx,1
    mov al, byte ptr cs:threadNumber
    add al,'0'
    mov ah,0ch    
 rep stosw
    lea si, thread_str
    mov cx, 90
rep movsb
 ; Печатает статусы потоков

    
    mov cx, threadsRegistered
    lea bx, status_threads_table
@get_statuses:
    mov ax, threadsRegistered
    sub ax, cx
    mov dx, size TThread
    mul dx
    add ax, cs:ThreadTable
    mov si,ax
    push ds
    mov ds, word ptr cs:mainds 
    mov al, [si].TThread.status
    pop ds
    mov byte ptr ds:[bx], al
    inc bx
loop @get_statuses
    
    lea bx, status_threads_table
    mov cx, threadsRegistered
    sub di, 76
@out_statuses: 
    
    mov al, byte ptr [bx]
    add al, 30h
    mov ah, str_c_white
    stosw
    add di,2
    inc bx
    add di, 12
loop @out_statuses


; ************* STAUS STDOUT ***************
    mov sp, bp
    pop bp
    pop es ds di si dx cx bx ax
    iret
scheduler endp
code ends

data segment PARA PUBLIC 'data'
data ends
end