
bios_process_linked_seg equ 040h
bios_process_linked_offs equ 0f0h

; es:di - address of process in the process table

@get_ptr_to_data macro num
    xor ax,ax
    mov al, bios_process_linked_seg
    mov es,ax
    mov al, bios_process_linked_offs
    mov bx, ax
    mov di,word ptr es:[bx+4*num]
    mov ax,word ptr es:[bx+2+4*num]
    mov es,ax
  endm


@get_ptr_to_thread_data macro process_num
    xor ax,ax
    mov al, bios_process_linked_seg
    mov es,ax
    mov al,  bios_process_linked_offs
    mov bx, ax
    mov di,word ptr es:[bx]
    mov ax,word ptr es:[bx+2]
    mov es,ax
    mov bx,process_num
    mov ax,size TThread
    mul bx
    add di,ax
    endm

@init_process macro process_num, savearea 
    pushf
    mov ax,cs
    push ax
    lea ax, savearea
    push ax
    xor ax,ax
    push ax bx cx dx si di ds es 
    xor ax,ax
    push ax   ;bp

    @get_ptr_to_thread_data process_num

    ;lea di, es:[bx[si * process_num]]
  ;  xor ax,ax
    pushf
    pop ax
    stosw
    mov ax,cs           ;cs
    stosw
    lea ax, savearea   ;ip
    stosw
    mov cx, 6           ;ax, bx, cx, dx, si, di
    mov ax,0
rep stosw
    mov ax, data        ;ds
    stosw
    mov ax, 0           ;es
    stosw
    mov ax, bp
    stosw               ;bp
    mov ax,sp
    ;sub ax,24
    stosw                ;sp
    mov ax,ss
    stosw                 ;ss
    mov al, 1
    stosb
    xor ax,ax
    mov ax,bp           ;
   ; pop es cx si di bx ax
endm

@deactivate_process macro process_num
@get_ptr_to_thread_data process_num
mov al,0
mov byte ptr es:[di].TThread.status,al
endm

@restore_vect macro inum, savearea
    push ax es dx 
    mov ax, 0
    mov es, ax
    mov ax, word ptr cs:[savearea]
    mov dx, word ptr cs:[savearea+2]
    cli
    mov es:[inum*4], ax
    mov es:[inum*4+2], dx
    sti
    pop dx es ax
 endm

 @change_vect macro inum, new, savearea
    push ax es dx 
    mov ax,0
    mov es,ax
    mov ax,es:[4*inum]
    mov dx,es:[4*inum+2]
    mov word ptr cs:[savearea],ax
    mov word ptr cs:[savearea+2],dx
    mov ax,offset cs:new
    cli
    mov es:[4*inum], ax
    mov es:[4*inum+2], cs
    sti
    pop dx es ax
 endm

;************** KEYBOARD ********************

 @perepere_int09 macro
;------ ᫥���騩 ��� ����室�� ��� ��ࠡ�⪨ �����⭮�� ���뢠���
         in      al,kb_ctrl         ;����� ���祭�e ���� �ࠢ����� ��������ன
         mov     ah,al              ; ��࠭��� ���
         or      al,80h             ;��⠭����� ��� ࠧ�襭�� ��� ����������
         out     kb_ctrl,al         ; � �뢥�� ��� � �ࠢ���騩 ����
         xchg    ah,al              ;������� ��室��� ���祭�� ����
         out     kb_ctrl,al         ; � ������� ��� ���⭮
         mov     al,eoi             ;��᫠�� ᨣ��� "����� ���뢠���"
         out     int_ctrl,al        ; ����஫���� ���뢠��� 8259
 endm
;************** KEYBOARD ********************