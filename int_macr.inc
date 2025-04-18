
; ****************************************************************
; * Модуль вспомогательных макросов                              *
; *                                                              *
; ****************************************************************

bios_process_linked_seg equ 040h              ; Сегмент и смещение пользовательских данных
bios_process_linked_offs equ 0f0h             ; предназначенных для мнэпроцессного взаимодействия

; es:di - The address of process in the process table

; Макрос устанавливает указатель es:di на первую запись в таблице потоков по номеру потока

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

; Макрос устанавливает указатель es:di на произвольную запись в таблице потоков по номеру потока

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

; Макрос инициализирует нлвую запись в ьаблице процессов

@fork macro process_num, savearea 
    pushf
    mov ax,cs
    push ax
    lea ax, savearea
    push ax
    xor ax,ax
    push ax bx cx dx si di ds es 
    xor ax,ax
    push ax                               ;bp
    @get_ptr_to_thread_data process_num
    pushf
    pop ax
    stosw
    mov ax,cs                             ;cs
    stosw
    lea ax, savearea                      ;ip
    stosw
    mov cx, 6                             ;ax, bx, cx, dx, si, di
    mov ax,0
rep stosw
    mov ax, data                          ;ds
    stosw
    mov ax, 0                             ;es
    stosw
    mov ax, bp
    stosw                                 ;bp
    mov ax,sp
    ;sub ax,24
    stosw                                 ;sp
    mov ax,ss
    stosw                                 ;ss
    mov al, 1
    stosb
    xor ax,ax
    mov ax,bp           
endm

; Макрос устанавливает статус потока нефктивным

@deactivate_process macro process_num
@get_ptr_to_thread_data process_num
mov al,0
mov byte ptr es:[di].TThread.status,al
endm

; Макрос восстанавливает старое значение вектор прерывания

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

; Макрос сохраняет вектор прерывания и устанавливает новое значение

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
; Макрос для отработки аппаратного прерывания клавиатуры

 @perepere_int09 macro
         ;------ следующий код необходим для отработки аппаратного прерывания
         in      al,kb_ctrl         ;взять значениe порта управления клавиатурой
         mov     ah,al              ; сохранить его
         or      al,80h             ;установить бит разрешения для клавиатуры
         out     kb_ctrl,al         ; и вывести его в управляющий порт
         xchg    ah,al              ;извлечь исходное значение порта
         out     kb_ctrl,al         ; и записать его обратно
         mov     al,eoi             ;послать сигнал "конец прерывания"
         out     int_ctrl,al        ; контроллеру прерываний 8259    
 endm
;************** KEYBOARD ********************