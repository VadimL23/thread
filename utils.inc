
; ***************************************************************
; *                          *
; *                                                             *
; ***************************************************************

;  ĒāŽá āĨ Ģ¨§ãĨâ § ¤ĨāĻĒã

delay macro time
        local
        push cx
        mov cx, time
@@:
        push cx
        mov cx, 0ffffh
@@l:
        loop @@l
        pop cx
        loop @@
        pop cx
endm

;  ĒāŽá ĸëĸŽ¤¨â áâāŽĒã ­  íĒā ­

print_str macro str
    push dx
    mov ah,09h
    lea dx, str
    int 21h
    pop dx
endm