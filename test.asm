.model SMALL
include struc.inc

EXTRN draw_window:PROC, clrscr:PROC

data segment para public 'data'

data ends

code segment para public 'code'
assume cs:code, ds:data
jmp @start

 wnd TWind<20,0,24,79,21,1,23,78>


@start:

    call clrscr
    mov ax, seg wnd
    push ax 
    mov ax, offset wnd
    push ax
    call far ptr draw_window

mov ax,4c00h
int 21h

code ends
end