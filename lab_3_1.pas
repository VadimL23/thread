uses
  Crt, Dos;

const
  StackSize = 4096;
  BufferSize = 10;
  ProducerSleepTime: integer = 0;
  ConsumerSleepTime: integer = 0;

type
  TThreadStateWord = record
    flags, cs, ip, ax, bx, cx, dx, si, di, ds, es, bp, sp, ss: word; {14 words} {53}
    active: boolean;
  end;

type
  TBuffer = array [1..BufferSize] of Integer;

var
  ts: array[0..2] of TThreadStateWord;
  ThreadsRegistered: byte;
  OldTimerVec: Procedure;
  DisableHardwareEvents: boolean;
  preemptiveSwitch: boolean;
  parallelfinished: boolean;
  parallelStart: boolean;
  Stacks: array[0..1] of Pointer;
  Buffer: TBuffer;
  BufferIndex: Integer;
  code1, code2: integer;
  ch: char;

const
  ProducerTimeQuantum: Integer=0; {Случайный квант времени для производителя}
  ConsumerTimeQuantum: Integer=0; {Случайный квант времени для потребителя}
  ProducerInterruptCount: Integer=0; {Счетчик прерываний для производителя}
  ConsumerInterruptCount: Integer=0; {Счетчик прерываний для потребителя}

procedure SwitchThreads;
begin
  if not parallelFinished then
    asm
      cli
      mov DisableHardwareEvents, 1
      int 08h
      sti
    end;
end;

procedure print(val: integer);
var
  l: integer;
  i: integer;
begin
  gotoxy(20, 10); writeln('--------------------------------------------------------');
  gotoxy(20, 11); writeln('|   Producer:                                           |');
  gotoxy(20, 12); writeln('|   Consumer:                                           |');
  gotoxy(20, 13); writeln('|   Buffer:                                             |');
  gotoxy(20, 14); writeln('|   Buffer fill percentage:                             |');
  gotoxy(20, 15); writeln('--------------------------------------------------------');

  if (ts[1].active) then
  begin
    gotoxy(34, 12); write('                           ');
    gotoxy(34, 11); write('Active '); write('produced: ', val);
    gotoxy(34, 12); write('Hold');
  end;
  if (ts[2].active) then
  begin
    gotoxy(34, 11); write('                            ');
    gotoxy(34, 11); write('Hold');
    gotoxy(34, 12); write('Active '); write('consumed: ', val);
  end;

  gotoxy(34, 13);
  for i := 1 to BufferIndex do
  begin
    write(Buffer[i]);
    write(' ');
  end;
  gotoxy(52, 14);
  Write((BufferIndex / BufferSize) * 100:0:2, '%');
end;

procedure HideCursor;
begin
  asm
    mov ah, 01h  { Функция установки формы курсора}
    mov ch, 20h  { Верхняя граница курсора (20h = 32 в десятичной системе)}
    mov cl, 20h  { Нижняя граница курсора (20h = 32 в десятичной системе)}
    int 10h      { Вызов прерывания BIOS}
  end;
end;

procedure ShowCursor;
begin
  asm
    mov ah, 01h  { Функция установки формы курсора}
    mov ch, 06h  { Верхняя граница курсора (6 в десятичной системе)}
    mov cl, 07h  { Нижняя граница курсора (7 в десятичной системе)}
    int 10h      { Вызов прерывания BIOS}
  end;
end;

{+$F}
procedure TimerHandler(flags, cs, ip, ax, bx, cx, dx, si, di, ds, es, bp: word); interrupt;
{Храним все переменный как константы}
{это ячейки памяти, если как переменные, то}
{хранятся в локальном выделенном стеке и теряются}
{при переключении контекста потоков}
const 
  ThreadNumber: byte = 0; 
  NumPrev: byte = 0;
  tsoffset: word = 0;
  tsoffset_start: word = 0;
  mainSP: word = 0;
  mainSS: word = 0;
  print: byte = 0;
  iter: byte = 2;
  i: integer = 0;
begin
  if not DisableHardwareEvents then
  begin
    asm pushf end;
    OldTimerVec;
  end;

  if preemptiveSwitch or DisableHardwareEvents then begin {wrap begin --- start}
   
    if (ThreadsRegistered > 0) or (parallelStart) then begin {first begin --- start}
                      {4975:05c4}
      asm cli end;
      if ParallelStart then begin {start parallel}
        asm
          mov ax, ss
          mov mainss, ax
          mov ax, bp
          mov mainsp, bp
          push ds
          mov ax, ss
          mov ds, ax
          mov ax, bp
          mov si, ax
          mov ax, seg ts
          mov es, ax
          mov ax, offset ts
          add ax, 22
          mov di, ax
          mov cx, 12
         
          @l:
            mov ax, word ptr ds:[si]
            mov word ptr es:[di], ax
            add si, 2
            sub di, 2
            loop @l

          mov ax, offset ts
          add ax, 24
          mov di, ax
          mov ax, bp
          stosw
          mov ax, ss
          stosw
          pop ds
        end;
      end;


      if not ParallelStart then begin {store to TS}
        tsoffset := (ThreadNumber + 1) * sizeof(TThreadStateWord) - 5; {29-7=2 <=> 29 -sizeof(word)*3 - sizeof(byte)}
        tsoffset_start := tsOffset + 2;
        asm
          push ds
          mov ax, ss
          mov ds, ax
          mov ax, bp
          mov si, ax
          mov ax, seg ts
          mov es, ax
          mov ax, offset ts
          add ax, tsoffset
          mov di, ax

          mov cx, 12
          @l:
            mov ax, word ptr ds:[si]
            mov word ptr es:[di], ax
            add si, 2
            sub di, 2
            loop @l

          mov ax, offset ts
          add ax, tsoffset_start
          mov di, ax
          mov ax, bp
          stosw
          mov ax, ss
          stosw
          pop ds
        end;
      end; {store to TS}



      NumPrev := ThreadNumber;

      repeat
        ThreadNumber := (ThreadNumber + 1) mod ThreadsRegistered;
      until (ThreadNumber = NumPrev) or TS[ThreadNumber].active;

      if ts[ThreadNumber].active and ((ThreadNumber <> NumPrev) or parallelStart) {loading from TS}
      then
      begin
        tsOffset := (ThreadNumber + 1) * sizeof(TThreadStateWord) - 3; {26}
        tsoffset_start := (ThreadNumber) * sizeof(TThreadStateWord);
                        {4A58:0113 - title}
                        {4976:05c4 - table}
                        {48ed:}
{1. call harware handler old_08h -> @do_subrutine -> @it_has_to_save_main_process ->}
{call make_new_thread_number -> @load_from_TS ->}                          
        asm
          mov dx, ds
          mov ax, ss
          mov es, ax
          mov ax, bp
          mov bx, ax

          mov ax, seg TS
          mov ds, ax
          mov si, offset TS
          add si, tsOffset

          std
          lodsw
          mov ss, ax
          lodsw
          mov bp, ax
          add ax, 12 * 2
          mov sp, ax

          mov si, offset ts
          add si, tsoffset_start
          cld
          mov cx, 12

          @m1:
            lodsw
            push ax
            loop @m1

          mov ds, dx
        end;
      end {loading from TS}

      else if (not ts[Threadnumber].active) and (Threadnumber = NumPrev) then
      begin
        setintvec($8, @OldTimerVec);
        asm
          mov parallelfinished, true
          mov ax, mainss
          mov ss, ax
          mov ax, mainsp
          mov bp, mainsp
          sub ax, 12 * 2
          mov sp, ax
        end;
      end;
      parallelstart := false;
    end; {first begin --- end}



    asm sti end;
  end; {wrap begin --- end}

  DisableHardwareEvents := false;
  asm sti end;

  { Уменьшение счетчиков прерываний}
  Dec(ProducerInterruptCount);
  Dec(ConsumerInterruptCount);

  {Проверка счетчиков прерываний}
  if ProducerInterruptCount <= 0 then
  begin
    ts[1].active := False; { Переключение производителя}
    ts[2].active := True;  {Активация потребителя}
    ProducerInterruptCount := Random(10) + 5; { Новый случайный квант для производителя}
  end;

  if ConsumerInterruptCount <= 0 then
  begin
    ts[2].active := False; { Переключение потребителя}
    ts[1].active := True;  { Активация производителя}
    ConsumerInterruptCount := Random(10) + 5; {Новый случайный квант для потребителя}
  end;
end;
{-$F}

procedure Producer;
var
  num: Integer;
begin
  while true do
  begin
    if ts[1].active then
    begin
      if BufferIndex < BufferSize then
      begin
        asm cli end;
        num := Random(100);
        Buffer[BufferIndex + 1] := num;
        Inc(BufferIndex);
        asm sti end;
        print(num);
      end
      else
      begin
        gotoxy(1,3);
        WriteLn('Buffer is full, Producer is waiting...');
        ts[1].active := False;
        { Проверка на взаимоблокировку: если потребитель}
        { тоже не активен, активируем его }
        if not ts[2].active then
          ts[2].active := True;
      end;
      Delay(ProducerSleepTime);
    end;
  end;
end;

procedure Consumer;
var
  num: Integer;
begin
  while true do
  begin
    if ts[2].active then
    begin
      if BufferIndex > 0 then
      begin
        asm cli end;
        num := Buffer[BufferIndex];
        Dec(BufferIndex);
        asm sti end;
        print(num);
      end
      else
      begin
        gotoxy(1,3);
        WriteLn('Buffer is empty, Consumer is waiting...');
        ts[2].active := False;
        { Проверка на взаимоблокировку: если производитель}
        { тоже не активен, активируем его }
        if not ts[1].active then
          ts[1].active := True;
      end;
      Delay(ConsumerSleepTime);
    end;
  end;
end;

procedure init;
var
  i: integer;
begin
  for i := 1 to 2 do
  begin
    GetMem(Stacks[i], StackSize);
    ts[i].sp := Ofs(Stacks[i]^) + StackSize - 30;
    ts[i].bp := ts[i].sp;
    ts[i].ss := Seg(Stacks[i]^);
    ts[i].ds := Seg(ts);
    ts[i].ss := ts[i].ss;
  end;

  ts[1].cs := Seg(Producer);
  ts[1].ip := Ofs(Producer);
  ts[1].flags := $202;
  ts[1].active := true;

  ts[2].cs := Seg(Consumer);
  ts[2].ip := Ofs(Consumer);
  ts[2].flags := $202;
  ts[2].active := true;

  ThreadsRegistered := 3;
end;

begin
  ClrScr;
  init;

  Randomize;
  BufferIndex := 0;
  preemptiveSwitch := true;
  DisableHardwareEvents := false;
  ParallelStart := true;

  repeat
    code1 := -1;
    code2 := -1;

    if (ParamCount = 2) then
    begin
      Val(ParamStr(1), ProducerSleepTime, code1);
      Val(ParamStr(2), ConsumerSleepTime, code2);
      if ((code1 <> 0) or (code2 <> 0)) then
      begin
        writeln('Wrong params');
        halt;
      end;
    end
    else
    begin
      {$I-}
      Write('Enter producer sleep time (ms): ');
      ReadLn(ProducerSleepTime);
      Write('Enter consumer sleep time (ms): ');
      ReadLn(ConsumerSleepTime);
      {$I+}
      if IOResult <> 0 then
      begin
        writeln('Wrong params, please try again or ESC');
        continue;
      end;
      code1 := 0;
      code2 := 0;
    end;
  until (code1 = 0) or (code2 = 0);

  HideCursor;
  ts[0].active := true;
  GetIntVec($08, @OldTimerVec);
  SetIntVec($08, @TimerHandler);

 {Основной поток программы}
  while true do
  begin
    if KeyPressed then
    begin
      ch := ReadKey;
      {Выход из программы по нажатию Esc}
      if ch = #27 then begin
          break;
        end;
    end;
   end;

  ShowCursor;
  setintvec($8, @OldTimerVec);
end.