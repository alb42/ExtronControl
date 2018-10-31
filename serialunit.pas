unit serialunit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, syncobjs,
  {$ifdef HASAMIGA}
  Exec, AmigaDos, workbench, icon,
  {$endif}
  Math, dateutils;

const

  TIMEOUT = 1000;
  {$ifndef HASAMIGA}
  CMD_NONSTD = 0;
  {$endif}

  {$ifdef AMIGA68k}
  DEFAULTDEVICENAME = 'serial.device';
  DEFAULTUNITNUMBER = 0;
  DEFAULTBAUDRATE = 9600;
  {$else}
  DEFAULTDEVICENAME = 'usbmodem.device';
  DEFAULTUNITNUMBER = 0;
  DEFAULTBAUDRATE = 9600;
  {$endif}
  //
  TERMINATORA = $0a030303;
  TERMINATORB = $03030303;
  //
  SDCMD_QUERY = CMD_NONSTD;     // $09
  SDCMD_BREAK = CMD_NONSTD + 1; // $0A
  SDCMD_SETPARAMS = CMD_NONSTD + 2; // $0B

  SERB_XDISABLED = 7;     // xOn-xOff feature disabled bit
  SERF_XDISABLED = 1 shl 7; // xOn-xOff feature disabled mask
  SERB_EOFMODE = 6;         // EOF mode enabled bit
  SERF_EOFMODE = 1 shl 6;   // EOF mode enabled mask
  SERB_PARTY_ON = 0;      // parity-enabled bit
  SERF_PARTY_ON = 1 shl 0;  // parity-enabled mask

type
  TIOTArray = record
    TermArray0: LongWord;
    TermArray1: LongWord;
  end;

  {$ifdef HASAMIGA}
  TIOExtSer = record
    IOSer: TIOStdReq;
    io_CtlChar: LongWord;    // control characters */
    io_RBufLen: LongWord;    //;    /* length in bytes of serial read buffer */
    io_ExtFlags: LongWord;    //;   /* additional serial flags */
    io_Baud: LongWord;    //;       /* baud rate */
    io_BrkTime: LongWord;    //;    /* duration of break in microseconds */
    io_TermArray: TiOTArray;  //* termination character array */
    io_ReadLen: Byte;    //;    /* number of bits per read character */
    io_WriteLen: Byte;    //;   /* number of bits per write character */
    io_StopBits: Byte;    //;   /* number of stopbits for read */
    io_SerFlags: Byte;    //;   /* serial device flags */
    io_Status: Word;    //;     /* status of serial port and lines */
  end;
  PIOExtSer = ^TIOExtSer;
  {$endif}

  TSerialStuff = class
  private
    InitDone: Boolean;
    DevOpen: Boolean;
    IORunning: Boolean;
    Async: boolean;
    AsyncBuffer: array[0..256] of Char;
    mp: PMsgPort;
    io: PIOExtSer;
    iod: PIORequest;
    procedure SendText(s: string);
    function GetText(): string;
  public
    procedure InitSerial;
    procedure FinishSerial;
    procedure RestartAsync;
    function CheckAsyncMsg: string;

    function SendGetText(s: string): string;
  end;

var
  devicename: string = DEFAULTDEVICENAME;
  unitnumber: Integer = DEFAULTUNITNUMBER;
  Baudrate: Integer = DEFAULTBAUDRATE;

  ST: TSerialStuff;

implementation

procedure DebugOut(Msg: String);
begin
  {$ifdef HASAMIGA}
  sysdebugln(Msg);
  {$else}
  writeln(Msg);
  {$endif}
end;

{$ifdef HASAMIGA}
function CreateExtIO(const Mp: PMsgPort; Size: Integer): PIORequest;
begin
  Result := nil;
  if not Assigned(mp) then
    Exit;
  Result := System.AllocMem(Size);
  if Assigned(Result) then
  begin
    Result^.io_Message.mn_Node.ln_Type := NT_REPLYMSG;
    Result^.io_Message.mn_ReplyPort := Mp;
    Result^.io_Message.mn_Length := Size;
  end;
end;

procedure DeleteExtIO(ioReq: PIORequest);
begin
  if Assigned(ioReq) then
  begin
    ioReq^.io_Message.mn_Node.ln_Type := Byte(-1);
    ioReq^.io_Device := Pointer(-1);
    ioReq^.io_Unit := Pointer(-1);
    System.FreeMem(ioReq);
  end;
end;
{$endif}

procedure TSerialStuff.InitSerial;
var
  Res: Integer;
  t1: LongWord;
begin
  Async := False;
  InitDone := False;
  {$ifdef HASAMIGA}
  mp := nil;
  io := nil;
  iod := nil;
  DevOpen := False;
  IORunning := False;
  //writeln(1);
  mp := CreateMsgPort;
  if not Assigned(Mp) then
  begin
    Writeln('Error open MessagePort');
    Exit;
  end;
  //
  //writeln(2);
  io := PIOExtSer(CreateExtIO(mp, sizeof(TIOExtSer)));
  if not assigned(io) then
  begin
    Writeln('Cannot alloc io');
    Exit;
  end;
  //writeln(3);
  iod := Pointer(io);
  Res := OpenDevice(PChar(DeviceName), UnitNumber, iod,0);
  if Res <> 0 then
  begin
    Writeln('Unable to open device "' + DeviceName + ' ' + IntToStr(UnitNumber) + '" :' +  IntToStr(Res));
    Exit;
  end;
  DevOpen := True;
  //writeln(4);
  io^.io_SerFlags := (io^.io_SerFlags or SERF_EOFMODE or SERF_XDISABLED) and (not SERF_PARTY_ON);
  io^.io_Baud := BaudRate;
  io^.io_TermArray.TermArray0 := TERMINATORA;
  io^.io_TermArray.TermArray1 := TERMINATORB;
  io^.IOSer.io_Command := SDCMD_SETPARAMS;
  //writeln(5);
  SendIO(iod);
  //writeln(6);
  t1 := GetTickCount;
  while CheckIO(iod) = nil do
  begin
    Sleep(10);
    if (GetTickCount - t1) >= TIMEOUT then
    begin
      Break;
    end;
  end;
  //writeln(7);
  if CheckIO(iod) = nil then
  begin
    Res := -1;
    AbortIO(iod);
  end
  else
  begin
    WaitIO(iod);
    Res := iod^.io_Error;
  end;
  //writeln(8);

  //writeln(9);
  if Res <> 0 then
  begin
    Writeln('Error set serial params ' + IntToStr(Res));
    Exit;
  end;
  {$endif}
  InitDone := True;
end;

procedure TSerialStuff.FinishSerial;
begin
  //writeln('finish serial');
  {$ifdef HASAMIGA}
  if DevOpen and Assigned(iod) and (IORunning or Async) then
  begin
    AbortIO(iod);
    WaitIO(iod);
  end;
  if DevOpen then
    CloseDevice(iod);
  if Assigned(io) then
    DeleteExtIO(PIORequest(io));
  if Assigned(Mp) then
    DeleteMsgPort(Mp);
  {$endif}
end;

procedure TSerialStuff.SendText(s: string);
var
  t1: LongWord;
  Buffer: PChar;
begin
  Buffer := PChar(s);
  io^.IOSer.io_Length := Length(s);
  io^.IOSer.io_Data := Buffer;
  io^.IOSer.io_Command := CMD_WRITE;
  IORunning := True;
  SendIO(iod);
  //
  t1 := GetTickCount;
  while CheckIO(iod) = nil do
  begin
    Sleep(10);
    if (GetTickCount - t1) >= TIMEOUT then
      Break;
  end;
  if CheckIO(iod) <> nil then
    WaitIO(iod)
  else
    AbortIO(iod);
  IORunning := False;
end;

function TSerialStuff.GetText(): string;
var
  t1: LongWord;
  Buffer: PChar;
begin
  Buffer := AllocMem(256);
  io^.IOSer.io_Length := 256;
  io^.IOSer.io_Data := Buffer;
  io^.IOSer.io_Command := CMD_READ;
  IORunning := True;
  SendIO(iod);
  //
  t1 := GetTickCount;
  while CheckIO(iod) = nil do
  begin
    Sleep(10);
    if (GetTickCount - t1) >= TIMEOUT then
      Break;
  end;
  if CheckIO(iod) <> nil then
    WaitIO(iod)
  else
    AbortIO(iod);
  IORunning := False;
  Result := Buffer;
  FreeMem(Buffer);
end;

function TSerialStuff.SendGetText(s: string): string;
begin
  if Async then
  begin
    AbortIO(iod);
    WaitIO(iod);
    Async := False;
  end;
  SendText(s);
  Result := GetText;
  RestartAsync;
end;

procedure TSerialStuff.RestartAsync;
begin
  FillChar(AsyncBuffer[0], Length(AsyncBuffer), #0);
  io^.IOSer.io_Length := Length(AsyncBuffer);
  io^.IOSer.io_Data := @AsyncBuffer[0];
  io^.IOSer.io_Command := CMD_READ;
  IORunning := True;
  SendIO(iod);
  Async := True;
end;

function TSerialStuff.CheckAsyncMsg: string;
begin
  if not Async then
  begin
    RestartAsync;
    Exit;
  end;
  if CheckIO(iod) <> nil then
  begin
    WaitIO(iod);
    Result := AsyncBuffer;
    RestartAsync;
  end;
end;

procedure GetSettings();
var
  DObj: PDiskObject;
  s: PChar;
begin
  // Options
	DObj := GetDiskObject(ParamStr(0));
	if Assigned(DObj) then
	begin
		try
			s := FindToolType(DObj^.do_ToolTypes, PChar('DEVICE'));
		  if Assigned(s) then
				DeviceName := s;
		  s := FindToolType(DObj^.do_ToolTypes, PChar('UNIT'));
		  if Assigned(s) then
				UnitNumber := StrToIntDef(s, UnitNumber);
			s := FindToolType(DObj^.do_ToolTypes, PChar('BAUD'));
		  if Assigned(s) then
				BaudRate := StrToIntDef(s, BaudRate);
		finally
		  FreeDiskobject(DObj);
		end;
	end;
		
end;


initialization
  GetSettings();

  ST := TSerialStuff.Create;
  ST.Initserial;
  if not ST.InitDone then
  begin
    ST.FinishSerial;
    ST.Free;
    ST := nil;
  end
  else
  begin
    //writeln('connected');
    if ST.SendGetText('I') = '' then
    begin
      writeln('Extron DSC 301 HD not found.');
      ST.FinishSerial;
      ST.Free;
      ST := nil;
    end;
  end;
finalization
  if Assigned(ST) then
  begin
    ST.FinishSerial;
    ST.Free;
  end;
end.
