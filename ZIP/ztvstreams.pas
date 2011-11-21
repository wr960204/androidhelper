Unit ztvStreams;

Interface

Uses
   Windows,
   Classes,
   SysUtils,
   ztvConsts;

{$WARN SYMBOL_PLATFORM OFF}
Resourcestring
   SReadError = 'Stream read error';
   SWriteError = 'Stream write error';
   SMemoryStreamError = 'Out of memory while expanding memory stream';

Resourcestring
   SRangeError = 'Range check error';
   sSeekNotImplemented = 'Seek not implemented';

Type
   TTempFileMove =
   	Procedure(Sender: TObject; ToFileName, FromFileName: AnsiString;
		SameDrive: Boolean)
      Of Object;

	TTempFileMoveBegin =
   	Procedure(Sender: TObject; Archive, TempFileName: AnsiString;
      Var Cancel: Boolean)
      Of Object;

Type
   TStream32 = Class(TStream)
   Private
      fProgressCallBackProc: TNotifyEvent;
   Protected
      pCancel: pBoolean;
      fOnProgress: TNotifyEvent;
      Procedure Progress(Sender: TObject); Dynamic;
   Public
      CancelCallBackPtr: pBoolean;
   	pProgressPosition: ^Int64;
      TempFileProgressProc: TNotifyEvent;
      TempFileMoveProc: TTempFileMove;
      TempFileMoveBeginProc: TTempFileMoveBegin;
      TempFileMoveEndProc: TNotifyEvent;
      Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      Property OnProgress: TNotifyEvent Read fOnProgress Write fOnProgress;
      Property ProgressCallBackProc: TNotifyEvent Read fProgressCallBackProc Write
         fProgressCallBackProc;
   End;

   THandleStream32 = Class(TStream32)
   Private
   Protected
      fHandle: Integer;
      Function GetSize: Int64;          //Override;
      Procedure SetSize(NewSize: Longint); Overload; Override;
      Procedure SetSize(Const NewSize: Int64); Overload; Override;
   Public
      Constructor Create(AHandle: Integer);
      Destructor Destroy; Override;
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Override;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
      Property Handle: Integer Read fHandle;
   End;

   TFileStream32 = Class(THandleStream32)
   Public
      Constructor Create(Const FileName: String; Mode: Word);
      Destructor Destroy; Override;
   End;

   TCustomMemoryStream32 = Class(TStream32)
   Private
      fMemory: Pointer;
      fSize, fPosition: Longint;
   Protected
      Procedure SetPointer(Ptr: Pointer; Size: Longint);
   Public
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Override;
      Procedure SaveToStream(Stream: TStream32);
      Procedure SaveToFile(Const FileName: String);
      Property Memory: Pointer Read fMemory;
   End;

   TMemoryStream32 = Class(TCustomMemoryStream32)
   Private
      fCapacity: Longint;
      Procedure SetCapacity(NewCapacity: Longint);
   Protected
      Function Realloc(Var NewCapacity: Longint): Pointer; Virtual;
      Property Capacity: Longint Read fCapacity Write SetCapacity;
   Public
      Destructor Destroy; Override;
      Procedure Clear;
      Procedure LoadFromStream(Stream: TStream32);
      Procedure LoadFromFile(Const FileName: String);
      Procedure SetSize(NewSize: Longint); Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
   End;

Const
   WSIZE = 32768; (* window size--must be a power of two, and at least 32k *)
   CUSTOM_BUF_SIZE = High(Word) * 2;    //WSIZE * 2; //v6.3.2 revised

   {b and mask_bits[i] gets lower i bits out of i}
   mask_bits: Array[0..16] Of Word =
      ($0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F,
      $00FF, $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF,
      $FFFF);

Type
   TCustomStream = Class(TStream32)
   Private
      BSize: Byte;
   Protected
      fProgressPosition: Double;
      // v5.2 revised to allocate/deallocate the required memory only when needed
      //fBuffer: Array[Word]{[0..WSIZE-1]} Of Char;
      fBuffer: Pointer;
   Public
      fStrm: TStream32;
      fStrmPos: Int64;
      FZRec: ztv_stream;
      Constructor Create(strm: TStream32);
      Destructor Destroy; Override;
   End;

   TCompressStream = Class(TCustomStream)
   Private
   	Function GetCrcValue: u_long;
   	Function GetCompressionRate: Integer;
   Public
      Constructor Create(dest: TStream32; Level: TDeflateType; MAX_WBITS: ShortInt);
      Destructor Destroy; Override;
      Procedure doInitialize(dest: TStream32; Level: TDeflateType; MAX_WBITS:
      	ShortInt); Virtual;
      Procedure doCompStream; Virtual;
      Procedure doCompEnd; Virtual;
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Override;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
    	Property CompressionRate: Integer Read GetCompressionRate;
      Property Crc: u_long Read GetCrcValue;
   End;

   TDecompressStream = Class(TCustomStream)
   Private
      Procedure doInflateReset(Var z: ztv_stream); Virtual;
      Procedure doDecompEnd(Var z: ztv_stream); Virtual;
      Procedure doInitialize(z: ztv_streamp; stream_size: _int;
         DEF_WBITS: ShortInt); Virtual;
   Protected
   Public
      Constructor Create(Source: TStream32; DEF_WBITS: ShortInt);
      Destructor Destroy; Override;
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Override;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
      Property OnProgress;
   End;

   TCalcStreamCRC = Class(TCustomStream)
   Private
   Public
      CRC: u_long;
      Constructor Create(inStream: TStream32; Len: Int64; BitSize: Byte;
         zsp: ztv_stream_plus; Cancel_CallBackProc: Pointer;
         Progress_CallBackProc: TNotifyEvent);
      Destructor Destroy; Override;
      Function Write(Const Buf; iCount: Longint): Longint; Override;
      Function Read(Var Buffer; iCount: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Override;
   End;

   TEncryptStream = Class(TMemoryStream32)
   Private
      fStrm: TStream32;
   Public
      CRC: u_long;
      CryptHDR: String[RAND_HEAD_LEN];
      Constructor Create(Var inStream, dest: TStream32; Len: Int64;
         Password: String; WriteHeader: Boolean);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
   End;

   TDecryptStream = Class(TMemoryStream32)
   Private
      fPassword: String;
      fStrm: TStream32;
   Public
      Constructor Create(Var inStream, dest: TStream32; Password: String);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
   End;

   TDecryptStreamCRC = Class(TMemoryStream32)
   Private
      fPassword: String;
      fCryptHDR: String[RAND_HEAD_LEN * 2];
      fHeadRead: Boolean;
      fStrm: TStream32;
   Public
      Constructor Create(Var inStream, dest: TStream32; Password: String;
         CRC: u_long);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
   End;

   TStoreStream = Class(TCustomStream)
   Private
      pInStream: ^TStream32;
      Encrypted: Boolean;
   Public
   	CancelCallBackPtr: Pointer;
      ProgressCallBackProc: TNotifyEvent;
      Constructor Create(dest: TStream32; BitSize: Byte; zsp: ztv_stream_plus);
      Destructor Destroy; Override;
      Function Read(Var Buffer; iCount: Longint): Longint; Override;
      Function Write(Const Buffer; iCount: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Override;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
      Function CopyStream(inStream: TStream32): Int64;
      Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      Property OnProgress;
   End;

Function _adler(a: u_long; Buf: _pBytef; Len: uInt): u_long;
Function CalcFileCRC(f: THandle; Len: Int64; zsp: ztv_stream_plus; CancelCallBackPtr:
   Pointer; ProgressCallBackProc: TNotifyEvent): u_long;
Function CalcStreamCRC16(strm16: TStream32; Len: Int64; zsp:
   ztv_stream_plus): u_long;
Function CalcStreamCRC32(strm32: TStream32; Len: Int64; zsp:
   ztv_stream_plus; CancelCallBackPtr: Pointer; ProgressCallBackProc:
   TNotifyEvent): u_long;

Procedure ztvFreeMem(AppData, Block: Pointer);
Procedure TStreamToztvStream(Source: TStream; dest: TStream32);
Procedure ztvStreamToTStream(Source: TStream32; dest: TStream; Len: Int64);
Function ztvAllocMem(AppData: Pointer; Items, Size: u_long): Pointer;



Implementation

Uses
   Dialogs,
   Forms,
   Consts,
   TypInfo,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvCrypt,
   ztvDeflate,
   ztvInflate,
   ztvErrMsgs;

Const
   MaxBufSize = $20000; //$F000;

   //-------------------------------------------------------------

Constructor THandleStream32.Create(AHandle: Integer);
Begin
   fHandle := AHandle;
   CancelCallBackPtr := Nil;
   ProgressCallBackProc := Nil;
End;
//-------------------------------------------------------------

Destructor THandleStream32.Destroy;
Begin
   fHandle := -1;
End;
//-------------------------------------------------------------

Function THandleStream32.Read(Var Buffer; Count: Longint): Longint;
Begin
   Result := FileRead(fHandle, Buffer, Count);
   If Result = -1 Then
      Result := 0;

   If Result <> 0 Then
   Begin
      If Assigned(TempFileProgressProc) Then
      Begin
      	pProgressPosition^ := pProgressPosition^ - Count;
      	TempFileProgressProc(Self);
      End;
   End;
End;
//-------------------------------------------------------------

Function THandleStream32.Write(Const Buffer; Count: Longint): Longint;
Begin
   If Not WriteFile(THandle(fHandle), Buffer, Count, LongWord(Result), Nil) Then
      Result := 0;
End;
//-------------------------------------------------------------

Function THandleStream32.GetSize: Int64;
Var
   dwSizeHigh: DWord;
Begin
   Result := Windows.GetFileSize(fHandle, @dwSizeHigh);
   If (Result = $FFFFFFFF) And (GetLastError <> NO_ERROR) Then
      Result := 0
   Else
      Result := (Int64(dwSizeHigh) Shl 32) Or Result;

End;
//-------------------------------------------------------------

Function THandleStream32.Seek(Offset: Integer; Origin: Word): Integer;
Begin
   Result := FileSeek(fHandle, Offset, Origin);
End;
//-------------------------------------------------------------

// Note: Use FileSeek instead of SetFilePointer unless the value of dwSizeHigh
// is known.    FileSeek effienciently handles 4gig+ files.

Function THandleStream32.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
   Result := FileSeek(fHandle, Offset, Ord(Origin));
End;
//-------------------------------------------------------------

Procedure THandleStream32.SetSize(NewSize: Longint);
Begin
   SetSize(Int64(NewSize));
End;
//-------------------------------------------------------------

Procedure THandleStream32.SetSize(Const NewSize: Int64);
Begin
   Seek(NewSize, soFromBeginning);
   Win32Check(SetEndOfFile(fHandle));
End;
//-------------------------------------------------------------

{ TFileStream32 }
Constructor TFileStream32.Create(Const FileName: String; Mode: Word);
Const
   fOpenErr = -1;
   fCreateErr = -2;
Begin
   If Mode = fmCreate Then
   Begin
      //fHandle := FileCreate(FileName);

      Inherited Create(FileCreate(FileName));
      //rem'd v4.6.1
      //If fHandle < 0 Then
      //   {$ifndef DEL5_OR_HIGHER}
      //   Raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
      //   {$else}
      //   Raise EFCreateError.CreateResFmt(@SFCreateError, [FileName]);
      //   {$endif}
   End Else Begin
      //fHandle := FileOpen(FileName, Mode);
      Inherited Create(FileOpen(FileName, Mode));
      //rem'd v4.6.1
      //If fHandle < 0 Then
      //   {$ifndef DEL5_OR_HIGHER}
      //   Raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
      //   {$else}
      //   Raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
      //   {$endif}
   End;
End;
//-------------------------------------------------------------

Destructor TFileStream32.Destroy;
Begin
   If Handle >= 0 Then
      If CloseHandle(THandle(fHandle)) Then
         fHandle := -1;
   //Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TStreamToztvStream(Source: TStream; dest: TStream32);
Var
   Buffer: Pchar;
   Count: Int64;
   BufSize, n: Integer;
Begin
   Count := Source.Size - Source.Position;
   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   //Source.Position := 0;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) Do             // And (Not pCancel^)
      Begin
         If Count > BufSize Then
            n := BufSize
         Else
            n := Count;

         //Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         dest.WriteBuffer(Buffer^, n);

         dec(Count, n);
      End;
   Finally
      dest.Position := 0;
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

// convert TStream32 to a Delphi compatible TStream
Procedure ztvStreamToTStream(Source: TStream32; dest: TStream; Len: Int64);
Var
   Buffer: Pchar;
   BufSize,
      n: Integer;
Begin
   If Len > (Source.Size - Source.Position) Then
      Len := Source.Size - Source.Position;

   If Len > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Len;

   GetMem(Buffer, BufSize);
   Try
      While (Len <> 0) Do               // And (Not pCancel^)
      Begin
         If Len > BufSize Then
            n := BufSize
         Else
            n := Len;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         dest.WriteBuffer(Buffer^, n);
         dec(Len, n);
      End;
   Finally
      dest.Position := 0;
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

Function _adler(a: u_long; Buf: _pBytef; Len: uInt): u_long;
Var
   k: _int;
   s1, s2: u_long;
Begin
   If Not Assigned(Buf) Then
   Begin
      Result := u_long(1);
      exit;
   End;

   s1 := a And $FFFF;
   s2 := (a Shr 16) And $FFFF;

   While (Len > 0) Do
   Begin
      If Len < NMAX Then
         k := Len
      Else
         k := NMAX;

      dec(Len, k);

      While (k > 0) Do
      Begin
         Inc(s1, Buf^);
         Inc(s2, s1);
         Inc(Buf);
         dec(k);
      End;

      s1 := s1 Mod BASE;
      s2 := s2 Mod BASE;
   End;
   Result := (s2 Shl 16) Or s1;
End;
//-------------------------------------------------------------

Function ztvAllocMem(AppData: Pointer; Items, Size: u_long): Pointer;
Begin
   Result := AllocMem(Items * Size);    //GetMem(Result, Items * Size);
End;
//-------------------------------------------------------------

Procedure ztvFreeMem(AppData, Block: Pointer);
Begin
   FreeMem(Block);
End;
//-------------------------------------------------------------

Procedure TStream32.Progress(Sender: TObject);
Begin
   If Assigned(TStream32(Sender).OnProgress) Then
      TStream32(Sender).OnProgress(Sender);
End;
//-------------------------------------------------------------

Function TStream32.CopyFrom(Source: TStream32; Count: Int64): Int64;
Var
   Buffer: Pchar;
   //pCancel: pBoolean;
   TempCancel: Boolean;
   BufSize, n: Integer;
Begin
   If Count = 0 Then
   Begin
      Source.Position := 0;
      Count := Source.Size;
   End;

   Result := Count;
   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   // if pCancel = true, then assign the stream objects pCancel variable to
   // the current objects cancel variable to allow a process cancelation.
   pCancel := CancelCallBackPtr;
   If pCancel = Nil Then
   Begin
      TempCancel := False;
      pCancel := @TempCancel;
   End;

   OnProgress := ProgressCallBackProc;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) And (Not pCancel^) Do
      Begin
         If Count > BufSize Then
            n := BufSize
         Else
            n := Count;

         Application.ProcessMessages();
         If pCancel^ Then
            Break;

         Source.ReadBuffer(Buffer^, n); //Source.Read(Buffer^, n);
         Try
            WriteBuffer(Buffer^, n);    //Write(Buffer^, n);
         Except
            Raise;
         End;
         dec(Count, n);
      End;
   Finally
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

{ TCustomMemoryStream32 }
Procedure TCustomMemoryStream32.SetPointer(Ptr: Pointer; Size: Longint);
Begin
   fMemory := Ptr;
   fSize := Size;
End;
//-------------------------------------------------------------

Function TCustomMemoryStream32.Read(Var Buffer; Count: Longint): Longint;
Begin
   If (fPosition >= 0) And (Count >= 0) Then
   Begin
      Result := fSize - fPosition;
      If Result > 0 Then
      Begin
         If Result > Count Then
            Result := Count;
         Move(Pointer(Longint(fMemory) + fPosition)^, Buffer, Result);
         Inc(fPosition, Result);
         exit;
      End;
   End;
   Result := 0;
End;
//-------------------------------------------------------------

Function TCustomMemoryStream32.Seek(Offset: Longint; Origin: Word): Longint;
Begin
   Case Origin Of
      soFromBeginning: fPosition := Offset;
      soFromCurrent: Inc(fPosition, Offset);
      soFromEnd: fPosition := fSize + Offset;
   End;
   Result := fPosition;
End;
//-------------------------------------------------------------

Procedure TCustomMemoryStream32.SaveToStream;
Begin
   If fSize <> 0 Then
      Stream.WriteBuffer(fMemory^, fSize);
End;
//-------------------------------------------------------------

Procedure TCustomMemoryStream32.SaveToFile(Const FileName: String);
Var
   strm: TFileStream32;
Begin
   strm := TFileStream32.Create(FileName, fmCreate);
   Try
      If (strm.Handle < 0) Then
         exit;
      SaveToStream(strm);
   Finally
      strm.Free();
   End;
End;
//-------------------------------------------------------------

{ TMemoryStream32 }
Const
   MemoryDelta = $2000;                 { Must be a power of 2 }

Destructor TMemoryStream32.Destroy;
Begin
   Clear();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.Clear;
Begin
   SetCapacity(0);
   fSize := 0;
   fPosition := 0;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.LoadFromStream;
Var
   Count: Longint;
Begin
   Stream.Position := 0;
   Count := Stream.Size;
   SetSize(Count);
   If Count <> 0 Then
      Stream.ReadBuffer(fMemory^, Count);
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.LoadFromFile(Const FileName: String);
Var
   strm: TFileStream32;
Begin
   strm := TFileStream32.Create(FileName, fmOpenRead Or fmShareDenyWrite);
   Try
      If (strm.Handle < 0) Then
         exit;
      LoadFromStream(strm);
   Finally
      strm.Free();
   End;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.SetCapacity(NewCapacity: Longint);
Begin
   SetPointer(Realloc(NewCapacity), fSize);
   fCapacity := NewCapacity;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.SetSize(NewSize: Longint);
Var
   OldPosition: Longint;
Begin
   OldPosition := fPosition;
   SetCapacity(NewSize);
   fSize := NewSize;
   If OldPosition > NewSize Then
      Seek(0, soFromEnd);
End;
//-------------------------------------------------------------

Function TMemoryStream32.Realloc(Var NewCapacity: Longint): Pointer;
Begin
   If (NewCapacity > 0) And (NewCapacity <> fSize) Then
      NewCapacity := (NewCapacity + (MemoryDelta - 1)) And Not (MemoryDelta - 1);
   Result := Memory;
   If NewCapacity <> fCapacity Then
   Begin
      If NewCapacity = 0 Then
      Begin
         GlobalFreePtr(Memory);
         Result := Nil;
      End
      Else
      Begin
         If Capacity = 0 Then
            Result := GlobalAllocPtr(HeapAllocFlags, NewCapacity)
         Else
            Result := GlobalReallocPtr(Memory, NewCapacity, HeapAllocFlags);
         If Result = Nil Then
            Raise EStreamError.CreateRes(@SMemoryStreamError);
      End;
   End;
End;
//-------------------------------------------------------------

Function TMemoryStream32.Write(Const Buffer; Count: Longint): Longint;
Var
   Pos: Longint;
Begin
   If (fPosition >= 0) And (Count >= 0) Then
   Begin
      Pos := fPosition + Count;
      If Pos > 0 Then
      Begin
         If Pos > fSize Then
         Begin
            If Pos > fCapacity Then
               SetCapacity(Pos);
            fSize := Pos;
         End;
         System.Move(Buffer, Pointer(Longint(fMemory) + fPosition)^, Count);
         fPosition := Pos;
         Result := Count;
         exit;
      End;
   End;
   Result := 0;
End;
//-------------------------------------------------------------
{.$endif}

(*************************************************************)
(*************************************************************)
(*                      TCustomStream                        *)
(*************************************************************)
(*************************************************************)

Constructor TCustomStream.Create(strm: TStream32);
Begin
   Inherited Create;
   fStrm := strm;
   fStrmPos := strm.Position;
   FZRec.ZALLOC := ztvAllocMem;
   FZRec.ZFREE := ztvFreeMem;
   GetMem(fBuffer, CUSTOM_BUF_SIZE);
End;
//-------------------------------------------------------------

Destructor TCustomStream.Destroy;
Begin
   FreeMem(fBuffer, CUSTOM_BUF_SIZE);
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TCompressStream                      *)
(*************************************************************)
(*************************************************************)

Constructor TCompressStream.Create(dest: TStream32; Level: TDeflateType;
   MAX_WBITS: ShortInt);
Var
	bCancel: Boolean;
Begin
   Inherited Create(dest);
   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
   FZRec.cb.CRC := CRC_MASK;
   FZRec.cb.pCancel := @bCancel;
   doInitialize(dest, Level, MAX_WBITS);
End;
//-------------------------------------------------------------

Destructor TCompressStream.Destroy;
Begin
   FZRec.next_in := Nil;
   FZRec.avail_in := 0;
   doCompEnd();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TCompressStream.GetCrcValue: u_long;
Begin
	Result := FZRec.cb.crc;
End;
//-------------------------------------------------------------

Function TCompressStream.GetCompressionRate: Integer;
Begin
	Result := CalcRatio(FZRec.total_out, FZRec.total_in);
End;
//-------------------------------------------------------------

Function TCompressStream.Read(Var Buffer; Count: Longint): Longint;
Begin
   //ShowMessage( 'Error: InvalidStreamOp' );
   Result := -1;
End;
//-------------------------------------------------------------

Function TCompressStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
   { Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
     Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
      Raise ERangeError.CreateRes(@SRangeError);

   //Result := Seek(Longint(Offset), Ord(Origin));
   If (Offset = 0) And (Origin = soCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
   //ShowMessage( 'Error: sInvalidStreamOp' );

End;
//-------------------------------------------------------------

Function TCompressStream.Seek(Offset: Longint; Origin: Word): Longint;
Begin
   If (Offset = 0) And (Origin = soFromCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
End;
//-------------------------------------------------------------

Function TCompressStream.Write(Const Buffer; Count: Longint): Longint;
Begin
   FZRec.next_in := @Buffer;
   FZRec.avail_in := Count;
   If (fStrm.Position <> fStrmPos) Then
      fStrm.Position := fStrmPos;

   doCompStream();
   Result := Count;
End;
//-------------------------------------------------------------

Procedure TCompressStream.doCompStream;
Begin
   While (FZRec.avail_in > 0) Do
   Begin

      If FZRec.cb.pCancel^ Then
         Break;

      If Assigned(FZRec.cb.pArchivePos) Then
      	FZRec.cb.pArchivePos^ :=
         	FZRec.cb.pArchivePos^ - FZRec.avail_in;

      //CCheck(_deflate(FZRec, 0));
      _deflate(FZRec, 0);               // v4.1.7

      If (FZRec.avail_out = 0) Then
      Begin
      	If Assigned(FZRec.cb.pArchivePos) Then
         	FZRec.cb.pArchivePos^ :=
            	FZRec.cb.pArchivePos^ + FZRec.avail_in;

         If FZRec.cb.Protect Then
            ztvEncodeBuf(fBuffer, CUSTOM_BUF_SIZE);

         fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE);

         FZRec.next_out := fBuffer;
         FZRec.avail_out := CUSTOM_BUF_SIZE;
         fStrmPos := fStrm.Position;
      End;

      Progress(Self);
   End;
End;
//-------------------------------------------------------------

Procedure TCompressStream.doInitialize(dest: TStream32; Level: TDeflateType;
   MAX_WBITS: ShortInt);
Begin
   //CCheck(deflateInit2(FZRec, ZLevels[Level], Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL, 0));
   deflateInit2(FZRec, ZLevels[Level], Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL,
      0);                                  // v4.1.7
End;
//-------------------------------------------------------------

Procedure TCompressStream.doCompEnd;
Var
   code: _int;
Begin
   Try
      If fStrm.Position <> fStrmPos Then
         fStrm.Position := fStrmPos;

      code := _deflate(FZRec, Z_FINISH);

      //While (CCheck(code) <> Z_STREAM_END)
      While (code <> Z_STREAM_END)      // v 4.1.7
      And (FZRec.avail_out = 0) And (Not FZRec.cb.pCancel^) Do
      Begin

         If (code = Z_FINISH) Or (code = Z_OK) Then
         Begin
            If FZRec.cb.Protect Then
               ztvEncodeBuf(fBuffer, CUSTOM_BUF_SIZE);

            fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE);

            FZRec.next_out := fBuffer;
            FZRec.avail_out := CUSTOM_BUF_SIZE;
         End
         Else
            exit;

         code := _deflate(FZRec, Z_FINISH);
      End;

      If (FZRec.avail_out < CUSTOM_BUF_SIZE) Then
      Begin
         If FZRec.cb.Protect Then
            ztvEncodeBuf(fBuffer, CUSTOM_BUF_SIZE - FZRec.avail_out);

         fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE - FZRec.avail_out);

      End;
   Finally
      _deflateEnd(FZRec);
   End;
End;
//-------------------------------------------------------------

(*************************************************************)
(*************************************************************)
(*                      TDecompressStream                    *)
(*************************************************************)
(*************************************************************)

Constructor TDecompressStream.Create(Source: TStream32; DEF_WBITS: ShortInt);
Begin
   Inherited Create(Source);
   FZRec.next_in := fBuffer;
   FZRec.avail_in := 0;
   doInitialize(@FZRec, SizeOf(FZRec), DEF_WBITS);
End;
//-------------------------------------------------------------

Destructor TDecompressStream.Destroy;
Begin
   fStrm.Seek(-FZRec.avail_in, 1);
   doDecompEnd(FZRec);
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TDecompressStream.Read(Var Buffer; Count: Longint): Longint;
Var
   r: _int;
Begin
   FZRec.next_out := @Buffer;
   FZRec.avail_out := Count;

   If fStrm.Position <> fStrmPos Then
      fStrm.Position := fStrmPos;

   While (FZRec.avail_out > 0) Do
   Begin
      If FZRec.avail_in = 0 Then
      Begin
         FZRec.avail_in := fStrm.Read(fBuffer^, Count);

         // ------------------------------------------------------------
         // DO NOT USE THE FOLLOWING BLOCK!  If an archive's Eof marker
         // immediately follows the compressed data, this function fails
         // using this block check.
         // ------------------------------------------------------------
         //If FZRec.avail_in = 0 Then
         //Begin
         //   Result := Count - LongInt( FZRec.avail_out );
         //   Exit;
         //End;
         // ------------------------------------------------------------

         FZRec.next_in := fBuffer;
         fStrmPos := fStrm.Position;

         If (FZRec.avail_in > 0) Then
         Begin
            If FZRec.cb.Protect Then
               ztvDecodeBuf(FZRec.next_in, FZRec.avail_in);
            Progress(Self);
         End;
      End;

   	r := _inflate(FZRec, 0);        // v4.1.7

      If r = Z_STREAM_END Then          // 051601: added for compatibility with TZipKey
      Begin
         FZRec.cb.IsEof := True;
         Break
      End
      Else
         If r < 0 Then // ( r = Z_DATA_ERROR ) Or ( r = Z_STREAM_ERROR ) Then
         Begin
            Count := r;
            Break;
         End;
   End;
   Result := Count;
End;
//-------------------------------------------------------------

Function TDecompressStream.Write(Const Buffer; Count: Longint): Longint;
Begin
   //Raise EDecompressionError.Create( sInvalidStreamOp );
   Result := -1;
End;
//-------------------------------------------------------------

Function TDecompressStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Var
   i: Integer;
   Buf: Array[0..4095] Of Char;
   LocalOffset: Int64;
Begin

   LocalOffset := Offset;
   { Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
     Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
      Raise ERangeError.CreateRes(@SRangeError);

   //Result := Seek(Longint(Offset), Ord(Origin));
   If (LocalOffset = 0) And (Origin = soBeginning) Then
   Begin
      doInflateReset(FZRec);
      FZRec.next_in := fBuffer;
      FZRec.avail_in := 0;
      fStrm.Position := 0;
      fStrmPos := 0;
   End
   Else
      If ((LocalOffset >= 0) And (Origin = soCurrent)) Or
         (((LocalOffset - Longint(FZRec.total_out)) > 0) And (Origin = soBeginning)) Then
      Begin
         If Origin = soBeginning Then
            LocalOffset := LocalOffset - FZRec.total_out;

         If LocalOffset > 0 Then
         Begin
            For i := 1 To LocalOffset Div SizeOf(Buf) Do
               ReadBuffer(Buf, SizeOf(Buf));

            ReadBuffer(Buf, LocalOffset Mod SizeOf(Buf));
         End;
      End
      Else
         ; //Raise EDecompressionError.Create( sInvalidStreamOp );

   Result := FZRec.total_out;
End;
//-------------------------------------------------------------

Function TDecompressStream.Seek(Offset: Longint; Origin: Word): Longint;
Var
   i: Integer;
   Buf: Array[0..4095] Of Char;
Begin
   If (Offset = 0) And (Origin = soFromBeginning) Then
   Begin
      doInflateReset(FZRec);
      FZRec.next_in := fBuffer;
      FZRec.avail_in := 0;
      fStrm.Position := 0;
      fStrmPos := 0;
   End
   Else
      If ((Offset >= 0) And (Origin = soFromCurrent)) Or
      (((Offset - Longint(FZRec.total_out)) > 0) And (Origin = soFromBeginning)) Then
      Begin
         If Origin = soFromBeginning Then
            dec(Offset, FZRec.total_out);
         If Offset > 0 Then
         Begin
            For i := 1 To Offset Div SizeOf(Buf) Do
               ReadBuffer(Buf, SizeOf(Buf));

            ReadBuffer(Buf, Offset Mod SizeOf(Buf));
         End;
      End
      Else
         ; //Raise EDecompressionError.Create( sInvalidStreamOp );

   Result := FZRec.total_out;
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doInitialize(z: ztv_streamp; stream_size: _int;
   DEF_WBITS: ShortInt);
Begin
   //inflateInit_(@FZRec, DEF_WBITS, SizeOf(FZRec)); // v4.1.7
   {CCheck(}inflateInit_(z, DEF_WBITS, SizeOf(FZRec)); // v4.1.7
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doDecompEnd(Var z: ztv_stream);
Begin
   inflateEnd(z);
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doInflateReset(Var z: ztv_stream);
Begin
   //CCheck(inflateReset(z));
   inflateReset(z);                     // v4.1.7
End;

(*************************************************************)
(*************************************************************)
(*                        TStoreStream                       *)
(*************************************************************)
(*************************************************************)

Destructor TStoreStream.Destroy;
Begin
   FZRec.next_in := Nil;
   FZRec.avail_in := 0;
   Inherited Destroy;
End;
//-------------------------------------------------------------

Constructor TStoreStream.Create(dest: TStream32; BitSize: Byte; zsp: ztv_stream_plus);
Begin
   Inherited Create(dest);
   fStrm := dest;
   BSize := BitSize;

   FZRec.cb := zsp;
   Encrypted := zsp.Protect;

   If (Not FZRec.cb.Protect) Then
   Begin
      Case BSize Of
         16: FZRec.cb.CRC := 0;
         32: FZRec.cb.CRC := CRC_MASK;
      End;
   End
   Else
      Case BSize Of
         16: FZRec.cb.CRC := Crc16Val;
         32: FZRec.cb.CRC := Crc32Val;
      End;

   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
End;
//-------------------------------------------------------------

Function TStoreStream.Read(Var Buffer; iCount: Longint): Longint;
Begin
   Result := -1;                        //ShowMessage( 'Error: InvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TStoreStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
   { Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
     Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
      Raise ERangeError.CreateRes(@SRangeError);
   Result := Seek(Longint(Offset), Ord(Origin));
End;
//-------------------------------------------------------------

Function TStoreStream.Seek(Offset: Longint; Origin: Word): Longint;
Begin
   If (Offset = 0) And (Origin = soFromCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
   //ShowMessage( 'Error: sInvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TStoreStream.Write(Const Buffer; iCount: Longint): Longint;
Begin
   FZRec.next_in := @Buffer;
   FZRec.avail_in := iCount;
   //If (fStrm.Position <> fStrmPos) Then
   //   fStrm.Position := fStrmPos;

   If (Not FZRec.cb.Protect) Then
      Case BSize Of
         16: Crc16_buf(@Buffer, iCount, FZRec.cb.CRC);
         32: Crc32_buf(@Buffer, iCount, FZRec.cb.CRC);
      End;

   If Encrypted Then
      ztvEncodeBuf(@Buffer, iCount);

   If Assigned(FZRec.cb.pArchivePos) Then
   	FZRec.cb.pArchivePos^ :=
      	FZRec.cb.pArchivePos^ - FZRec.avail_in;
      
   fStrm.WriteBuffer(Buffer, iCount);

   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
   //fStrmPos := fStrm.Position;

   Progress(pInStream^);
   Result := iCount;
End;
//-------------------------------------------------------------

Function TStoreStream.CopyFrom(Source: TStream32; Count: Int64): Int64;
Const
   MaxBufSize = $19000;                 //$186A0; //$F000;
Var
   TempCancel: Boolean;
   BufSize{, n}: Integer;
   Buffer: Pchar;
Begin
   If Count = 0 Then
   Begin
      Source.Position := 0;
      Count := Source.Size;
   End;

   Result := Count;

   // if pCancel = true, then assign the stream objects pCancel variable to
   // the current objects cancel variable to allow a process cancelation.
   pCancel := CancelCallBackPtr;
   If pCancel = Nil Then
   Begin
      TempCancel := False;
      pCancel := @TempCancel;
   End;

   pInStream^.OnProgress := ProgressCallBackProc;

   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) And (Not pCancel^) Do
      Begin
         Application.ProcessMessages();
         Source.Read(Buffer^, BufSize);
         Try
            Write(Buffer^, BufSize);
         Except
            Raise;
         End;

         dec(Count, BufSize);

         If Count > MaxBufSize Then
            BufSize := MaxBufSize
         Else
            BufSize := Count;

      End;
   Finally
      FreeMem(Buffer);
   End;
End;
//-------------------------------------------------------------

Function TStoreStream.CopyStream(inStream: TStream32): Int64;
Begin
   inStream.CancelCallBackPtr := CancelCallBackPtr;
   inStream.ProgressCallBackProc := ProgressCallBackProc;
   pInStream := @inStream;
   Result := CopyFrom(inStream, 0);
End;

(*************************************************************)
(*************************************************************)
(*                      TEncryptStream                       *)
(*************************************************************)
(*************************************************************)

Constructor TEncryptStream.Create(Var inStream, dest: TStream32;
   Len: Int64; Password: String; WriteHeader: Boolean);
Var
   LocalCancel: Boolean;
   zsp: ztv_stream_plus;
Begin
   fStrm := dest;
   LocalCancel := False;

   //zsp.Protect := False;
   //zsp.CRC := Crc32Val;
   //zsp.pCancel := pCancel; //@fCancel;
   //zsp.pArchivePos := Nil; //@ProgressPosition;

   CRC := CalcStreamCRC32(inStream, Len, zsp, Nil, Nil) Xor CRC_MASK;
   CryptHDR := ztvEncryptHead(Password, 0, CRC);
   If WriteHeader Then
      fStrm.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);

   CancelCallBackPtr := @LocalCancel;
   ProgressCallBackProc := Nil;
   CopyFrom(inStream, 0);
End;
//-------------------------------------------------------------

Function TEncryptStream.Write(Const Buffer; Count: Longint): Longint;
Begin
   ztvEncodeBuf(@Buffer, Count);
   fStrm.WriteBuffer(Buffer, Count);
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TEncryptStream.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TDecryptStreamCRC                       *)
(*************************************************************)
(*************************************************************)

// must be called with encrypted crc value!

Constructor TDecryptStreamCRC.Create(Var inStream, dest: TStream32;
   Password: String; CRC: u_long);
Begin
   //If (Password = '') Then
   //   Exit;

   fHeadRead := False;
   fPassword := Password;
   fStrm := dest;

   CancelCallBackPtr := Nil;
   ProgressCallBackProc := Nil;

   If CopyFrom(inStream, RAND_HEAD_LEN) = RAND_HEAD_LEN Then
   Begin
      // make a working copy
      CopyMem(@fCryptHDR[1], @fCryptHDR[RAND_HEAD_LEN + 1], RAND_HEAD_LEN);

      If decrypt_pw(@fCryptHDR[1], {RAND_HEAD_LEN,} 0, CRC, 0, fPassword) Then
         CopyFrom(inStream, inStream.Size - RAND_HEAD_LEN);
   End;
End;
//-------------------------------------------------------------

Function TDecryptStreamCRC.Write(Const Buffer; Count: Longint): Longint;
Begin
   If Not fHeadRead Then
   Begin
      fHeadRead := True;
      CopyMem(@Buffer, @fCryptHDR[1], Count);
   End
   Else
   Begin
      ztvDecodeBuf(@Buffer, Count);
      fStrm.WriteBuffer(Buffer, Count);
   End;
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TDecryptStreamCRC.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TDecryptStreamPW                     *)
(*************************************************************)
(*************************************************************)

// must be called with password!

Constructor TDecryptStream.Create(Var inStream, dest: TStream32; Password: String);
Begin
   If (Password = '') Then
      exit;

   fPassword := Password;
   fStrm := dest;

   If seed_keys(Password) = 0 Then
   Begin
      CancelCallBackPtr := Nil;
      ProgressCallBackProc := Nil;
      CopyFrom(inStream, inStream.Size);
   End;
End;
//-------------------------------------------------------------

Function TDecryptStream.Write(Const Buffer; Count: Longint): Longint;
Begin
   ztvDecodeBuf(@Buffer, Count);
   fStrm.WriteBuffer(Buffer, Count);
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TDecryptStream.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TCalcStreamCRC                    *)
(*************************************************************)
(*************************************************************)

Constructor TCalcStreamCRC.Create(inStream: TStream32; Len: Int64; BitSize:
   Byte; zsp: ztv_stream_plus; Cancel_CallBackProc: Pointer;
   Progress_CallBackProc: TNotifyEvent);
Begin
   BSize := BitSize;
   fStrm := inStream;
   FZRec.cb := zsp;
   pCancel := CancelCallBackPtr;
   fStrm.OnProgress := ProgressCallBackProc;

   Case BSize Of
      16: CRC := 0;
      32: CRC := CRC_MASK;
   End;

   CancelCallBackPtr := Cancel_CallBackProc;
   ProgressCallBackProc := Progress_CallBackProc;
   CopyFrom(inStream, Len);
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Read(Var Buffer; iCount: Longint): Longint;
Begin
   Result := -1;                        //ShowMessage( 'Error: InvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Seek(Offset: Longint; Origin: Word): Longint;
Begin
   Result := -1;                        //ShowMessage( 'Error: sInvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Write(Const Buf; iCount: Longint): Longint;
Begin
   FZRec.avail_in := iCount;

   Case BSize Of
      16: Crc16_buf(@Buf, iCount, CRC);
      32: Crc32_buf(@Buf, iCount, CRC);
   End;

   //If (FZRec.cb <> Nil) Then
   If Assigned(fStrm.OnProgress) Then
   Begin
      FZRec.cb.pArchivePos^ :=
      	FZRec.cb.pArchivePos^ - FZRec.avail_in;
      Progress(fStrm);
   End;
   Result := iCount;
End;
//-------------------------------------------------------------

Destructor TCalcStreamCRC.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function CalcStreamCRC16(strm16: TStream32; Len: Int64; zsp: ztv_stream_plus): u_long;
Var
   crcobj16: TCalcStreamCRC;
Begin
   crcobj16 := TCalcStreamCRC.Create(strm16, 16, Len, zsp, Nil, Nil);
   strm16.Position := 0;
   Result := crcobj16.CRC;
   crcobj16.Free();
End;
//-------------------------------------------------------------

Function CalcStreamCRC32(strm32: TStream32; Len: Int64; zsp: ztv_stream_plus;
   CancelCallBackPtr: Pointer; ProgressCallBackProc: TNotifyEvent):
   u_long;
Var
   crcobj32: TCalcStreamCRC;
Begin
   crcobj32 :=
      TCalcStreamCRC.Create(strm32, Len, 32, zsp, CancelCallBackPtr,
         ProgressCallBackProc);
   strm32.Position := 0;
   Result := crcobj32.CRC;
   crcobj32.Free();
End;
//-------------------------------------------------------------

Function CalcFileCRC(f: THandle; Len: Int64; zsp: ztv_stream_plus; CancelCallBackPtr:
   Pointer; ProgressCallBackProc: TNotifyEvent): u_long;
Var
   HS: THandleStream32;
Begin
   HS := THandleStream32.Create(f);
   Result := CalcStreamCRC32(HS, Len, zsp, CancelCallBackPtr, ProgressCallBackProc);
   HS.Position := 0;
   HS.Free();
End;
//-------------------------------------------------------------

End.
