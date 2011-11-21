Unit ztvGbls;

Interface

Uses
   Windows,
   Dialogs,
   SysUtils,
   Classes,
   Forms,
   Buttons,
   StdCtrls,
   Controls,
   ComCtrls,
   ztvStreams,
   ztvCrypt,
   ztvErrMsgs,
   ztvConsts;

Type
   TztvDriveType =
      (dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM,
      dtRam, dtFloppy3, dtFloppy5);

   //TExtSet = (ace, arc, arj, bh, cab, enc, gz, ha, jar, lha, lzh, pak, pk3,
   //   pk_, rar, tar, tgz, war, uue, uu, xxe, z, zip, zoo);

   E_RAISE = Class(exception);          //custom error handler

   TSearchRec32 = Record
      Time: Integer;
      Size: Cardinal;
      Attr: Integer;
      Name: TFileName;
      ExcludeAttr: Integer;
      FindHandle: THandle;
      FindData: TWin32FindData;
   End;

   TDateMethod = (dtCreate, dtLastWrite, dtLastAccess);

   THeaderType = (htLocal, htCentral, htEnding, htLocator, htEnding64, htNone);
   THeaderTypeState = Set Of THeaderType;
   pHeaderTypeState = ^THeaderTypeState;

   (* ArcType set members *)
   TArcType = (atNA,
      atUnsupported,
      atFileOpenErr,
      atUnknown,
      atAce, atAceExe,
      atArc, atArcExe,
      atArj, atArjExe,
      atBh, atBhExe,
      atCab, atCabExe,
      atGZip,
      atHA,
      atJar, atJarExe,
      atLha, atLhaExe, atLzh, atLzhExe,
      atMsGZ,
      atPak, atPakExe,
      atRar, atRarExe,
      atTar,
      atTarGZip,
      atUUE,
      atZip, atZipExe, atZipDS, atZipMV,
      atZoo);

Const
   Wrapper_ArcType: Set Of TArcType = [atAce, atAceExe, atRar, atRarExe, atCab,
      atCabExe];
   Invalid_ArcType: Set Of TArcType = [atNA..atUnknown]; // Invalid archive range
   Zipped_ArcType: Set Of TArcType = [atZip, atZipExe, atJar, atJarExe];
   Split_ArcType: Set Of TArcType = [atZip, atZipExe, atZipMV, atJar, atJarExe];

   (* Archive types supported with decompression *)
   Decompress_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      , atCab..atCabExe
      , atGZip
      , atJar..atJar
      , atLha..atLzhExe
   , atRar..atRarExe
      , atTar
      , atUUE
      , atZip..atZipMV
      , atZoo
      ];

   (* File types supported with Encoding *)
   Encode_ArcType: Set Of TArcType = [atUUE];

   (* Archive types supported with compression *)
   Compress_ArcType: Set Of TArcType = [
   	atBh..atBh
      , atCab
      , atGZip
      //,atZoo
   	, atJar
      , atLha
      , atLzh
      , atTar
      , atZip..atZip{, atZipExe}, atZipMV
   ];

   (* Archives TZipSearch supports *)
   Search_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      , atCab..atCabExe
      , atGZip
      , atJar..atJarExe
      , atLha..atLzhExe
   	, atRar..atRarExe
      , atTar
      , atUUE
      , atZip..atZipExe
      , atZipDS
      , atZipMV
      , atZoo
      ];

   (* Archives TZipCheck supports *)
   Verify_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      //, atCab
   	, atGZip
      , atJar..atJarExe
      , atLha..atLzhExe
      , atRar..atRarExe
      , atTar
      , atZip..atZipExe
      , atZipDS
      , atZipMV
      , atZoo
      , atUUE
      ];

   (* Compression components that support
                       password encryption *)
   Compress_PasswordSupport:
   Set Of TArcType = [
      atBh..atBh
      , atJar..atJar
      , atZip..atZip, atZipMV
      ];

   Comment_Support_ArcType: Set Of TArcType = [
      //atJar..atJarExe
   atZip..atZipExe, atZipMV
      ];

   UnSFX_ArcType: Set Of TArcType = [
   atArcExe
      , atArjExe
      , atAceExe
      , atBhExe
      , atLhaExe
      , atLzhExe
      , atPakExe
      , atRarExe
      , atZipExe
      ];

Const
   // ArcTypeNames must EXACTLY!! match
   // the order of the members of
   // TArcType (above)
   ArcTypeNames: Array[TArcType] Of String =
   ('Not avail. '
      , 'Unsupport.'
      , 'File Error'
      , 'Unknown   '
      , 'ACE       ', 'ACE SFX   '
      , 'ARC       ', 'ARC SFX   '
      , 'ARJ       ', 'ARJ SFX   '
      , 'BH        ', 'BH  SFX   '
      , 'CAB       ', 'CAB SFX   '
      , 'GZip      '
      , 'HA        '
      , 'Java JAR  ', 'JJAR SFX  '
      , 'LHA       ', 'LHA SFX   ', 'LZH       ', 'LZH SFX   '
      , 'MsGZ     	'
      , 'PAK       ', 'PAK SFX   '
      , 'RAR       ', 'RAR SFX   '
      , 'TAR       '
      , 'TarGzip   '
      , 'UUE       '
      , 'ZIP       ', 'ZIP SFX   '
      , 'Zip Span  '
      , 'Zip MVol  '
      , 'ZOO       '
      );

Const
   Arj_ArcType: Set Of TArcType = [atArj..atArjExe];
   Arc_ArcType: Set Of TArcType = [atArc..atArcExe];
   Bh_ArcType: Set Of TArcType = [atBh..atBhExe];
   Cab_ArcType: Set Of TArcType = [atCab..atCabExe];
   GZip_ArcType: Set Of TArcType = [atGZip..atGZip];
   Ha_ArcType: Set Of TArcType = [atHA..atHA];
   Lha_ArcType: Set Of TArcType = [atLha..atLzhExe];
   Pak_ArcType: Set Of TArcType = [atPak..atPakExe];
   Rar_ArcType: Set Of TArcType = [atRar..atRarExe];
   Tar_ArcType: Set Of TArcType = [atTar..atTar];
   Zip_ArcType: Set Of TArcType = [atJar..atJarExe, atZip..atZipExe,
      atZipDS..atZipMV];
   Zoo_ArcType: Set Of TArcType = [atZoo..atZoo];


Const
   DriveNames: Array[TztvDriveType] Of String[9] = (
   	'Unknown', 'None', 'Floppy', 'Fixed', 'Network', 'CD-ROM', 'RAM',
      '3?Floppy', '5?Floppy'
      );
      
Function _ChangeFileExt(FileName, Ext: String): String;
Function _DirectoryExists(Const Name: String): Boolean;
Function AppendDirTail(Path: String): String;
Function AppendDrive(FileName: String): String;
Function CalcProgress64(dx, dy: int64): Byte;
Function CalcProgress32(dx, dy: Cardinal): Byte; Overload;
Function CalcProgressDbl(dx, dy: Double): Byte;
Function CalcRatio(dx, dy: int64): Integer; Overload;
Function CalcRatio(dx, dy: Double): Integer; Overload;
Function CalcRatio(dx, dy: Integer): Integer; Overload;
Function ChangeFileExtTmp(FileName: String): String;
Function CharToOemFilter(str: String; b: Boolean): String;
Function CharToUnixOemFilter(str: String; b: Boolean): String;
Function CompareMem(p1, p2: Pointer; Len: Integer): Boolean;
Function CreateDirEx(DirName: String): Boolean;
Function DecodeDir(Dir: String): String;
Function DirExists(Name: String): Boolean;
Function DOSToUnixFilename(Const str: String): String;
Function GetDeflateMethodStr(BitFlag: word): String;
Function GetDeflateMethodStr64(BitFlag: word): String;
Function ExtractFilenameOnly(str: String): String; Overload;
Function ExtractFilenameOnly(str: pChar): Byte; Overload;
Function FileTimeToInt(FileTime: TFileTime): Integer;
Function GetImageIndex(FileName: String): Integer;
Function GetTempPathStr: String;
Function GetToken(Var str: String; Seperator: String): String;
Function isDirEmpty(Dir: String): Boolean;
Function IsDrivePath(srce: Pointer; Count: Integer): Boolean;
Function IsGZipSignAttr(Attr: Integer): Boolean;
Function IsUncPath(Path: String): Boolean;
Function IsWriteable(FileName: String): Boolean;
Function OemToCharFilter(str: String; b: Boolean): String;
Function PCharToStr(pstr: pChar): String;
Function ReadFilename_NullTerminate(strm: TStream32; Var fPos: int64): String;
Function ReadFilename_DefinedLen(strm: TStream32; Len: Byte): AnsiString; Overload;
Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: int64; Len: Byte):
   AnsiString; Overload;
Function RemoveDirTail(Path: String): String;
Function ShortName(HWind: Hwnd; Const FileName: String): String;
Function SlashSep(Const Path, s: String): String;
Function StringAsPChar(s: String): pChar;
Function SwapWords(SW: Integer): Integer;
Function TStringsToStr(ts: TStrings; SepChar: Char): String;
Function UnformatNumber(s: String): String;
Function UnixToDosFilename(str: String): String;
Function VerifyAttr(Attr: Integer): Integer;
Function ztvConvertDate(FileDate: Integer): TDateTime;
Function ztvFileExists(Const FileName: String): Boolean;

Procedure CopyMem(source, dest: Pointer; Count: Integer);
Procedure DestroyImageList(DIL: TListView);
Procedure InitializeImageList(Sender: TComponent; IIL: TListView);
Procedure StrToTStrings(p: pChar; ts: TStrings);

Implementation

Uses
   ShellApi,
   ztvBase;



//-------------------------------------------------------------

Function StringAsPChar(s: String): pChar;
Begin
   Result := pChar(s);
End;
//-------------------------------------------------------------

Function SlashSep(Const Path, s: String): String;
Begin
   Result := '';
   If Path = '' Then exit;
   If AnsiLastChar(Path)^ <> '\' Then
      Result := Path + '\' + s
   Else
      Result := Path + s;
End;
//-------------------------------------------------------------

Function AppendDirTail(Path: String): String;
Begin
   If Path <> '' Then
   	If AnsiLastChar(Path)^ <> '\' Then
         Path := Path + '\';

   Result := Path;
End;
//-------------------------------------------------------------

Function AppendDrive(FileName: String): String;
Begin
   If Pos(':\', FileName) = 0 Then
      Result := GetCurrentDir[1] + ':\' + FileName
   Else
      Result := FileName;
End;
//-------------------------------------------------------------

Function RemoveDirTail(Path: String): String;
Begin
   If Path = '' Then
   	Result := ''
   Else If Path[Length(Path)] = '\' Then
      //Delete(Path, Length(Path), 1); Result := Path;
        //or
      Result := copy(Path, 1, Length(Path) - 1)
   Else
      Result := Path;
End;
//-------------------------------------------------------------

Function CalcProgress64(dx, dy: int64): Byte;
Var
   R, s: EXTENDED;
Begin
   //If dy > 0 Then
   //Begin
   //   dx := dx * 100;
   //   R := dx Div dy;
   //   If (R < 0) Then R := 0;
   //End
   //Else
   //   R := 0;
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);
End;
//-------------------------------------------------------------

Function CalcProgressDbl(dx, dy: Double): Byte;
Var
   R, s: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);

End;
//-------------------------------------------------------------

Function CalcProgress32(dx, dy: Cardinal): Byte;
Var
   R, s: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);

End;
//-------------------------------------------------------------

Function DecodeDir(Dir: String): String;
Const
   DELIM = $2F;
   DELIM2 = $FF;
   WIN_DELIM = $5C;
Var
   c: Byte;
   i: Word;
   flg: Boolean;
Begin
   flg := false;
   For i := 1 To Length(Dir) Do
   Begin
      c := Byte(Dir[i]);
      If flg Then
         flg := false
      Else If (c >= $80) And (c <= $9F) Or (c >= $E0) And (c <= $FD) Then
         flg := True
      Else If (Dir[i] = '\\') Or (c = DELIM) Or (c = DELIM2) Then
         Dir[i] := Char(WIN_DELIM);
   End;
   Result := Trim(Dir);                 (* Trim undesired ctrl codes *)
End;
//-------------------------------------------------------------

Function DirExists(Name: String): Boolean;
Var
   Code: Integer;
Begin
   Code := GetFileAttributes(pChar(Name));
   Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;
//-------------------------------------------------------------

Function OemToCharFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      If b Then
         OemToChar(@str[1], @str[1]);
         //OemToCharBuff(@str[1], @str[1], Length(str));

      // v4.6.1 rem'd the following block
      //For i := 1 To Length(str) Do
      //	If str[i] = '0' Then
      //   	str[i] = #
      str := UnixToDosFilename(str);
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function CharToUnixOemFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      str := DOSToUnixFilename(str);
      If b Then
         CharToOem(@str[1], @str[1]);
         //CharToOemBuff(@str[1], @str[1], Length(str));
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function CharToOemFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      If b Then
         CharToOem(@str[1], @str[1]);
         //CharToOemBuff(@str[1], @str[1], Length(str));
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function DOSToUnixFilename(Const str: String): String;
Var
   i: Integer;
Begin
   Result := str;
   For i := 1 To Length(str) Do
      If Result[i] = '\' Then Result[i] := '/';
End;
//-------------------------------------------------------------

Function ChangeFileExtTmp(FileName: String): String;
Var
   Ext: String;
Begin
   Ext := ExtractFileExt(FileName);
   If Length(Ext) > 3 Then
      Result := _ChangeFileExt(FileName, '.~' + copy(Ext, 2, Length(Ext) - 2))
   Else If Length(Ext) > 1 Then
      Result := _ChangeFileExt(FileName, '.~' + copy(Ext, 2, Length(Ext) - 1))
   Else
      Result := FileName + '.~' + Ext;
End;
//-------------------------------------------------------------
// call with dot in Ext

Function _ChangeFileExt(FileName, Ext: String): String;
Begin
   Result := copy(FileName, 1, ExtractFilenameOnly(@FileName[1])) + Ext;
End;
//-------------------------------------------------------------

Function ExtractFilenameOnly(str: String): String;
Begin
	str := ExtractFileName(str);
	Result := Copy(str, 1, ExtractFilenameOnly(pChar(str)));
End;
//-------------------------------------------------------------

Function ExtractFilenameOnly(str: pChar): Byte;
Var
   p: pChar;
Begin
   p := StrRScan(str, '.');
   If p = Nil Then
      Result := Strlen(str)
   Else
      Result := Integer(p) - Integer(str);
End;
//-------------------------------------------------------------

Function ztvFileExists(Const FileName: String): Boolean;
Var
   Handle: THandle;
   FindData: TWin32FindData;
Begin
   Handle := FindFirstFile(pChar(FileName), FindData);
   Result := Handle <> INVALID_HANDLE_VALUE;
   If Result Then Windows.FindClose(Handle);
End;
//-------------------------------------------------------------

Function GetTempPathStr: String;
Var
   Buffer: Array[0..MAX_PATH - 1] Of Char;
Begin
   If Windows.GetTempPath(SizeOf(Buffer) - 1, @Buffer[0]) <> 0 Then
      Result := String(Buffer)
   Else
      SetLength(Result, 0);
End;
//-------------------------------------------------------------

Function GetToken(Var str: String; Seperator: String): String;
Var
   i: word;
Begin
   i := Pos(Seperator, str);
   If i <> 0 Then
   Begin
      Result := System.copy(str, 1, i - 1);
      System.Delete(str, 1, i + (Length(Seperator) - 1));
   End
   Else
   Begin
      Result := str;
      SetLength(str, 0);
   End;
End;
//-------------------------------------------------------------

Function isDirEmpty(Dir: String): Boolean;
Var
   SearchRec: TSearchRec;
Begin
	Result := True;
   If FindFirst(SlashSep(Dir, '*.*'), faAnyFile, SearchRec) = 0 Then
   Begin
      Repeat
         If (SearchRec.Attr = FILE_ATTRIBUTE_DIRECTORY) Then
         Begin
            If (SearchRec.Name[1] <> '.') Then
               If Not isDirEmpty(Dir + AppendDirTail(SearchRec.Name)) Then
                  Result := False;
         End Else
         	Result := False;

         If Not Result Then Break;
      Until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
   End Else
   	Result := False;

End;
//-------------------------------------------------------------

Function IsGZipSignAttr(Attr: Integer): Boolean;
Begin
   Result :=
      (Attr = GZIP_MAGIC) Or (Attr = OLD_GZIP_MAGIC) Or
      (Attr = LZW_MAGIC) Or (Attr = LZH_MAGIC) Or
      (Attr = PACK_MAGIC);
End;
//-------------------------------------------------------------

// some stored file's attributes (created from PkWare's zip) are stored
// as High(integer) + Attribute.  VerifyAttr is used to convert these
// attributes to compatible file-attribute variables, so crc comparisons
// are successful.

Function VerifyAttr(Attr: Integer): Integer;

	Procedure AddBit(Bit: Word);
   Begin
      If Attr And Bit > 0 Then Result := Result Or Bit;
   End;
Begin
   Result := 0;
   AddBit(ZTV_FILE_ATTRIBUTE_NORMAL);
   AddBit(ZTV_FILE_ATTRIBUTE_ARCHIVE);
   AddBit(ZTV_FILE_ATTRIBUTE_READONLY);
   AddBit(ZTV_FILE_ATTRIBUTE_HIDDEN);
   AddBit(SysUtils.faVolumeID);
   AddBit(ZTV_FILE_ATTRIBUTE_SYSTEM);
   AddBit(ZTV_FILE_ATTRIBUTE_DIRECTORY);
   AddBit(ZTV_FILE_ATTRIBUTE_ENCRYPTED);
End;
//-------------------------------------------------------------

Function IsWriteable(FileName: String): Boolean;
Var
   Attr: Integer;
Begin
   Attr := GetFileAttributes(pChar(FileName));
   Result :=
      ((Attr And FILE_ATTRIBUTE_DIRECTORY) = 0) And
      ((Attr And FILE_ATTRIBUTE_READONLY) = 0) And
      ((Attr And FILE_ATTRIBUTE_SYSTEM) = 0) And
      ((Attr And FILE_ATTRIBUTE_HIDDEN) = 0);
End;
//-------------------------------------------------------------

Function PCharToStr(pstr: pChar): String;
Begin
   SetLength(Result, Strlen(pstr));
   CopyMem(pstr, @Result[1], Length(Result));
End;
//-------------------------------------------------------------

Function ReadFilename_NullTerminate(strm: TStream32; Var fPos: int64): String;
Var
   Buf: Array[0..255] Of Char;
Begin
   strm.Position := fPos;
   strm.Read(Buf[0], 256);
   Result := String(Buf);
   Inc(fPos, Length(Result) + 1);
End;
//-------------------------------------------------------------

Function ReadFilename_DefinedLen(strm: TStream32; Len: Byte): AnsiString;
Var
   s: AnsiString;
	BytesRead: Integer;
   i: Integer;
Begin
	i := strm.Position;
   SetLength(s, Len);
   BytesRead := strm.Read(s[1], Len);
   If BytesRead <> 0 Then ;
   If i <> 0 Then ;
   Result := s;
End;
//-------------------------------------------------------------

Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: Int64; Len:
   Byte): AnsiString;
Begin
   strm.Position := fPos;
   SetLength(Result, Len);
   ZeroMemory(@Result[1], Len);
   strm.Read(Result[1], Len);
   Inc(fPos, Len);
End;

{Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: int64; Len: Byte):
   String;
Var
   s: pChar;
Begin
   strm.Position := fPos;

   GetMem(s, Len + 1);
   Try
      ZeroMemory(s, Len + 1);
      strm.Read(s[0], Len);
      //SetString(Result, s, Len);	// v4.6.7 changed from Result := StrPas(s);
      Result := String(s);
   Finally
      FreeMem(s, Len + 1);
   End;

   Inc(fPos, Len);
End;}
//-------------------------------------------------------------

Function SwapWords(SW: Integer): Integer;
Begin
   Result := Makelong(HiWord(SW), LoWord(SW));
End;
//-------------------------------------------------------------

Function UnixToDosFilename(str: String): String;
Var
   i: word;
Begin
   Result := str;
   For i := 1 To Length(Result) Do
      If Result[i] = '/' Then
         Result[i] := '\';
End;
//-------------------------------------------------------------

Function CompareMem(p1, p2: Pointer; Len: Integer): Boolean;
Var
   b1, b2: ^Byte;
Begin
   b1 := p1;
   b2 := p2;
   While (b1^ = b2^) And (longint(b1) < longint(p1) + Len) Do
   Begin
      Inc(b1);
      Inc(b2);
   End;
   Result := longint(b1) = longint(p1) + Len;
End;
//-------------------------------------------------------------

Function CreateDirEx(DirName: String): Boolean;

Var
   i: Integer;
   FullDirName: String;
   LFullDirName: Integer;
   PrevError: word;

Begin
   // if directory already exists, do nothing
   If DirExists(DirName) Then
      Result := True
   Else
   Begin
      // Disable "abort, retry, ignore..."
      PrevError := SetErrorMode(SEM_FAILCRITICALERRORS);

      // Make sure we have '\' at end of directory name
      FullDirName := AppendDirTail(ExpandFileName(DirName));
      LFullDirName := System.Length(FullDirName);

      // Search for initial '\' in directory name
      i := 1;
      While ((i <= LFullDirName) And (FullDirName[i] <> '\')) Do
         Inc(i);

      While (i <= LFullDirName) Do
      Begin
         // Create subdirectory up to current '\'
         CreateDir(copy(FullDirName, 1, Pred(i)));

         // Look for next '\' in directory name
         Inc(i);
         While ((i <= LFullDirName) And (FullDirName[i] <> '\')) Do
            Inc(i);

      End;
      // Reenable "abort, retry, ignore ..."
      SetErrorMode(PrevError);

      // Success if directory now exists
      Result := DirExists(DirName);
   End;

End;
//-------------------------------------------------------------

{
function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

Function ztvFileTimeToDateTime(FileTime: TFileTime): TDateTime;
Var
   SysTime: TSystemTime;
   LocalTime: TFileTime;
Begin
   FileTimeToLocalFileTime(FileTime, LocalTime);
   FileTimeToSystemTime(LocalTime, SysTime);
   Result :=
      EncodeDate(SysTime.wYear, SysTime.wMonth, SysTime.wDay) +
      EncodeTime(SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);
End;}
//-------------------------------------------------------------
Function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
Var
	I: Integer;
  	DayTable: PDayTable;
Begin
  	Result := False;
  	DayTable := @MonthDays[IsLeapYear(Year)];
  	If (Year >= 1) And (Year <= 9999) And (Month >= 1) And (Month <= 12) And
   	(Day >= 1) And (Day <= DayTable^[Month]) Then
  	Begin
    	For I := 1 To Month - 1 Do Inc(Day, DayTable^[I]);
    	I := Year - 1;
    	Date := I * 365 + I Div 4 - I Div 100 + I Div 400 + Day - DateDelta;
    	Result := True;
  	End;
End;

Function EncodeDate(Year, Month, Day: Word): TDateTime;
Begin
  If Not ztvGbls.DoEncodeDate(Year, Month, Day, Result) then
      Result := 29221.000694;           // 1/1/80 12:01 am
End;

//-------------------------------------------------------------

// Note: 2162720 is the integer value of 1/1/1980 12:01 am
Function ztvConvertDate(FileDate: Integer): TDateTime;
Begin
   Try
      If FileDate = 0 Then
         FileDate := 2162720; //1980

      // The below conversion is the same as:
      // Result := FileDateToDateTime( FileDate );
      // ...except FileDateToDateTime has no built-in error recovery
      Result :=
         EncodeDate(
         LongRec(FileDate).HI Shr 9 + 1980, // year
         (LongRec(FileDate).HI Shr 5 And 15) {Mod 13}, // month
         (LongRec(FileDate).HI And 31) {Mod 32}) + // day
         EncodeTime(
         (LongRec(FileDate).lo Shr 11) {Mod 25}, // hr
         (LongRec(FileDate).lo Shr 5 And 63) {Mod 61}, // min
         (LongRec(FileDate).lo And 31 Shl 1) {Mod 61}, // sec
         0);                            // milsec

   Except
      Result := 29221.000694;           // 1/1/80 12:01 am
   End;
End;
//-------------------------------------------------------------

Procedure DestroyImageList(DIL: TListView);
Begin
   DIL.LargeImages.Free();
   DIL.SmallImages.Free();
End;
//-------------------------------------------------------------

Function _DirectoryExists(Const Name: String): Boolean;
Var
   Code: Integer;
Begin
   Code := GetFileAttributes(pChar(Name));
   Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;
//-------------------------------------------------------------

Procedure InitializeImageList(Sender: TComponent; IIL: TListView);
Var
   SHFileInfo: TSHFileInfo;
Begin
   IIL.LargeImages := TImageList.Create(Sender);
   IIL.LargeImages.ShareImages := True;
   IIL.LargeImages.Handle :=
      ShGetFileInfo({'*.*'}'',
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX Or SHGFI_LARGEICON);

   IIL.SmallImages := TImageList.Create(Sender);
   IIL.SmallImages.ShareImages := True;
   IIL.SmallImages.Handle :=
      ShGetFileInfo({'*.*'}'',
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX Or SHGFI_SMALLICON);
End;
//-------------------------------------------------------------

Function TStringsToStr(ts: TStrings; SepChar: Char): String;
Const
   AllowedSepChars: Set Of Char = [' ', ',', ';', #0];
Var
   i: Integer;
Begin
   Result := '';
   If (SepChar In AllowedSepChars) Then
      For i := 0 To ts.Count - 1 Do
         Result := Result + ts[i];
End;
//-------------------------------------------------------------

Procedure StrToTStrings(p: pChar; ts: TStrings);
Const
   SepChars: Set Of Char = [',', ';', #0];
Var
   p2: pChar;
Begin
   While (p^ In SepChars) Do            // advance past seperators
      Inc(p);

   p2 := p;
   Repeat
      If (p^ In SepChars) Then
      Begin
         p2[p - p2] := #0;
         ts.Add(StrPas(p2));
         While (p^ In SepChars) Do      // advance past seperators
            Inc(p);
         p2 := p;
      End
      Else
         Inc(p);
   Until (p^ = #0);

   If Strlen(p2) > 0 Then
      ts.Add(StrPas(p2));
End;
//-------------------------------------------------------------

Procedure CopyMem(source, dest: Pointer; Count: Integer);
Var
   D: ^Byte Absolute dest;
   s: ^Byte Absolute source;
Begin
   While Count > 0 Do
   Begin
      D^ := s^;
      Inc(D);
      Inc(s);
      dec(Count);
   End;
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: Integer): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: int64): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: Double): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function GetDeflateMethodStr64(BitFlag: word): String;
Const
   DeflateMethods: Array[0..6] Of String =
   ('Def-64N', '', 'Def-64X', '', 'Def-64F', '', 'Def-64S');
Begin
   If (BitFlag < 7) And (BitFlag Mod 2 = 0) Then
      Result := DeflateMethods[BitFlag]
   Else
      Result := DeflateMethods[0];

   If Length(Result) = 0 Then
      Result := 'Deflate';
End;
//-------------------------------------------------------------

Function GetDeflateMethodStr(BitFlag: word): String;
Const
   DeflateMethods: Array[0..6] Of String =
   ('DeflateN', '', 'DeflateX', '', 'DeflateF', '', 'DeflateS');
Begin
   If (BitFlag < 7) And (BitFlag Mod 2 = 0) Then
      Result := DeflateMethods[BitFlag]
   Else
      Result := DeflateMethods[0];

   If Length(Result) = 0 Then
      Result := 'Deflate';
End;
//-------------------------------------------------------------

Function GetImageIndex(FileName: String): Integer;
Var
   SHFileInfo: TSHFileInfo;
Begin
   FillChar(SHFileInfo, SizeOf(SHFileInfo), #0);
   ShGetFileInfo(pChar(ExtractFileName(FileName)),
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_USEFILEATTRIBUTES Or
      SHGFI_SYSICONINDEX Or
      SHGFI_EXETYPE);

   Result := SHFileInfo.iIcon;
End;
//-------------------------------------------------------------

Function IsDrivePath(srce: Pointer; Count: Integer): Boolean;
Var
   s1: ^Byte Absolute srce;
Begin
	Result := (Count > 3);  // ie.. 'c:\'
   If Result Then
   Begin
   	If s1^ = 92 Then
      Begin
      	Inc(s1);
         If s1^ = 92 Then	// UNC path name?
      	Begin
         	Result := False;
            Exit;
         End;
      	Inc(s1);
      End Else
      	Inc(s1, 2);

      Result := s1^ = 92; // char '\' = ascii value 92
   End;
End;
//-------------------------------------------------------------

Function IsUncPath(Path: String): Boolean;
Begin
   If Length(Path) > 2 Then
      Result := (Path[1] = '\') And (Path[2] = '\')  //CompareStr( Copy(Path, 1, 2), '\\' ) = 0
   Else
      Result := false;
End;
//-------------------------------------------------------------

Function FileTimeToInt(FileTime: TFileTime): Integer;
Var
   LocalFileTime: TFileTime;
Begin
   FileTimeToLocalFileTime(FileTime, LocalFileTime);
   FileTimeToDosDateTime(LocalFileTime, LongRec(Result).HI, LongRec(Result).lo);
   //ShowMessage( FormatDateTime( 'mmddyy hh:mm', FileDateToDateTime( Result )) );
End;
//------------------------------------------------------------

Function ShortName(HWind: Hwnd; Const FileName: String): String;

   Function TooBig(hdcCtrl: hDC; Const width: Integer; Const s: String):
      Boolean;
   Var
      Size: TSize;
   Begin
      GetTextExtentPoint32(hdcCtrl, pChar(s), Length(s), Size);
      Result := Size.cx > width;
   End;

   Procedure CutFirstDirectory(Var s: String);
   Var
      ROOT: Boolean;
      p: Integer;
   Begin
      If s = '\' Then
         SetLength(s, 0)
      Else
      Begin
         If s[1] = '\' Then
         Begin
            ROOT := True;
            s := copy(s, 2, 255);
         End
         Else
            ROOT := false;

         If s[1] = '.' Then
            s := copy(s, 5, 255);

         p := Pos('\', s);

         If p <> 0 Then
            s := '...\' + copy(s, p + 1, 255)
         Else
            SetLength(s, 0);

         If ROOT Then
            s := '\' + s;
      End;
   End;
Var
   Drive: String[3];
   Dir, Name, Ext: String;
   p: Integer;
   rect: TRect;
   hdcCtrl: hDC;
   hwndCtrl: Hwnd;
Begin

   hwndCtrl := GetDlgItem(HWind, 1);
   hdcCtrl := GetDC(hwndCtrl);
   GetClientRect(hwndCtrl, rect);

   Result := FileName;
   Dir := ExtractFilePath(Result);
   Name := ExtractFileName(Result);
   p := Pos('.', Name);
   If p > 0 Then
      SetLength(Name, p - 1);

   Ext := ExtractFileExt(Result);

   If (Length(Dir) > 1) And (Dir[2] = ':') Then
   Begin
      Drive := copy(Dir, 1, 2);
      Dir := copy(Dir, 3, 255);
   End
   Else
      SetLength(Drive, 0);

   While ((Dir <> '') Or (Drive <> '')) And (TooBig(hdcCtrl, rect.Right -
      rect.Left, Result)) Do
   Begin
      If Dir = '\...\' Then
      Begin
         SetLength(Drive, 0);
         Dir := '...\';
      End
      Else If Dir = '' Then
         SetLength(Drive, 0)
      Else
         CutFirstDirectory(Dir);

      Result := Drive + Dir + Name + Ext;
   End;
   ReleaseDC(hwndCtrl, hdcCtrl);
End;
//-------------------------------------------------------------
// remove the comma from the formated number string
//-------------------------------------------------------------

Function UnformatNumber(s: String): String;
Var
   b: ^Byte;
   i: Integer;
Begin
   Result := '';
   b := @s[1];

   For i := 1 To Length(s) Do
   Begin
      // if a number, add it to the result string
      If (b^ >= 48) And (b^ <= 57) Then
         Result := Result + s[i];

      Inc(b);
   End;
End;
//------------------------------------------------------------


End.
