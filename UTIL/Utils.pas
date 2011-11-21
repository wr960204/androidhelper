unit Utils;

{$I AndroidLanguage.inc}

interface

uses
  Windows, Classes, SysUtils, Graphics ,UIListItem, StringConsts, ApkConsts, StrUtils, Controls,
  ECC200, ztvUnZip, ztvregister, ztvBase, Forms,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TDisks = array of string;
  
  TAndroidUtils = class
  public
    class function UninstallApk(ACaption: string): Cardinal;
    class function SelectLinuxFile(ACaption: string; StartDir: string; OnlyDir: Boolean;
      OnlyFile: Boolean; NewDBButton: Boolean = False): string;
    class function SelectWin32File(ACaption: string; StartDir: string; OnlyDir: Boolean): string;
    class function CreateNewSQLite3atabase: string;
    class function SelectDevice: string;
    class function SelectCategory: string;
    class function AndroidMessageBox(ACaption, AMessage: string; AButtons: Integer = 2; ACloseCaption: string = ''): Cardinal;
    class function ExtractFilePathLinux(APath: string): string;
    class function GetImageIndex(Item: string): TAndroidItemType;
    class function BuildSurveyBody: string;
    class function GetPackageName(AFileName: string): string;
    class function GetDisks: TDisks;
    class function TrimSpc(const S: string): string;
    class function ExtractLinuxFileNameAndType(AStr: string): string;
    class function ExtractLinuxAppNameAndSize(AStr: string; out ASize: Integer): string;
    class function AdjustProcessPrivilege(ProcessHandle: THandle; Token_Name: PChar): boolean;
    class procedure Generate2DCode(AStr: string; ASize: Integer; ABmp: TBitmap);
  end;

implementation

uses
  UIMessageBox, UILinuxDirSelector, UIWin32DirSelector, UI, UISelectDevice, UIUninstllApk,
  UIAppStore, UISelectCategory, UINewSQLite3;

{ TAndroidUtils }

class function TAndroidUtils.AdjustProcessPrivilege(ProcessHandle: THandle; Token_Name: PChar): boolean;
var
  Token:       cardinal;
  TokenPri:    _TOKEN_PRIVILEGES;
  ProcessDest: int64;
  l:           DWORD;
begin
  Result := False;
  if OpenProcessToken(ProcessHandle, TOKEN_Adjust_Privileges, Token) then
  begin
    if LookupPrivilegeValue(nil, Token_Name, ProcessDest) then
    begin
      TokenPri.PrivilegeCount := 1;
      TokenPri.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      TokenPri.Privileges[0].Luid := ProcessDest;
      l := 0;
      if AdjustTokenPrivileges(Token, False, TokenPri, sizeof(TokenPri), nil, l) then
        Result := True;
    end;
  end;
end;

class function TAndroidUtils.AndroidMessageBox(ACaption, AMessage: string; AButtons: Integer = 2; ACloseCaption: string = ''): Cardinal;
begin
  with TAndroidUIMessageBox.Create do
  begin
    SetCaption(ACaption);
    SetMessage(AMessage);
    SetButton(AButtons);
    if ACloseCaption <> EmptyStr then
      SetCloseCaption(ACloseCaption);    
    Result := Execute;
    Free;
  end;
end;

class function TAndroidUtils.BuildSurveyBody: string;
begin
  Result := STR_SURVEY_BODY;
end;

class function TAndroidUtils.CreateNewSQLite3atabase: string;
begin
  // create new sqlite3 database dialog
  Result := EmptyStr;
  with TAndroidUINewSQLite3.Create do
  begin
    if Execute = mrOk then
      Result := edtDbName.Text;
    Free;
  end;  
end;

class function TAndroidUtils.ExtractFilePathLinux(APath: string): string;
var
  i: Integer;
begin
  if APath[Length(APath)] = LINUX_SP then
    APath := LeftStr(APath, Length(APath) - 1);
  for i := Length(APath) downto 1 do
  begin
    if APath[i] <> LINUX_SP then
      APath := LeftStr(APath, Length(APath) - 1)
    else
      Break;
  end;
  Result := APath;

end;

class function TAndroidUtils.ExtractLinuxAppNameAndSize(AStr: string;
  out ASize: Integer): string;
const
  S_START = 28;
var
  strSize: string;
  i: Integer;
begin
  Result := ExtractLinuxFileNameAndType(AStr);
  if Result[Length(Result)] = LINUX_FL then
    Result := LeftStr(Result, Length(Result) - 1);
  // remove file mask (*.apk)
  Result := LeftStr(Result, Length(Result) - 4);
  AStr := Trim(RightStr(AStr, Length(AStr) - S_START));
  for i := 1 to Length(AStr) do
  begin
    if AStr[i] <> SPC then
      strSize := strSize + AStr[i]
    else
      Break;
  end;
  ASize := StrToInt(strSize);  
end;

class function TAndroidUtils.ExtractLinuxFileNameAndType(AStr: string): string;
const
  N_START = 58;
var
  pt: Integer;
  ft: Char;
  ft_f: string;
  ft_n: string;
  i: Integer;
begin
  ft := AStr[1];
  ft_f := EmptyStr;
  case ft of
  LINUX_FMARK:
    begin
      if AStr[4] = LINUX_FMARK_EX then
        ft_f := LINUX_FL;
    end;  
  LINUX_DMARK: ft_f := LINUX_SP;
  LINUX_LMARK: ft_f := LINUX_LNK;
  end;
  pt := Pos(LINUX_G3_ST, AStr);
  if pt > 0 then
    ft_n := Copy(AStr, pt, length(AStr) - pt +  1)
  else
  begin
    // if has not G3_ST, then check space
    if Length(AStr) > N_START then
    begin
      ft_n := Trim(Copy(AStr, N_START, length(AStr) - (N_START - 1)));
      if ft_n[1] <> SPC then
      begin
        for i := N_START - 1 downto N_START - 10 do
        begin
          if AStr[i] <> SPC then
            ft_n := AStr[i] + ft_n
          else
            Break;
        end;
      end;
    end
    else
      ft_n := Copy(AStr, N_START - 2, Length(AStr) - (N_START - 3));

  end;
  if ft_f = LINUX_LNK then
  begin
    pt := Pos(LINUX_LMARK_EX, ft_n);
    ft_n := LeftStr(ft_n, pt - 1);
  end;
  result := TrimSpc(ft_n)+ft_f;

end;

class procedure TAndroidUtils.Generate2DCode(AStr: string; ASize: Integer; ABmp: TBitmap);
var
  s : TByteArray;
  m : TByteArray;
  i, j: Integer;
  w, h : integer;
  cc: Integer;
begin
  if not Assigned(ABmp) then
    Exit;
  SetLength(s, Length(AStr));
  for i := 1 to Length(AStr) do
  begin
    s[i-1] := Ord(AStr[i]);
  end;
  CalcECC200(s, ecc200_Autosize, ecc200_Square, m, w, h);
  ABmp.Width := w * ASize;
  ABmp.Height := h * ASize;
  for i := 0 to h - 1 do
  begin
    for j := 0 to w - 1 do
    begin
      cc := m[i * w + j];
      if cc = 1 then
      begin
        ABmp.Canvas.Brush.Color := clBlack;
        ABmp.Canvas.Rectangle(Rect(
          j*ASize,
          i*ASize,
          j*ASize+ASize,
          i*ASize+ASize));
      end;
    end;
  end;
end;

class function TAndroidUtils.GetDisks: TDisks;
var
  i:integer;
  len: Integer;
  DriveBits: set of 0..25;
  DriveChar: Char;
  ls: TStringList;
begin
  // 67 = C
  Integer(DriveBits) := GetLogicalDrives;
  
  for i := 2 to 25 do
  begin
    if not (i in DriveBits) then Continue;
    DriveChar := Char(i + 65);
    Application.ProcessMessages;
    len := Length(Result);
    SetLength(Result, len + 1);
    Result[len] := DriveChar + WIN32_DISK;
  end;
end;

class function TAndroidUtils.GetImageIndex(Item: string): TAndroidItemType;
var
  s: Char;
begin
  // /@*  NULL
  s := Item[Length(Item)];
  case s of
    LINUX_FL: Result := itFile;
    LINUX_SP: Result := itFolder;
    LINUX_LNK: Result := itLink;
    LINUX_UP: Result := itBack;
  else
    Result := itUnknown;
  end;

end;

class function TAndroidUtils.GetPackageName(AFileName: string): string;
var
  r: string;
  i, j: Integer;
  p: integer;
  sName: string;
  DataStr: string;
  ms: TDecompMemoryStream;
  ss: TStringStream;
  unzip: TUnZip;
begin
  Result := EmptyStr;
  unzip:= TUnZip.Create(nil);
  unzip.FileSpec.Text := STR_MASK_XML;
  unzip.ArchiveFile := AFileName;
  ms:= TDecompMemoryStream.Create;
  ss:= TStringStream.Create(EmptyStr);
  unzip.ExtractToMemoryStream(ms);

  for i := 0 to ms.FileCount - 1 do
  begin
    if ms.FileName[i] = STR_MASK_MANIFEST_XML then
    begin
      ms.Position := ms.FileOffset[i];
      ss.Size := ms.FileSize[i];
      ss.CopyFrom(ms, ms.FileSize[i]);
      DataStr := ss.DataString;

      r := EmptyStr;
      for j := 1 to Length(DataStr) do
      begin
        if (Ord(DataStr[j]) = 0) then
        begin
          if (Ord(DataStr[j+1]) = 0) then
          begin
            if Length(r) > 0 then
            begin
              if r[Length(r)] <> CH_ZERO then
                r := r + CH_ZERO;
            end;
          end;
        end
        else
        if (Ord(DataStr[j]) > 31) and (Ord(DataStr[j]) < 127) then
        begin
          r := r + DataStr[j];
        end;
      end;
      p := pos(STR_MANIFEST, r) + Length(STR_MANIFEST) + 1;
      sName := EmptyStr;
      for j := p to Length(r) do
      begin
        if r[j] <> CH_ZERO then
        begin
          if (ord(r[j]) > 45) and (ord(r[j]) < 127) then
            sName := sName+ r[j];
        end
        else
          Break;
      end;
      Result := sName;

      Break;
    end;
  end;
  ms.Free;
  ss.Free;
  unzip.Free;

end;

class function TAndroidUtils.SelectCategory: string;
var
  ori: string;
begin
  ori := AndroidUIAppStore.btnCategory.Text;
  with TAndroidUISelectCategory.Create do
  begin
    if Execute = mrOk then
    begin
      if lsItem.Count = 0 then
        Result := EmptyStr
      else if lsItem.ItemIndex = -1 then
        Result := ori
      else
        Result := TCategoryItem(lsItem.ItemByIndex(lsItem.ItemIndex)).Category;
    end
    else
      Result := ori;
    Free;
  end;  
end;

class function TAndroidUtils.SelectDevice: string;
var
  ori: string;
begin
  // select device
  ori := AndroidUI.CurrentDevice;
  with TAndroidUISelectDevice.Create do
  begin
    AndroidUI.Command.InitDevices;
    if Execute = mrOk then
    begin
      if lsItem.Count = 0 then
        Result := EmptyStr
      else if lsItem.ItemIndex = -1 then
        Result := ori
      else
        Result := TDeviceListItem(lsItem.ItemByIndex(lsItem.ItemIndex)).TextStr;
    end
    else
      Result := ori;
    Free;
  end;

end;

class function TAndroidUtils.SelectLinuxFile(ACaption, StartDir: string;
  OnlyDir: Boolean; OnlyFile: Boolean; NewDBButton: Boolean = False): string;
begin
  with TAndroidUILinuxDirSelector.Create do
  begin
    SetCaption(ACaption);
    AndroidUI.OnlySelectDirs := OnlyDir;
    AndroidUI.OnlyFile := OnlyFile;
    btnNewDB.Visible := NewDBButton;
    edtSeledPath.Text := StartDir;
    AndroidUI.Command.InitDir(StartDir);

    if Execute = mrOk then
      Result := edtSeledPath.Text
    else
      Result := StartDir;
    Free;
  end;
end;

class function TAndroidUtils.SelectWin32File(ACaption, StartDir: string; OnlyDir: Boolean): string;
begin
  with TAndroidUIWin32DirSelector.Create do
  begin
    SetCaption(ACaption);
    AndroidUI.OnlySelectDirs := OnlyDir;
    if StartDir <> EmptyStr then
    begin
      if StartDir[Length(StartDir)] = WIN32_SP then
        StartDir := LeftStr(StartDir, Length(StartDir) - 1);
    end;  
    AndroidUI.Command.InitWin32Dir(StartDir);
    edtSeledPath.Text := StartDir;
    if Execute = mrOk then
      Result := edtSeledPath.Text
    else
      Result := StartDir;
    Free;
  end;
end;

class function TAndroidUtils.TrimSpc(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = SPC) do
    Inc(I);
  if I > L then
    Result := EmptyStr
  else
  begin
    while S[L] <= SPC do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;

end;

class function TAndroidUtils.UninstallApk(ACaption: string): Cardinal;
begin
  with TAndroidUIUninstall.Create do
  begin
    SetCaption(ACaption);
    AndroidUI.Command.InitInstalledApp;
    Result := Execute;
    Free;
  end;
end;

end.
