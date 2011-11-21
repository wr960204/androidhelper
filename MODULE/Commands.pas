unit Commands;

{$I AndroidLanguage.inc} 

interface

uses
  Windows, Classes, SysUtils, Graphics, AndroidThread, AndroidThreadBase,
  ApkConsts, ConsoleRun, StringConsts, StrUtils, vg_listbox, vg_extctrls,
  vg_controls, vg_scene, UIListItem, SearchFile, Forms, Utils,
  ASServiceSoap, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  
  TAndroidCommands = class
  private
    threadInstall: TAndroidThread;
    threadUninstall: TAndroidThread;
    threadCopyFile: TAndroidThread;
    threadReceive: TAndroidThread;
    crInstall: TConsoleRun;
    crUninstall: TConsoleRun;
    crRemount: TConsoleRun;
    crDir: TConsoleRun;
    crCopyFile: TConsoleRun;
    crReceive: TConsoleRun;
    crReboot: TConsoleRun;
    crDevice: TConsoleRun;
    crGetLS: TConsoleRun;
    crGetApp: TConsoleRun;

    crCheckSQLite: TConsoleRun;
    crInstallSQLite3: TConsoleRun;
    crSQLite3: TConsoleRun;

    FDeviceConnected: Boolean;
    FDeviceList: TStringList;
    FCurrentList: TStringList;
    FCurrentInstall: string;
  protected
    procedure threadInstallExecute(Sender: TObject; params: IAndroidMap);
    procedure threadInstallFinish(Sender: TObject);
    procedure threadUninstallExecute(Sender: TObject; params: IAndroidMap);
    procedure threadUninstallFinish(Sender: TObject);

    procedure crInstallGetConsole(ConsoleText: string);
    procedure crUninstallGetConsole(ConsoleText: string);
    procedure crDirGetConsole(ConsoleText: string);

    procedure threadCopyFileExecute(Sender: TObject; params: IAndroidMap);
    procedure threadCopyFileFinish(Sender: TObject);
    procedure crCopyFileGetConsole(ConsoleText: string);

    procedure threadReceiveExecute(Sender: TObject; params: IAndroidMap);
    procedure threadReceiveFinish(Sender: TObject);
    procedure crReceiveGetConsole(ConsoleText: string);
    procedure crRebootGetConsole(ConsoleText: string);
    procedure crDeviceGetConsole(ConsoleText: string);
    procedure crGetLSGetConsole(ConsoleText: string);
    procedure crGetAppGetConsole(ConsoleText: string);

    procedure crGetSQLite3Console(ConsoleText: string);

    procedure FoundFile(Sender: TObject;Path:string);
    procedure BatchFoundFile(Sender: TObject; Path: string);
    procedure BatchUnFoundFile(Sender: TObject; Path: string);

    // download events
    procedure OnBeginDownload(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Integer);
    procedure OnEndDownload(ASender: TObject; AWorkMode: TWorkMode);
    procedure OnDownloading(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InstallApk;
    procedure BatchInstallApk;
    procedure UninstallApk; overload; deprecated;   // v2.0.15
    procedure UninstallApk(ApkNameSpace: string); overload;
    procedure BatchUninstallApk; deprecated;  // v2.0.15
    procedure DoUninstall;
    procedure CopyFileToDevice(AFileName, APath: string);
    procedure CopyFileFromDevice(ADevicePath, APath: string);
    procedure RebootDevice(AMode: Integer);
    procedure RebootDeviceShell;
    function CheckDevice: Boolean;
    procedure InitDir(ADir: string);
    procedure InitWin32Dir(ADir: string);
    procedure InitDevices;
    procedure InitInstalledApp;
    procedure ShowAppStore;
    procedure ShowAppDetail(App: TAndroidApp; ico: TvgBitmap);
    procedure DoSearchApp(ACategory: string; AName: string);
    procedure DoShowAppDetail(App: TAndroidApp; ico: TvgBitmap);
    procedure DoDownloadApp(AUrl: string; AFileName: string);
    function CheckSQLiteExists: Boolean;
    function CheckSQLiteAvailable(AFileName: string): Boolean;
    procedure DoInstallSQLite3Module;
    function CheckSQLitePermission: Boolean;
    procedure DoOperateOnSQLite3(AFileName: string);
    procedure DoExecuteSQLite3(ACmd: string);
  end;


implementation

uses
  UI, UILinuxDirSelector, UIWin32DirSelector, UISelectDevice, UIUninstllApk,
  UIAppStore, UIAppDetail, UISQLiteSQL;

{ TAndroidCommands }

constructor TAndroidCommands.Create;
begin
  FDeviceList:= TStringList.Create;
  FCurrentList:= TStringList.Create;

  threadInstall:= TAndroidThread.Create(nil);
  threadInstall.Exclusif := False;
  threadInstall.FreeOnTerminate := True;
  threadInstall.RunOnCreate := True;
  threadInstall.OnExecute := threadInstallExecute;
  threadInstall.OnFinish := threadInstallFinish;

  threadUninstall:= TAndroidThread.Create(nil);
  threadUninstall.Exclusif := False;
  threadUninstall.FreeOnTerminate := True;
  threadUninstall.RunOnCreate := True;
  threadUninstall.OnExecute := threadUninstallExecute;
  threadUninstall.OnFinish := threadUninstallFinish;

  threadCopyFile:= TAndroidThread.Create(nil);
  threadCopyFile.Exclusif := False;
  threadCopyFile.FreeOnTerminate := True;
  threadCopyFile.RunOnCreate := True;
  threadCopyFile.OnExecute := threadCopyFileExecute;
  threadCopyFile.OnFinish := threadCopyFileFinish;

  threadReceive:= TAndroidThread.Create(nil);
  threadReceive.Exclusif := False;
  threadReceive.FreeOnTerminate := True;
  threadReceive.RunOnCreate := True;
  threadReceive.OnExecute := threadReceiveExecute;
  threadReceive.OnFinish := threadReceiveFinish;

  crInstall:= TConsoleRun.Create(nil);
  crInstall.OnGetConsole := crInstallGetConsole;
  crUninstall:= TConsoleRun.Create(nil);
  crUninstall.OnGetConsole := crUninstallGetConsole;

  crRemount:= TConsoleRun.Create(nil);
  crDir:= TConsoleRun.Create(nil);
  crDir.OnGetConsole := crDirGetConsole;
  crCopyFile:= TConsoleRun.Create(nil);
  crCopyFile.OnGetConsole := crCopyFileGetConsole;
  crReceive:= TConsoleRun.Create(nil);
  crReceive.OnGetConsole := crReceiveGetConsole;
  crReboot:= TConsoleRun.Create(nil);
  crReboot.OnGetConsole := crRebootGetConsole;
  crDevice:= TConsoleRun.Create(nil);
  crDevice.OnGetConsole := crDeviceGetConsole;

  crGetLS:= TConsoleRun.Create(nil);
  crGetLS.Timeout := 1500;
  crGetLS.OnGetConsole := crGetLSGetConsole;
  crGetApp:= TConsoleRun.Create(nil);
  crGetApp.Timeout := 1500;
  crGetApp.OnGetConsole := crGetAppGetConsole;

  crCheckSQLite := TConsoleRun.Create(nil);
  crCheckSQLite.Timeout := 1500;
  crInstallSQLite3 := TConsoleRun.Create(nil);
  crInstallSQLite3.Timeout := 3000;

  crSQLite3:= TConsoleRun.Create(nil);
  crSQLite3.Timeout := 3000;
  crSQLite3.OnGetConsole := crGetSQLite3Console;
  
  CheckDevice;
end;

destructor TAndroidCommands.Destroy;
begin
  threadInstall.Free;
  threadUninstall.Free;
  threadCopyFile.Free;
  threadReceive.Free;
  crInstall.Free;
  crUninstall.Free;
  crRemount.Free;
  crDir.Free;
  crCopyFile.Free;
  crReceive.Free;
  crReboot.Free;
  crDevice.Free;
  crGetLS.Free;
  crGetApp.Free;
  crCheckSQLite.Free;
  crInstallSQLite3.Free;
  crSQLite3.Free;
  
  FDeviceList.Free;
  FCurrentList.Free;
  inherited;
end;

procedure TAndroidCommands.DoDownloadApp(AUrl: string; AFileName: string);
var
  http: TIdHTTP;
  ms: TMemoryStream;
  fn: string;
  ec: DWORD;
  uninstallStr: string;
begin
  // download app
  http := TIdHTTP.Create(nil);
  http.OnWorkBegin := OnBeginDownload;
  http.OnWorkEnd := OnEndDownload;
  http.OnWork := OnDownloading;
  ms := TMemoryStream.Create;
  http.Get(AUrl, ms);
  fn := ExtractFilePath(ParamStr(0))+LOC_APK+AFileName;
  ms.SaveToFile(fn);
  ms.Free;
  http.Free;

  // install
  if FileExists(fn) then
  begin
    AndroidUI.edtInstall.Text := fn;
    AndroidUIAppDetail.lblInstalling.Visible := True;
    AndroidUIAppDetail.lblInstalling.Text := STR_INSTALLING_SHORT;
    // uninstall old application
    uninstallStr := Format(ADB_UNINSTALL, [AndroidUI.CurrentDevice, TAndroidUtils.GetPackageName(AndroidUI.edtInstall.Text)]);
    FCurrentInstall := ExtractFileName(AndroidUI.edtInstall.Text);
    crUninstall.RunProg(ADB_EXE, uninstallStr, ADB_PATH, ec);
    // install new 
    AndroidUI.Operation.btnInstallClick(nil);
    TAndroidUtils.AndroidMessageBox(STR_HINT, Format(FMT_INSTALL_MSG, [AFileName]), 1, STR_BTN_CLOSE);
    AndroidUIAppDetail.FUI.Close;
  end;
end;

procedure TAndroidCommands.DoExecuteSQLite3(ACmd: string);
var
  ec: DWORD;
begin
  crSQLite3.RunProg(ADB_EXE, Format(ADB_SQLITE3_CMD,
    [AndroidUI.CurrentDevice, AndroidUI.SQLite3Name, ACmd]),
    ADB_PATH, ec);
end;

procedure TAndroidCommands.DoInstallSQLite3Module;
var
  ec: DWORD;
  sPath: string;
begin
  // install SQLite3 module
  sPath := ExtractFilePath(ParamStr(0)) + LOC_ANDROID;
  Application.ProcessMessages;
  crInstallSQLite3.RunProg(ADB_EXE, Format(ADB_REMOUNT, [AndroidUI.CurrentDevice]), ADB_PATH, ec);
  Application.ProcessMessages;
  crInstallSQLite3.RunProg(ADB_EXE,
    Format(ADB_PUSH, [AndroidUI.CurrentDevice, sPath+A_SQLITE3, A_SYSTEM_BIN]), ADB_PATH, ec);
  Application.ProcessMessages;
  crInstallSQLite3.RunProg(ADB_EXE,
    Format(ADB_PATH, [AndroidUI.CurrentDevice, sPath+A_LIB_SQLITE, A_SYSTEM_LIB]), ADB_PATH, ec);
  Application.ProcessMessages;
  crInstallSQLite3.RunProg(ADB_EXE,
    Format(ADB_CHMOD, [AndroidUI.CurrentDevice, A_SYSTEM_BIN_SQLITE3]), ADB_PATH, ec);
end;

procedure TAndroidCommands.DoOperateOnSQLite3(AFileName: string);
begin
  // do operation in sqlite3
  with TAndroidUISQLiteSQL.Create do
  begin
    AndroidUI.SQLite3Name := AFileName;
    SetCaption(Format(FMT_SQLITE3_TITLE, [AFileName]));
    Execute;
    Free;
  end;  
end;

procedure TAndroidCommands.DoSearchApp(ACategory, AName: string);
var
  addr: string;
  apps: ArrayOfTAndroidApp;
  i: Integer;
  item: TAppStoreItem;
begin
  addr := AndroidUI.Config.Server + SRV_WSDL;
  if ACategory = STR_ALL_APPS then
    ACategory := EmptyStr;
  try
    if (ACategory = EmptyStr) and (AName = EmptyStr) then
      apps := ASServiceSoap.GetTASServicesSoap(False, addr).GetAppList20
    else if (ACategory <> EmptyStr) and (AName = EmptyStr) then
      apps := ASServiceSoap.GetTASServicesSoap(False, addr).GetAppListByCategory(ACategory)
    else if (ACategory = EmptyStr) and (AName <> EmptyStr) then
      apps := ASServiceSoap.GetTASServicesSoap(False, addr).GetAppListByName(AName)
    else
      apps := ASServiceSoap.GetTASServicesSoap(False, addr).GetAppListByCategoryAndName(ACategory, AName);
  except
    apps := nil;
  end;

  AndroidUIAppStore.lsItems.Clear;
  if (apps <> nil) then
  begin
    if Length(apps) > 0 then
    begin
      for i := 0 to Length(apps) - 1 do
      begin
        item:= TAppStoreItem.Create(AndroidUIAppStore.lsItems);
        item.Parent := AndroidUIAppStore.lsItems;
        item.ItemData := apps[i];
      end;  
    end;  
  end;   
end;

procedure TAndroidCommands.DoShowAppDetail(App: TAndroidApp; ico: TvgBitmap);
var
  i: Integer;
  strId: string;
  bmp: TBitmap;
begin
  // show applicatio detail
  strId := IntToStr(App.AppId);
  for i := Length(strId) + 1 to 16 do
    strId := VG_CHAR_0 + strId;
  bmp := TBitmap.Create;
  TAndroidUtils.Generate2DCode(strId, 2, bmp);
  AndroidUIAppDetail.App2DCode.Bitmap.Assign(bmp);
  bmp.Free;
  AndroidUIAppDetail.AppIco.Bitmap.Assign(ico);
  AndroidUIAppDetail.Window.Text := App.AppName + Format(FMT_QUOTED, [App.AppVersion]);
  AndroidUIAppDetail.lblSize.Text := Format(FMT_APP_SIZE, [App.AppSize]);
  AndroidUIAppDetail.lblDownCnt.Text := Format(FMT_APP_DOWN_CNT, [App.AppDownCount]);
  AndroidUIAppDetail.lblDate.Text := Format(FMT_APP_DATE, [App.AppAddDate]);
  AndroidUIAppDetail.lblPlatform.Text := Format(FMT_APP_PLATFORM, [App.AppPlatform]);
  AndroidUIAppDetail.lblAuthor.Text := Format(FMT_APP_AUTHOR, [App.AppAuthor]);
  AndroidUIAppDetail.barLevel.StarCount := App.AppLevel;
  AndroidUIAppDetail.mmDesc.ReadOnly := False;
  AndroidUIAppDetail.mmDesc.Lines.text := App.AppDesc;
  AndroidUIAppDetail.mmDesc.ReadOnly := True;
  AndroidUIAppDetail.Item := App;
end;

procedure TAndroidCommands.DoUninstall;
begin
  TAndroidUtils.UninstallApk(STR_UN_INSTLL_DIAG);
end;

procedure TAndroidCommands.FoundFile(Sender: TObject; Path: string);
var
  item: TWin32ListItem;
begin
  Application.ProcessMessages;
  item:= TWin32ListItem.Create(AndroidUIWin32DirSelector.lsItems);
  item.Parent := AndroidUIWin32DirSelector.lsItems;
  item.ItemName := ExtractFileName(Path);
  if FileExists(Path) then
    item.ItemType := itFile
  else if DirectoryExists(Path) then
    item.ItemType := itFolder
  else
    item.ItemType := itUnknown;
end;

procedure TAndroidCommands.InstallApk;
begin
  threadInstall.Execute(nil);
end;

procedure TAndroidCommands.OnBeginDownload(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Integer);
begin
  AndroidUIAppDetail.procDownload.Visible := True;
  AndroidUIAppDetail.procDownload.Max := AWorkCountMax;
  AndroidUIAppDetail.procDownload.Min := 0;
  AndroidUIAppDetail.procDownload.Value := 0;
  Application.ProcessMessages;
end;

procedure TAndroidCommands.OnDownloading(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Integer);
begin
  Application.ProcessMessages;
  AndroidUIAppDetail.procDownload.Value := AWorkCount;
end;

procedure TAndroidCommands.OnEndDownload(ASender: TObject;
  AWorkMode: TWorkMode);
begin
  AndroidUIAppDetail.procDownload.Visible := False;
  Application.ProcessMessages;
end;

procedure TAndroidCommands.threadInstallExecute(Sender: TObject; params: IAndroidMap);
var
  ec: DWORD;
  installStr: string;
begin
  if AndroidUI.chkReinstall.IsChecked then
    installStr := Format(ADB_INSTALL, [AndroidUI.CurrentDevice, CMDP_R, AndroidUI.edtInstall.Text])
  else
    installStr := Format(ADB_INSTALL, [AndroidUI.CurrentDevice, EmptyStr, AndroidUI.edtInstall.Text]);
  FCurrentInstall := ExtractFileName(AndroidUI.edtInstall.Text);
  crInstall.RunProg(ADB_EXE, installStr, ADB_PATH, ec);
end;

procedure TAndroidCommands.threadInstallFinish(Sender: TObject);
begin

  if AndroidUI.chkSaveApk.IsChecked then
    CopyFileToDevice(AndroidUI.edtInstall.Text, AndroidUI.edtSaveApk.Text)
  else
  begin
    AndroidUI.lblInstalling.Visible := False;
    AndroidUI.Operation.EnableOperation(True);
    AndroidUI.mmConsole.ReadOnly := False;
    AndroidUI.mmConsole.Lines.add(STR_OP_FINISH);
    AndroidUI.mmConsole.ReadOnly := True;
  end;  
  if Assigned(AndroidUIAppDetail) then
  begin
    AndroidUIAppDetail.lblInstalling.Visible := False;
  end;
end;

procedure TAndroidCommands.CopyFileToDevice(AFileName, APath: string);
var
  map: TAndroidMap;
  ec: DWORD;
begin
  crRemount.RunProg(ADB_EXE, Format(ADB_REMOUNT, [AndroidUI.CurrentDevice]), ADB_PATH, ec);
  crDir.RunProg(ADB_EXE, Format(ADB_MKDIR, [AndroidUI.CurrentDevice, APath]), ADB_PATH, ec);

  map:= TAndroidMap.Create;
  map.put(V_AFILENAME, AFileName);
  map.put(V_APATH, APath);
  threadCopyFile.Execute(map);
end;

procedure TAndroidCommands.crInstallGetConsole(ConsoleText: string);
var
  p1, p2: Integer;
  reason: string;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  ConsoleText := Trim(ConsoleText);
  if ContainsText(ConsoleText, RE_SUCCESS) then
    AndroidUI.mmConsole.Lines.Add(FCurrentInstall + SPC +STR_INSTALL_OK)
  else if ContainsText(ConsoleText, RE_FAILURE) then
  begin
    p1 := Pos(LINUX_LQ, ConsoleText);
    p2 := Pos(LINUX_RQ, ConsoleText);
    reason := Copy(ConsoleText, p1 + 1, p2 - p1 - 1);
    if reason = RE_EXISTS then
      AndroidUI.mmConsole.Lines.Add(Format(FMT_INSTALL_FAIL, [FCurrentInstall, SM_EXISTS]))
    else if ContainsText(ConsoleText, RE_SDK) then
      AndroidUI.mmConsole.Lines.add(Format(FMT_INSTALL_FAIL, [FCurrentInstall, SM_SDK_NOT_FIT]))
    else
      AndroidUI.mmConsole.Lines.add(Format(FMT_INSTALL_FAIL, [FCurrentInstall, reason]))
  end
  else
    AndroidUI.mmConsole.Lines.Add(STR_INSTALL_UNKNOWN);
  AndroidUI.mmConsole.ReadOnly := true;
end;

procedure TAndroidCommands.crDirGetConsole(ConsoleText: string);
var
  p: Integer;
  str: string;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  if ContainsText(ConsoleText, RE_FAILED) then
  begin
    p := Pos(LINUX_CA, ConsoleText);
    str := Trim(Copy(ConsoleText, p + 1, Length(ConsoleText) - p));
    if str <> RE_FILE_EXISTS then
    begin
      if ContainsText(ConsoleText, RE_NO_SUCH) then
        str := Format(FMT_CREATE_FAIL, [SM_NO_SUCH])
      else
        str := Format(FMT_CREATE_FAIL, [str]);
      AndroidUI.mmConsole.Lines.Add(str);
    end;
  end;
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.threadCopyFileExecute(Sender: TObject; params: IAndroidMap);
var
  str: string;
  AFileName: string;
  APath: string;
  ec: DWORD;
begin
  AFileName := params.get(V_AFILENAME);
  APath := params.get(V_APATH);
  str := Format(ADB_PUSH, [AndroidUI.CurrentDevice, AFileName,APath]);
  crCopyFile.RunProg(ADB_EXE, str, ADB_PATH, ec);
end;

procedure TAndroidCommands.threadCopyFileFinish(Sender: TObject);
begin
  AndroidUI.lblInstalling.Visible := False;
  AndroidUI.lblSending.Visible := False;
  AndroidUI.Operation.EnableOperation(True);
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Add(STR_OP_FINISH);
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.crCopyFileGetConsole(ConsoleText: string);
var
  p: Integer;
  str: string;
begin
  if ContainsText(UpperCase(ConsoleText), RE_FAILED) then
  begin
    p := Pos(LINUX_PT, ConsoleText);
    str := Copy(ConsoleText, p + 1, Length(ConsoleText) - p);
    p := Pos(LINUX_PT, str);
    if p > 0 then
      str := Copy(str, p + 1, Length(str) - p);
    if ContainsText(ConsoleText, RE_IS_DIR) then
      str := Format(FMT_COPY_FAIL, [SM_IS_DIR])
    else
      str := Format(FMT_COPY_FAIL, [Trim(str)]);
    AndroidUI.mmConsole.ReadOnly := False;
    AndroidUI.mmConsole.Lines.Add(str);
    AndroidUI.mmConsole.ReadOnly := True;
  end;
end;

procedure TAndroidCommands.CopyFileFromDevice(ADevicePath, APath: string);
var
  map: TAndroidMap;
begin
  ForceDirectories(APath);
  map:= TAndroidMap.Create;
  map.put(V_ADEVICE_PATH, ADevicePath);
  map.put(V_APATH, APath);
  threadReceive.Execute(map);
end;

procedure TAndroidCommands.threadReceiveExecute(Sender: TObject; params: IAndroidMap);
var
  str: string;
  ADevicePath: string;
  APath: string;
  ec: DWORD;
begin
  ADevicePath := params.get(V_ADEVICE_PATH);
  APath := params.get(V_APATH);
  str := Format(ADB_PULL, [AndroidUI.CurrentDevice, ADevicePath,APath]);
  crReceive.RunProg(ADB_EXE, str, ADB_PATH, ec);
end;

procedure TAndroidCommands.threadReceiveFinish(Sender: TObject);
begin
  AndroidUI.lblReceving.Visible := False;
  AndroidUI.Operation.EnableOperation(True);
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Add(STR_OP_FINISH);
  AndroidUI.mmConsole.ReadOnly := true;
end;

procedure TAndroidCommands.threadUninstallExecute(Sender: TObject;
  params: IAndroidMap);
var
  ec: DWORD;
  uninstallStr: string;
begin
  uninstallStr := Format(ADB_UNINSTALL, [AndroidUI.CurrentDevice, TAndroidUtils.GetPackageName(AndroidUI.edtInstall.Text)]);
  FCurrentInstall := ExtractFileName(AndroidUI.edtInstall.Text);
  crUninstall.RunProg(ADB_EXE, uninstallStr, ADB_PATH, ec);
end;

procedure TAndroidCommands.threadUninstallFinish(Sender: TObject);
begin
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.add(STR_OP_FINISH);
  AndroidUI.mmConsole.ReadOnly := True;
  AndroidUI.lblInstalling.Visible := False;
  AndroidUI.Operation.EnableOperation(True);
end;

procedure TAndroidCommands.UninstallApk(ApkNameSpace: string);
var
  ec: DWORD;
  uninstallStr: string;
begin
  uninstallStr := Format(ADB_UNINSTALL, [AndroidUI.CurrentDevice, ApkNameSpace]);
  FCurrentInstall := ApkNameSpace;
  crUninstall.RunProg(ADB_EXE, uninstallStr, ADB_PATH, ec);
end;

procedure TAndroidCommands.UninstallApk;
begin
  threadUninstall.Execute(nil);
end;

procedure TAndroidCommands.crReceiveGetConsole(ConsoleText: string);
begin
  if (ContainsText(ConsoleText, RE_BYTES)) and (ContainsText(ConsoleText, RE_KB)) then
    Exit;
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Add(Format(FMT_RECEIVE_FAIL, [Trim(ConsoleText)]));
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.crUninstallGetConsole(ConsoleText: string);
var
  s: string;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  s := Trim(ConsoleText);
  if UpperCase(s) = RE_SUCCESS then
  begin
    AndroidUI.mmConsole.Lines.add(Format(FMT_UNINSTALL_SUCCESS, [FCurrentInstall]));
    if Assigned(AndroidUIUninstall) then
      AndroidUIUninstall.lsItems.ItemByIndex(AndroidUIUninstall.lsItems.ItemIndex).Free;
  end
  else if UpperCase(s) = RE_FAILURE then
    AndroidUI.mmConsole.Lines.add(Format(FMT_UNINSTALL_FAILURE, [FCurrentInstall]))
  else
    AndroidUI.mmConsole.Lines.add(FMT_UNINSTALL_ERR);
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.RebootDevice(AMode: Integer);
var
  cmd: string;
  ec: DWORD;
begin
  cmd := Format(ADB_REBOOT, [AndroidUI.CurrentDevice]);
  case AMode of
  1:cmd := Format(ADB_REBOOT_BOOTLOADER, [AndroidUI.CurrentDevice]);
  2:cmd := Format(ADB_REBOOT_RECOVERY, [AndroidUI.CurrentDevice]);
  end;
  crReboot.RunProg(ADB_EXE, cmd, ADB_PATH, ec);
end;

procedure TAndroidCommands.RebootDeviceShell;
var
  cmd: string;
  ec: DWORD;
begin
  cmd := Format(ADB_SHELL_REBOOT, [AndroidUI.CurrentDevice]);
  crReboot.RunProg(ADB_EXE, cmd, ADB_PATH, ec);
end;

procedure TAndroidCommands.ShowAppDetail(App: TAndroidApp; ico: TvgBitmap);
begin
  //
  with TAndroidUIAppDetail.Create do
  begin
    DoShowAppDetail(App, ico);
    Execute;
    Free;
  end;  
end;

procedure TAndroidCommands.ShowAppStore;
begin
  with TAndroidUIAppStore.Create do
  begin
    DoSearchApp(EmptyStr, EmptyStr);
    Execute;
    Free;
  end;  
end;

procedure TAndroidCommands.crRebootGetConsole(ConsoleText: string);
var
  p: Integer;
  str: string;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  if ConsoleText = EmptyStr then
    AndroidUI.mmConsole.Lines.add(STR_OP_FINISH)
  else
  begin
    p := Pos(LINUX_PT, ConsoleText);
    str := Copy(ConsoleText, p+1, Length(ConsoleText) - p);
    // if devices returns "closed", run shell reboot again
    if LowerCase(Trim(str)) = RE_CLOSED then
      RebootDeviceShell
    else
      AndroidUI.mmConsole.Lines.Add(Format(FMT_REBOOT_FAIL, [Trim(str)]));
  end;
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.BatchFoundFile(Sender: TObject; Path: string);
var
  ec: DWORD;
  installStr: string;
begin
  Application.ProcessMessages;
  if AndroidUI.chkReinstall.IsChecked then
    installStr := Format(ADB_INSTALL, [AndroidUI.CurrentDevice, CMDP_R, Path])
  else
    installStr := Format(ADB_INSTALL, [AndroidUI.CurrentDevice, EmptyStr, Path]);
  FCurrentInstall := ExtractFileName(Path);
  crInstall.RunProg(ADB_EXE, installStr, ADB_PATH, ec);
  Application.ProcessMessages;
  if AndroidUI.chkSaveApk.IsChecked then
    CopyFileToDevice(Path, AndroidUI.edtSaveApk.Text);
end;

procedure TAndroidCommands.BatchInstallApk;
var
  sf: TSearchFile;
begin
  sf := TSearchFile.Create(nil);
  sf.Recursive := False;
  sf.OnFound := BatchFoundFile;
  sf.Mask := STR_MASK_APK;
  sf.Execute(AndroidUI.edtInstall.Text);
  sf.Free;
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Add(STR_OP_FINISH);
  AndroidUI.mmConsole.ReadOnly := True;

  AndroidUI.lblInstalling.Visible := False;
  AndroidUI.Operation.EnableOperation(True);
end;

procedure TAndroidCommands.BatchUnFoundFile(Sender: TObject; Path: string);
var
  ec: DWORD;
  uninstallStr: string;
begin
  Application.ProcessMessages;
  uninstallStr := Format(ADB_UNINSTALL, [AndroidUI.CurrentDevice, TAndroidUtils.GetPackageName(Path)]);
  FCurrentInstall := ExtractFileName(Path);
  crUninstall.RunProg(ADB_EXE, uninstallStr, ADB_PATH, ec);
end;

procedure TAndroidCommands.BatchUninstallApk;
var
  sf: TSearchFile;
begin
  sf := TSearchFile.Create(nil);
  sf.Recursive := False;
  sf.OnFound := BatchUnFoundFile;
  sf.Mask := STR_MASK_APK;
  sf.Execute(AndroidUI.edtInstall.Text);
  sf.Free;

  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Add(STR_OP_FINISH);
  AndroidUI.mmConsole.ReadOnly := True;
  AndroidUI.lblInstalling.Visible := False;
  AndroidUI.Operation.EnableOperation(True);
end;

function TAndroidCommands.CheckDevice: Boolean;
var
  ec: DWORD;
begin
  crDevice.RunProg(ADB_EXE, ADB_DEVICE, ADB_PATH, ec);
  Result := FDeviceConnected;
end;

function TAndroidCommands.CheckSQLiteAvailable(AFileName: string): Boolean;
var
  ec: DWORD;
  re: string;
begin
  // check sqlite3 available
  re := crCheckSQLite.RunProg(ADB_EXE, Format(ADB_CHECK_SQLITE3,
    [AndroidUI.CurrentDevice, AFileName]), ADB_PATH, ec);
  Result := not ContainsText(re, RE_SQLITE3_ERR);
end;

function TAndroidCommands.CheckSQLiteExists: Boolean;
var
  ec: DWORD;
  re: string;
begin
  // check sqlite3 exists
  re := crCheckSQLite.RunProg(ADB_EXE, Format(ADB_CHECK_SQLITE3_EXIST,
    [AndroidUI.CurrentDevice]), ADB_PATH, ec);
  Result := not ContainsText(re, RE_SQLITE3_NOT_FOUND);
end;

function TAndroidCommands.CheckSQLitePermission: Boolean;
var
  ec: DWORD;
  re: string;
begin
  // check sqlite3 exists
  re := crCheckSQLite.RunProg(ADB_EXE, Format(ADB_CHECK_SQLITE3_EXIST,
    [AndroidUI.CurrentDevice]), ADB_PATH, ec);
  Result := not ContainsText(re, RE_SQLITE_NO_PERMISSION);
end;

procedure TAndroidCommands.crDeviceGetConsole(ConsoleText: string);
var
  devLst: TStringList;
  i: Integer;
  curr: string;
begin
  FDeviceConnected := False;
  curr := AndroidUI.CurrentDevice;
  FDeviceList.Clear;
  AndroidUI.CurrentDevice := EmptyStr;
  AndroidUI.btnCurrentDevice.Text := Format(FMT_DEVICE, [STR_NA]);
  if Trim(ConsoleText) = EmptyStr then
  begin
    Exit;
  end;
  AndroidUI.mmConsole.ReadOnly := False;
  if ContainsText(ConsoleText, RE_ERROR) then
  begin
    AndroidUI.mmConsole.Lines.Add(STR_GET_DEVICE_ERROR);
    AndroidUI.mmConsole.ReadOnly := true;
    Exit;
  end;  

  devLst := TStringList.Create;
  ConsoleText := StringReplace(ConsoleText, V_DEVICE, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  ConsoleText := Trim(ConsoleText);
  devLst.Text := Trim(ConsoleText);

  for i := devLst.Count - 1 downto 0 do
  begin
    if (ContainsText(devLst[i], RE_KILLING)) or (ContainsText(devLst[i], RE_STAR))
      or (ContainsText(devLst[i], RE_OF)) or (ContainsText(devLst[i], RE_OFFLINE)) then
      devLst.Delete(i);
  end;

  if devLst.Count = 0 then
  begin
    AndroidUI.mmConsole.Lines.Add(STR_NO_DEVICE);
    AndroidUI.mmConsole.ReadOnly := true;
    Exit;
  end;  
  for i := 0 to devLst.Count - 1 do
  begin
    devLst[i] := Trim(devLst[i]);
    FDeviceList.Add(devLst[i]);
    AndroidUI.mmConsole.Lines.Add(Format(FMT_DEV_CONN, [devLst[i]]));
  end;

  if curr <> EmptyStr then
  begin
    if devLst.IndexOf(curr) = -1 then
      AndroidUI.CurrentDevice := devLst[0]
    else
      AndroidUI.CurrentDevice := curr;
  end
  else
    AndroidUI.CurrentDevice := devLst[0];
  AndroidUI.btnCurrentDevice.Text := Format(FMT_DEVICE, [AndroidUI.CurrentDevice]);

  FDeviceConnected := True;
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.crGetAppGetConsole(ConsoleText: string);
var
  i: Integer;
  pm: Integer;
  item: TAppListItem;
  tSize: Integer;
  ec: DWORD;
begin
  // get applications
  AndroidUI.mmConsole.ReadOnly := false;
  if ContainsText(ConsoleText, RE_PERMISSION_DENIED) then
  begin
    // no permission
    AndroidUI.mmConsole.Lines.Add(STR_PERMISSION_NO);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  if ContainsText(ConsoleText, RE_APPTOSD) then
  begin
    // app to sd
    AndroidUI.mmConsole.Lines.Add(SM_APPTOSD);
    AndroidUI.mmConsole.ReadOnly := True;
    crGetApp.RunProg(ADB_EXE, Format(ADB_SHELL_LS_APPTOSD_APP, [AndroidUI.CurrentDevice]), ADB_PATH, ec);
    Exit;
  end;  

  FCurrentList.Clear;
  FCurrentList.Text := ConsoleText;

  for i := FCurrentList.Count -1 downto 0 do
  begin
    Application.ProcessMessages;
    if TAndroidUtils.TrimSpc(FCurrentList[i]) = EmptyStr then
    begin
      FCurrentList.Delete(i);
      Continue;
    end;

    FCurrentList[i] := TAndroidUtils.ExtractLinuxAppNameAndSize(FCurrentList[i], tSize);
    if StartsStr(LINUX_G3_ST, FCurrentList[i]) then
    begin
      FCurrentList[i] := StringReplace(FCurrentList[i], LINUX_G3, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      pm := Pos(LINUX_G3_SP, FCurrentList[i]);
      FCurrentList[i] := Copy(FCurrentList[i], pm + 1, Length(FCurrentList[i]) - pm);
    end;
    if StartsText(LINUX_SP, FCurrentList[i]) then
    begin
      FCurrentList.Delete(i);
      Continue;
    end;
    FCurrentList[i] := FCurrentList[i]+Format(FMT_ITEM_VALUE, [tSize]);
  end;

  AndroidUIUninstall.lsItems.Clear;
  for i := 0 to FCurrentList.Count - 1 do
  begin
    Application.ProcessMessages;
    item:= TAppListItem.Create(AndroidUIUninstall.lsItems);
    item.Parent := AndroidUIUninstall.lsItems;
    item.TextStr := FCurrentList[i];
  end;
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidCommands.crGetLSGetConsole(ConsoleText: string);
var
  i: Integer;
  pm: Integer;
  item: TAndroidListItem;
begin
  AndroidUI.mmConsole.ReadOnly := false;
  if ContainsText(ConsoleText, RE_PERMISSION_DENIED) then
  begin
    AndroidUI.mmConsole.Lines.Add(STR_PERMISSION_NO);
    AndroidUILinuxDirSelector.edtSeledPath.Text :=
      TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  if ContainsText(ConsoleText, RE_NO_SUCH) then
  begin
    AndroidUILinuxDirSelector.edtSeledPath.Text := LINUX_SP;
    InitDir(LINUX_SP);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;  

  FCurrentList.Clear;
  FCurrentList.Text := ConsoleText;

  for i := FCurrentList.Count -1 downto 0 do
  begin
    Application.ProcessMessages;
    if TAndroidUtils.TrimSpc(FCurrentList[i]) = EmptyStr then
    begin
      FCurrentList.Delete(i);
      Continue;
    end;

    FCurrentList[i] := TAndroidUtils.ExtractLinuxFileNameAndType(FCurrentList[i]);

    if StartsStr(LINUX_G3_ST, FCurrentList[i]) then
    begin
      FCurrentList[i] := StringReplace(FCurrentList[i], LINUX_G3, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      pm := Pos(LINUX_G3_SP, FCurrentList[i]);
      FCurrentList[i] := Copy(FCurrentList[i], pm + 1, Length(FCurrentList[i]) - pm);
    end;
    if StartsText(LINUX_SP, FCurrentList[i]) then
    begin
      FCurrentList.Delete(i);
      Continue;
    end;
    if EndsText(LINUX_LNK, FCurrentList[i]) then
    begin
      FCurrentList.Delete(i);
      Continue;
    end;
  end;
  FCurrentList.Insert(0, STR_LINUX_UP);
  AndroidUILinuxDirSelector.lsItems.OnDblClick := nil;
  AndroidUILinuxDirSelector.lsItems.Clear;
  for i := 0 to FCurrentList.Count - 1 do
  begin
    Application.ProcessMessages;
    item:= TAndroidListItem.Create(AndroidUILinuxDirSelector.lsItems);
    item.Parent := AndroidUILinuxDirSelector.lsItems;
    item.ItemName := FCurrentList[i];
  end;
  AndroidUI.mmConsole.ReadOnly := True;
  AndroidUILinuxDirSelector.lsItems.OnDblClick := AndroidUI.Operation.lsItemsDblClick;
end;

procedure TAndroidCommands.crGetSQLite3Console(ConsoleText: string);
var
  sl: TStringList;
  i: Integer;
begin
  // sqlite3 execute
  AndroidUISQLiteSQL.mmSQLResult.Lines.clear;
  sl:= TStringList.Create;
  sl.Text := ConsoleText;
  for i := sl.Count - 1 downto 0 do
  begin
    if Trim(sl[i]) = EmptyStr then
      sl.Delete(i)
    else if (ContainsText(sl[i], RE_KILLING)) or (ContainsText(sl[i], RE_STAR)) then
      sl.Delete(i);    
  end;
  AndroidUISQLiteSQL.mmSQLResult.ReadOnly := False;
  AndroidUISQLiteSQL.mmSQLResult.Lines.Assign(sl);
  AndroidUISQLiteSQL.mmSQLResult.Lines.add(STR_OP_FINISH);
  AndroidUISQLiteSQL.mmSQLResult.ReadOnly := True;
  sl.Free;
end;

procedure TAndroidCommands.InitDevices;
var
  i: Integer;
  item: TDeviceListItem;
begin
  AndroidUISelectDevice.lsItem.Clear;
  for i := 0 to FDeviceList.Count - 1 do
  begin
    item := TDeviceListItem.Create(AndroidUISelectDevice.lsItem);
    item.Parent := AndroidUISelectDevice.lsItem;
    item.TextStr := FDeviceList[i];
  end;  
end;

procedure TAndroidCommands.InitDir(ADir: string);
var
  ec: DWORD;
begin
  crGetLS.RunProg(ADB_EXE, Format(ADB_SHELL_LS_G4_i7500, [AndroidUI.CurrentDevice, ADir]), ADB_PATH, ec);
end;

procedure TAndroidCommands.InitInstalledApp;
var
  ec: DWORD;
begin
  crGetApp.RunProg(ADB_EXE, Format(ADB_SHELL_LS_APP, [AndroidUI.CurrentDevice]), ADB_PATH, ec);
end;

procedure TAndroidCommands.InitWin32Dir(ADir: string);
var
  disks: TDisks;
  i: Integer;
  item: TWin32ListItem;
  search: TSearchFile;
begin
  AndroidUIWin32DirSelector.lsItems.OnDblClick := nil;
  AndroidUIWin32DirSelector.lsItems.Clear;
  if ADir = EmptyStr then
  begin
    disks := TAndroidUtils.GetDisks;
    for i := 0 to Length(disks) - 1 do
    begin
      item:= TWin32ListItem.Create(AndroidUIWin32DirSelector.lsItems);
      item.Parent := AndroidUIWin32DirSelector.lsItems;
      item.ItemName := disks[i];
    end;
  end
  else
  begin
    item:= TWin32ListItem.Create(AndroidUIWin32DirSelector.lsItems);
    item.Parent := AndroidUIWin32DirSelector.lsItems;
    item.ItemName := STR_LINUX_UP;
    search:= TSearchFile.Create(nil);
    search.Recursive := False;
    search.OnFound := FoundFile;
    search.Mask := STR_MASK;
    search.Execute(ADir);
    search.Free;
  end;
  AndroidUIWin32DirSelector.lsItems.OnDblClick := AndroidUI.Operation.lsItemWin32DblClick;
end;

end.
