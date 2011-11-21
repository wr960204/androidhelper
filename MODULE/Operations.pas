unit Operations;

{$I AndroidLanguage.inc} 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, vg_scene, vg_controls, vg_layouts, vg_tabcontrol, vg_textbox,
  vg_objects, vg_colors, vg_memo, StringConsts, Commands, ApkConsts, StrUtils,
  UILinuxDirSelector, UIListItem, UIWin32DirSelector, UIUninstllApk, 
  ShellAPI, UIDirectAlias, ShlObj, Utils, UIAppStore, UIAppDetail,
  UISQLiteSQL, ASServiceSoap,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidOperations = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure UIActived(Sender: TObject);
    procedure SwitchLayout(Index: Integer);
    procedure ChangeUIColor(AColor: string);
    procedure ChangeUITransparent(APercent: Single);
    procedure ChangeUIScale(AScale: Single);
    procedure EnableOperation(AEnabled: Boolean);
    procedure TabClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnReceiveClick(Sender: TObject);
    procedure btnRebootClick(Sender: TObject);
    procedure btnDeviceClick(Sender: TObject);
    procedure edtSaveApkChange(Sender: TObject);
    procedure edtSaveApkButtonClick(Sender: TObject);
    procedure chkSaveApkClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure BtnBackWin32Click(Sender: TObject);
    procedure lsItemsDblClick(Sender: TObject);
    procedure lsItemWin32DblClick(Sender: TObject);
    procedure lsItemAppDblClick(Sender: TObject);
    procedure edtSendPathChange(Sender: TObject);
    procedure edtSendPathButtonClick(Sender: TObject);
    procedure edtReceiveFileChange(Sender: TObject);
    procedure edtReceiveFileButtonClick(Sender: TObject);
    procedure edtInstallButtonClick(Sender: TObject);
    procedure edtSendFileButtonClick(Sender: TObject);
    procedure edtReceivePathChange(Sender: TObject);
    procedure edtReceivePathButtonClick(Sender: TObject);
    procedure btnCurrentDeviceClick(Sender: TObject);
    procedure btnSurveyClick(Sender: TObject);
    procedure btnImageAboutClick(Sender: TObject);
    procedure btnWin32DeskClick(Sender: TObject);
    procedure btnWin32HomeClick(Sender: TObject);
    procedure btnWin32DocumentClick(Sender: TObject);
    procedure btnLinuxHomeClick(Sender: TObject);
    procedure btnLinuxSDCardClick(Sender: TObject);
    procedure btnLinuxNewDBClick(Sender: TObject);
    procedure btnSubUninstallClick(Sender: TObject);
    procedure btnSelCategoryClick(Sender: TObject);
    procedure btnSearchAppClick(Sender: TObject);
    procedure btnAppStoreClick(Sender: TObject);
    procedure btnSQLiteClick(Sender: TObject);
    procedure btnSQLiteDumpClick(Sender: TObject);
    procedure btnSQLiteTableClick(Sender: TObject);
    procedure btnSQLiteLoadClick(Sender: TObject);
    procedure btnLinexDirOKClick(Sender: TObject);
    procedure lblAppWebsiteClick(Sender: TObject);
    procedure lblAppMailClick(Sender: TObject);
    procedure btnDownAndInstallClick(Sender: TObject);
    procedure AppStarClick(Sender: TObject);
    procedure btnSQLite3HelpClick(Sender: TObject);
    procedure btnSQLite3ExecuteClick(Sender: TObject);
    procedure btnSQLite3OptionClick(Sender: TObject);
    procedure UIKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UIKeyDownSQL(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UILabelMouseEnter(Sender: TObject);
    procedure UILabelMouseLeave(Sender: TObject);
    procedure UIColorChange(Sender: TObject);
    procedure UITransparentChange(Sender: TObject);
    procedure UIScaleChange(Sender: TObject);
    procedure UIAppStoreEnter(Sender: TObject);
    procedure UIAPPStoreLeave(Sender: TObject);
    procedure UISQLiteEnter(Sender: TObject);
    procedure UISQLiteLeave(Sender: TObject);

    procedure UISQLiteDumpEnter(Sender: TObject);
    procedure UISQLiteDumpLeave(Sender: TObject);
    procedure UISQLiteTablesEnter(Sender: TObject);
    procedure UISQLiteTablesLeave(Sender: TObject);
    procedure UISQLiteLoadEnter(Sender: TObject);
    procedure UISQLiteLoadLeave(Sender: TObject);

    procedure AppException(Sender: TObject; E: Exception);
  end;

implementation

uses
  UI;

{ TAndroidOperations }

procedure TAndroidOperations.edtReceivePathButtonClick(Sender: TObject);
begin
  AndroidUI.edtReceivePath.Text := TAndroidUtils.SelectWin32File(STR_SAVE_FILE, AndroidUI.edtReceivePath.Text, True);
  AndroidUI.Config.ReceivePath := AndroidUI.edtReceivePath.Text;
end;

procedure TAndroidOperations.edtReceivePathChange(Sender: TObject);
begin
  AndroidUI.Config.ReceivePath := AndroidUI.edtReceivePath.Text;
end;

procedure TAndroidOperations.edtSendFileButtonClick(Sender: TObject);
begin
  AndroidUI.edtSendFile.Text := TAndroidUtils.SelectWin32File(STR_SEL_FILE, ExtractFilePath(AndroidUI.edtSendFile.Text), False);
end;

procedure TAndroidOperations.edtInstallButtonClick(Sender: TObject);
begin
  AndroidUI.edtInstall.Text := TAndroidUtils.SelectWin32File(STR_SEL_APK_FILE, ExtractFilePath(AndroidUI.edtInstall.Text), False);
end;

procedure TAndroidOperations.edtSendPathButtonClick(Sender: TObject);
begin
  if AndroidUI.CurrentDevice = EmptyStr then
    Exit;
  if AndroidUI.edtSendPath.Text[Length(AndroidUI.edtSendPath.Text)] <> LINUX_SP then
    AndroidUI.edtSendPath.Text := AndroidUI.edtSendPath.Text + LINUX_SP;
  AndroidUI.edtSendPath.Text := TAndroidUtils.SelectLinuxFile(STR_M_FILE_PATH, AndroidUI.edtSendPath.Text, True, False);
  AndroidUI.Config.SendPath := AndroidUI.edtSendPath.Text;
end;

procedure TAndroidOperations.edtSendPathChange(Sender: TObject);
begin
  AndroidUI.Config.SendPath := AndroidUI.edtSendPath.Text;
end;

procedure TAndroidOperations.chkSaveApkClick(Sender: TObject);
begin
  AndroidUI.edtSaveApk.Enabled := AndroidUI.chkSaveApk.IsChecked;
  AndroidUI.Config.SaveApk := AndroidUI.chkSaveApk.IsChecked;
end;

procedure TAndroidOperations.edtSaveApkButtonClick(Sender: TObject);
begin
  if AndroidUI.CurrentDevice = EmptyStr then
    Exit;
  if AndroidUI.edtSaveApk.Text = EmptyStr then
    AndroidUI.edtSaveApk.Text := LINUX_SP;
  if AndroidUI.edtSaveApk.Text[Length(AndroidUI.edtSaveApk.Text)] <> LINUX_SP then
    AndroidUI.edtSaveApk.Text := AndroidUI.edtSaveApk.Text + LINUX_SP;
  AndroidUI.edtSaveApk.Text := TAndroidUtils.SelectLinuxFile(STR_APK_PATH, AndroidUI.edtSaveApk.Text, True, False);
  AndroidUI.Config.SaveApkPath := AndroidUI.edtSaveApk.Text;
end;

procedure TAndroidOperations.edtSaveApkChange(Sender: TObject);
begin
  AndroidUI.Config.SaveApkPath := AndroidUI.edtSaveApk.Text;
end;

procedure TAndroidOperations.btnImageAboutClick(Sender: TObject);
begin
  ShellExecute(0, SHELL_CMD_OPEN, SHELL_CMD_WEBSITE, nil, nil, SW_SHOW);
end;

procedure TAndroidOperations.btnInstallClick(Sender: TObject);
var
  rId: Cardinal;
begin
  if AndroidUI.CurrentDevice = EmptyStr then
  begin
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  if (FileExists(AndroidUI.edtInstall.Text))
    and (LowerCase(ExtractFileExt(AndroidUI.edtInstall.Text)) = STR_APK_EXT) then
  begin
    AndroidUI.mmConsole.ReadOnly := False;
    AndroidUI.mmConsole.Lines.Clear;

    if AndroidUI.chkSaveApk.IsChecked then
    begin
      if AndroidUI.edtSaveApk.Text = EmptyStr then
      begin
        AndroidUI.mmConsole.Lines.Add(SEL_APK_PATH);
        AndroidUI.mmConsole.ReadOnly := True;
        Exit;
      end;
    end;

    EnableOperation(False);
    AndroidUI.lblInstalling.Visible := True;
    AndroidUI.lblInstalling.Text := STR_INSTALLING;
    AndroidUI.Command.InstallApk;
    AndroidUI.mmConsole.ReadOnly := True;
  end
  else if FileExists(AndroidUI.edtInstall.Text)
    and (not (LowerCase(ExtractFileExt(AndroidUI.edtInstall.Text)) = STR_APK_EXT)) then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_APK_ERROR);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end
  else if (not FileExists(AndroidUI.edtInstall.Text))
    and (not DirectoryExists(AndroidUI.edtInstall.Text)) then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_APK);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end
  else if DirectoryExists(AndroidUI.edtInstall.Text) then
  begin

    rId := TAndroidUtils.AndroidMessageBox(STR_HINT, BATCH_INSTALL);
    if rId = mrOk then
    begin
      AndroidUI.mmConsole.ReadOnly := False;
      AndroidUI.mmConsole.Lines.Clear;
      if AndroidUI.chkSaveApk.IsChecked then
      begin
        if AndroidUI.edtSaveApk.Text = EmptyStr then
        begin
          AndroidUI.mmConsole.Lines.Add(SEL_APK_PATH);
          AndroidUI.mmConsole.ReadOnly := True;
          Exit;
        end;
      end;
      EnableOperation(False);
      AndroidUI.lblInstalling.Visible := True;
      AndroidUI.lblInstalling.Text := STR_INSTALLING;
      AndroidUI.Command.BatchInstallApk;
      AndroidUI.mmConsole.ReadOnly := True;
    end;  
  end;  

end;

procedure TAndroidOperations.btnLinexDirOKClick(Sender: TObject);
begin
  if AndroidUI.OnlyFile then
  begin
    if AndroidUILinuxDirSelector.edtSeledPath.Text[Length(AndroidUILinuxDirSelector.edtSeledPath.Text)] = LINUX_SP then
    begin
      TAndroidUtils.AndroidMessageBox(STR_HINT, STR_SELECT_ONLY_FILE, 1, STR_BTN_CLOSE);
      Exit;
    end;
  end;
  AndroidUILinuxDirSelector.FUI.ModalResult := mrOk;
end;

procedure TAndroidOperations.btnLinuxHomeClick(Sender: TObject);
begin
  AndroidUILinuxDirSelector.edtSeledPath.Text := LINUX_SP;
  AndroidUI.Command.InitDir(AndroidUILinuxDirSelector.edtSeledPath.Text);
end;

procedure TAndroidOperations.btnLinuxNewDBClick(Sender: TObject);
var
  fn: string;
begin
  // new database
  fn := TAndroidUtils.CreateNewSQLite3atabase;
  if fn = EmptyStr then
    Exit;
  if AndroidUILinuxDirSelector.edtSeledPath.Text
    [Length(AndroidUILinuxDirSelector.edtSeledPath.Text)] = LINUX_SP then
    AndroidUILinuxDirSelector.edtSeledPath.Text :=
      AndroidUILinuxDirSelector.edtSeledPath.Text + fn
  else
    AndroidUILinuxDirSelector.edtSeledPath.Text :=
      TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text) + fn;
end;

procedure TAndroidOperations.btnLinuxSDCardClick(Sender: TObject);
begin
  AndroidUILinuxDirSelector.edtSeledPath.Text := LINUX_SDCARD_SP;
  AndroidUI.Command.InitDir(AndroidUILinuxDirSelector.edtSeledPath.Text);
end;

procedure TAndroidOperations.btnSearchAppClick(Sender: TObject);
begin
  AndroidUI.Command.DoSearchApp(
    AndroidUIAppStore.btnCategory.Text,
    AndroidUIAppStore.edtName.Text);
end;

procedure TAndroidOperations.btnSelCategoryClick(Sender: TObject);
begin
  AndroidUIAppStore.btnCategory.Text := TAndroidUtils.SelectCategory;
end;

procedure TAndroidOperations.btnSendClick(Sender: TObject);
var
  bSendFolder: Boolean;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Clear;
  bSendFolder := DirectoryExists(AndroidUI.edtSendFile.Text);

  if not FileExists(AndroidUI.edtSendFile.Text) then
  begin
    if not bSendFolder then
    begin
      AndroidUI.mmConsole.Lines.Add(SEL_SEND);
      AndroidUI.mmConsole.ReadOnly := True;
      Exit;
    end;
  end;

  if AndroidUI.edtSendFile.Text = EmptyStr then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_SEND_PATH);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  if AndroidUI.CurrentDevice = EmptyStr then
  begin
    AndroidUI.mmConsole.ReadOnly := true;
    Exit;
  end;
    
  EnableOperation(False);
  AndroidUI.lblSending.Visible := True;
  if bSendFolder then
    AndroidUI.mmConsole.Lines.Add(SEND_DIR);
  AndroidUI.Command.CopyFileToDevice(AndroidUI.edtSendFile.Text, AndroidUI.edtSendPath.Text);
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidOperations.btnSQLite3ExecuteClick(Sender: TObject);
begin
  // sqlite3 execute
  if AndroidUISQLiteSQL.mmSQL.Lines.text = EmptyStr then
    Exit;
  AndroidUI.Command.DoExecuteSQLite3(AndroidUISQLiteSQL.mmSQL.Lines.Text);
end;

procedure TAndroidOperations.btnSQLite3HelpClick(Sender: TObject);
begin
  // sqlite3 help
  ShellExecute(0, SHELL_CMD_OPEN, SHELL_CMD_SQLITE3_WEBSITE, nil, nil, SW_SHOW);
end;

procedure TAndroidOperations.btnSQLite3OptionClick(Sender: TObject);
begin
  // sqlite3 options
end;

procedure TAndroidOperations.btnSQLiteClick(Sender: TObject);
var
  fn: string;
begin
  // sqlite click
  if AndroidUI.CurrentDevice = EmptyStr then
    Exit;
  if not AndroidUI.Command.CheckSQLiteExists then
  begin
    if TAndroidUtils.AndroidMessageBox(STR_HINT, STR_SQLITE3_NOT_INSTALLED) = mrOk then
    begin
      // insall sqlite3 module
      AndroidUI.Command.DoInstallSQLite3Module;
    end
    else
      Exit;    
  end;
  if not AndroidUI.Command.CheckSQLitePermission then
  begin
    TAndroidUtils.AndroidMessageBox(STR_HINT, STR_SQLITE3_NO_PERMISSION, 1, STR_BTN_CLOSE);
    Exit;
  end;  
  fn := TAndroidUtils.SelectLinuxFile(STR_SELECT_SQLITE_DB, LINUX_SP, False, True, True);
  if fn = LINUX_SP then
    Exit;
  if fn[Length(fn)] = LINUX_SP then
  begin
    TAndroidUtils.AndroidMessageBox(STR_HINT, STR_SQLITE3_ERR_MSG, 1, STR_BTN_CLOSE);
    Exit;
  end;

  if fn[Length(fn)] = LINUX_FL then
    fn := LeftStr(fn, Length(fn) - 1);

  if AndroidUI.Command.CheckSQLiteAvailable(fn) then
    AndroidUI.Command.DoOperateOnSQLite3(fn)
  else
    TAndroidUtils.AndroidMessageBox(STR_HINT, STR_SQLITE3_ERR_MSG, 1, STR_BTN_CLOSE);
end;

procedure TAndroidOperations.btnSQLiteDumpClick(Sender: TObject);
begin
  AndroidUISQLiteSQL.mmSQL.Lines.add(ADB_SQLITE3_DUMP);
end;

procedure TAndroidOperations.btnSQLiteLoadClick(Sender: TObject);
var
  fn: string;
  sl: TStringList;
  s: string;
begin
  // load script
  fn := TAndroidUtils.SelectWin32File(STR_SEL_FILE, EmptyStr, False);
  if not FileExists(fn) then
    Exit;
  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  for s in sl do
    AndroidUISQLiteSQL.mmSQL.Lines.add(s);
  sl.Free;
end;

procedure TAndroidOperations.btnSQLiteTableClick(Sender: TObject);
begin
  AndroidUISQLiteSQL.mmSQL.Lines.add(ADB_SQLITE3_TABLES);
end;

procedure TAndroidOperations.btnSubUninstallClick(Sender: TObject);
begin
  // uninstall
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Clear;
  EnableOperation(False);
  AndroidUIUninstall.Window.Enabled := False;
  AndroidUI.lblInstalling.Visible := True;
  AndroidUI.lblInstalling.Text := STR_UNINSTALLING;
  Application.ProcessMessages;
  AndroidUIUninstall.lsItems.ItemIndex := TAppListItem(TvgHudButton(Sender).Parent).Index;
  AndroidUI.Command.UninstallApk(TAppListItem(TvgHudButton(Sender).Parent).NameSpace);
  AndroidUI.mmConsole.ReadOnly := True;
  AndroidUIUninstall.Window.Enabled := True;
  AndroidUI.lblInstalling.Visible := False;
  EnableOperation(True);
end;

procedure TAndroidOperations.btnSurveyClick(Sender: TObject);
var
  mailStr: string;
begin
  mailStr := SHELL_CMD_MAILTO + TAndroidUtils.BuildSurveyBody;
  ShellExecute(0, SHELL_CMD_OPEN, PChar(mailStr), nil,nil, SW_SHOW);
end;

procedure TAndroidOperations.btnUninstallClick(Sender: TObject);
//var
//  rId: Cardinal;
begin
  if AndroidUI.CurrentDevice = EmptyStr then
  begin
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  AndroidUI.Command.DoUninstall;

  (*
  if (FileExists(AndroidUI.edtInstall.Text))
    and (LowerCase(ExtractFileExt(AndroidUI.edtInstall.Text)) = STR_APK_EXT) then
  begin
    AndroidUI.mmConsole.ReadOnly := False;
    AndroidUI.mmConsole.Lines.Clear;

    EnableOperation(False);
    AndroidUI.lblInstalling.Visible := True;
    AndroidUI.lblInstalling.Text := STR_UNINSTALLING;
    AndroidUI.Command.UninstallApk;
    AndroidUI.mmConsole.ReadOnly := True;
  end
  else if FileExists(AndroidUI.edtInstall.Text)
    and (not (LowerCase(ExtractFileExt(AndroidUI.edtInstall.Text)) = STR_APK_EXT)) then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_APK_ERROR);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end
  else if (not FileExists(AndroidUI.edtInstall.Text))
    and (not DirectoryExists(AndroidUI.edtInstall.Text)) then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_UN_APK);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end
  else if DirectoryExists(AndroidUI.edtInstall.Text) then
  begin

    rId := TAndroidUtils.AndroidMessageBox(STR_HINT, BATCH_UN_INSTALL);
    if rId = mrOk then
    begin
      AndroidUI.mmConsole.ReadOnly := False;
      AndroidUI.mmConsole.Lines.Clear;
      EnableOperation(False);
      AndroidUI.lblInstalling.Visible := True;
      AndroidUI.lblInstalling.Text := STR_UNINSTALLING;
      AndroidUI.Command.BatchUninstallApk;
      AndroidUI.mmConsole.ReadOnly := True;
    end;  
  end;
  *)
end;

procedure TAndroidOperations.btnWin32DeskClick(Sender: TObject);
var
  path: array[0..255] of Char;
begin
  SHGetSpecialFolderPath(0, Path, CSIDL_DESKTOPDIRECTORY, False);
  AndroidUIWin32DirSelector.edtSeledPath.Text := Trim(path);
  AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);
end;

procedure TAndroidOperations.btnWin32DocumentClick(Sender: TObject);
var
  path: array[0..255] of Char;
begin
  SHGetSpecialFolderPath(0, Path, CSIDL_PERSONAL, False);
  AndroidUIWin32DirSelector.edtSeledPath.Text := Trim(path);
  AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);
end;

procedure TAndroidOperations.btnWin32HomeClick(Sender: TObject);
begin
  AndroidUIWin32DirSelector.edtSeledPath.Text := EmptyStr;
  AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);
end;

procedure TAndroidOperations.ChangeUIColor(AColor: string);
begin
  AndroidUI.Picker.Color := vgStrToColor(AColor);
  UIColorChange(nil);
end;

procedure TAndroidOperations.ChangeUIScale(AScale: Single);
begin
  AndroidUI.Root.Scale.X := AScale;
  AndroidUI.Root.Scale.Y := AScale;
  AndroidUI.FUI.Width := Trunc(555 * AScale);
  AndroidUI.FUI.Height := Trunc(360 * AScale);
  AndroidUI.tbScale.Value := AScale;
  UIScaleChange(nil);
end;

procedure TAndroidOperations.ChangeUITransparent(APercent: Single);
begin
  AndroidUI.tbTransparent.Value := APercent;
  UITransparentChange(nil);
end;

constructor TAndroidOperations.Create;
begin
  ADB_EXE := ExtractFilePath(ParamStr(0)) + V_EXENAME;
  ADB_PATH := ExtractFilePath(ParamStr(0));

  AndroidUI.chkSaveApk.IsChecked := AndroidUI.Config.SaveApk;
  chkSaveApkClick(nil);
  AndroidUI.edtSaveApk.Text := AndroidUI.Config.SaveApkPath;
  AndroidUI.edtSendPath.Text := AndroidUI.Config.SendPath;
  AndroidUI.edtReceiveFile.Text := AndroidUI.Config.ReceiveFile;
  AndroidUI.edtReceivePath.Text := AndroidUI.Config.ReceivePath;
end;

destructor TAndroidOperations.Destroy;
begin
  inherited;
end;

procedure TAndroidOperations.EnableOperation(AEnabled: Boolean);
begin
  AndroidUI.tabInstallApk.Enabled := AEnabled;
  AndroidUI.tabSendFile.Enabled := AEnabled;
  AndroidUI.tabRecvFile.Enabled := AEnabled;
  AndroidUI.tabReboot.Enabled := AEnabled;
  AndroidUI.tabHelp.Enabled := AEnabled;
  AndroidUI.tabUISettings.Enabled := AEnabled;

  AndroidUI.layInstallApk.Enabled := AEnabled;
  AndroidUI.laySendFile.Enabled := AEnabled;
  AndroidUI.layRecvFile.Enabled := AEnabled;
  AndroidUI.layReboot.Enabled := AEnabled;
  AndroidUI.layHelp.Enabled := AEnabled;
  AndroidUI.layUISettings.Enabled := AEnabled;

  AndroidUI.btnDevice.Enabled := AEnabled;
  AndroidUI.btnCurrentDevice.Enabled := AEnabled;
  
end;


procedure TAndroidOperations.SwitchLayout(Index: Integer);
begin

  AndroidUI.layInstallApk.Visible := Index = 0;
  AndroidUI.laySendFile.Visible := Index = 1;
  AndroidUI.layRecvFile.Visible := Index = 2;
  AndroidUI.layReboot.Visible := Index = 3;
  AndroidUI.layHelp.Visible := Index = 4;
  AndroidUI.layUISettings.Visible := Index = 5;
end;

procedure TAndroidOperations.TabClick(Sender: TObject);
begin
  SwitchLayout(TvgHudButton(Sender).Tag);
end;

procedure TAndroidOperations.UIActived(Sender: TObject);
begin
  TypeDXUI(Sender).BringToFront;
end;

procedure TAndroidOperations.UIAppStoreEnter(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U2, STR_PNG_EXT);
  AndroidUI.btnAppStore.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UIAPPStoreLeave(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U1, STR_PNG_EXT);
  AndroidUI.btnAppStore.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UIColorChange(Sender: TObject);
begin
  AndroidUI.Quad.Alpha := ((AndroidUI.Window.Fill.SolidColor and $FF000000) shr 24) / $FF;
  AndroidUI.layInstallApk.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.laySendFile.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.layRecvFile.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.layReboot.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.layUISettings.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.layHelp.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  AndroidUI.Window.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);

  AndroidUI.edtColor.Text := vgColorToStr(AndroidUI.Window.Fill.SolidColor);
  AndroidUI.Config.UIColor := AndroidUI.edtColor.Text;
end;

procedure TAndroidOperations.UIKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    TypeDXUI(Sender).ModalResult := mrCancel;
  end
  else if Key = VK_RETURN then
  begin
    TypeDXUI(Sender).ModalResult := mrOk;
  end;
end;

procedure TAndroidOperations.UIKeyDownSQL(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    TypeDXUI(Sender).ModalResult := mrCancel;
  end
  else if Key = VK_F9 then
  begin
    AndroidUI.Operation.btnSQLite3ExecuteClick(nil);
  end;
end;

procedure TAndroidOperations.UILabelMouseEnter(Sender: TObject);
begin
  TvgHudLabel(Sender).Font.Style :=  vgFontUnderline;
end;

procedure TAndroidOperations.UILabelMouseLeave(Sender: TObject);
begin
  TvgHudLabel(Sender).Font.Style :=  vgFontBold;
end;

procedure TAndroidOperations.UIScaleChange(Sender: TObject);
begin
  AndroidUI.Config.UIScale := AndroidUI.tbScale.Value;
  AndroidUI.lblScalePercent.Text := Format(FMT_PERCENT, [Trunc(AndroidUI.Config.UIScale * 100)]);
end;

procedure TAndroidOperations.UISQLiteDumpEnter(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U8, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnDump.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteDumpLeave(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U7, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnDump.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteEnter(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U4, STR_PNG_EXT);
  AndroidUI.btnSqlite.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteLeave(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U3, STR_PNG_EXT);
  AndroidUI.btnSqlite.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteLoadEnter(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U10, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnLoadScript.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteLoadLeave(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U9, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnLoadScript.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteTablesEnter(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U6, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnTables.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UISQLiteTablesLeave(Sender: TObject);
var
  resimg: TResourceStream;
begin
  resimg := TResourceStream.Create(HInstance, IMG_U5, STR_PNG_EXT);
  AndroidUISQLiteSQL.btnTables.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidOperations.UITransparentChange(Sender: TObject);
begin
  AndroidUI.Window.Opacity := AndroidUI.tbTransparent.Value;
  AndroidUI.lblTransparentPercent.Text := Format(FMT_PERCENT, [Trunc(AndroidUI.Window.Opacity * 100)]);
  AndroidUI.Config.UITransparent := AndroidUI.Window.Opacity;
end;

procedure TAndroidOperations.btnReceiveClick(Sender: TObject);
var
  cmdStr: string;
  currSel: char;
begin
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Clear;
  
  if AndroidUI.edtReceiveFile.Text = EmptyStr then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_DIR_FILE);
    AndroidUI.mmConsole.ReadOnly := true;
    Exit;
  end;

  if not DirectoryExists(AndroidUI.edtReceivePath.Text) then
  begin
    AndroidUI.mmConsole.Lines.Add(SEL_DIR_FILE_PATH);
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;

  if AndroidUI.CurrentDevice = EmptyStr then
  begin
    AndroidUI.mmConsole.ReadOnly := True;
    Exit;
  end;
    
  EnableOperation(False);
  AndroidUI.lblReceving.Visible := True;

  currSel := AnsiString(AndroidUI.edtReceiveFile.Text)[Length(AnsiString(AndroidUI.edtReceiveFile.Text))];
  cmdStr := AndroidUI.edtReceiveFile.Text;
  case currSel of
  LINUX_SP,LINUX_LNK,LINUX_FL: cmdStr := LeftStr(cmdStr, Length(cmdStr) - 1);
  end;

  AndroidUI.Command.CopyFileFromDevice(cmdStr, AndroidUI.edtReceivePath.Text);
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidOperations.btnRebootClick(Sender: TObject);
var
  rId: Integer;
  ItemIndex: Integer;
begin

  if AndroidUI.CurrentDevice = EmptyStr then
    Exit;
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.Clear;
  rId := TAndroidUtils.AndroidMessageBox(STR_HINT, REBOOT_QUEST);
  ItemIndex := 0;
  if rId = mrOk then
  begin
    if AndroidUI.rbReboot.IsChecked then
      ItemIndex := 0
    else if AndroidUI.rbBootLoader.IsChecked then
      ItemIndex := 1
    else if AndroidUI.rbRecovery.IsChecked then
      ItemIndex := 2;
    AndroidUI.Command.RebootDevice(ItemIndex);
    AndroidUI.mmConsole.Lines.Add(STR_OP_FINISH);
  end;
  AndroidUI.mmConsole.ReadOnly := True;
end;

procedure TAndroidOperations.BtnBackWin32Click(Sender: TObject);
var
  ori: string;
begin
  if AndroidUIWin32DirSelector.edtSeledPath.Text = EmptyStr then
    Exit;
  AndroidUIWin32DirSelector.lsItems.OnDblClick := nil;
  ori := AndroidUIWin32DirSelector.edtSeledPath.Text;
  if FileExists(AndroidUIWin32DirSelector.edtSeledPath.Text) then
  begin
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      ExtractFilePath(AndroidUIWin32DirSelector.edtSeledPath.Text);
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      LeftStr(AndroidUIWin32DirSelector.edtSeledPath.Text,
      Length(AndroidUIWin32DirSelector.edtSeledPath.Text)-1);
  end;
  AndroidUIWin32DirSelector.edtSeledPath.Text :=
    ExtractFilePath(AndroidUIWin32DirSelector.edtSeledPath.Text);
  if AndroidUIWin32DirSelector.edtSeledPath.Text[Length(AndroidUIWin32DirSelector.edtSeledPath.Text)] = WIN32_SP then
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      LeftStr(AndroidUIWin32DirSelector.edtSeledPath.Text, Length(AndroidUIWin32DirSelector.edtSeledPath.Text)-1);
  if ori = AndroidUIWin32DirSelector.edtSeledPath.Text then
    AndroidUIWin32DirSelector.edtSeledPath.Text := EmptyStr;
  AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);
  AndroidUIWin32DirSelector.lsItems.OnDblClick := AndroidUI.Operation.lsItemWin32DblClick;
end;

procedure TAndroidOperations.btnCurrentDeviceClick(Sender: TObject);
begin
  AndroidUI.CurrentDevice := TAndroidUtils.SelectDevice;
  if AndroidUI.CurrentDevice = EmptyStr then
    AndroidUI.btnCurrentDevice.Text := Format(FMT_DEVICE, [STR_NA])
  else
    AndroidUI.btnCurrentDevice.Text := Format(FMT_DEVICE, [AndroidUI.CurrentDevice]);
end;

procedure TAndroidOperations.btnDeviceClick(Sender: TObject);
begin
  AndroidUI.mmConsole.ReadOnly := False;
  AndroidUI.mmConsole.Lines.clear;
  AndroidUI.Command.CheckDevice;
  AndroidUI.mmConsole.ReadOnly := True;
end;


procedure TAndroidOperations.btnDownAndInstallClick(Sender: TObject);
var
  url: string;
  addr: string;
begin
  url := AndroidUI.Config.Server + SRV_APP + AndroidUIAppDetail.Item.AppFile;
  AndroidUI.Command.DoDownloadApp(url, AndroidUIAppDetail.Item.AppFile);
  addr := AndroidUI.Config.Server + SRV_WSDL;
  try
    ASServiceSoap.GetTASServicesSoap(False, addr).DownloadFile(AndroidUIAppDetail.Item.AppId);
  except
  end;
end;

procedure TAndroidOperations.AppException(Sender: TObject; E: Exception);
begin
  // do nothing
  // ShowMessage(E.Message);
end;

procedure TAndroidOperations.AppStarClick(Sender: TObject);
begin
  ShowMessage(TvgImage(Sender).Parent.ClassName);
end;

procedure TAndroidOperations.btnAppStoreClick(Sender: TObject);
begin
  AndroidUI.Command.ShowAppStore;
end;

procedure TAndroidOperations.btnBackClick(Sender: TObject);
var
  currSel: Char;
begin
  if AndroidUILinuxDirSelector.edtSeledPath.Text = LINUX_SP then
    Exit;
  AndroidUILinuxDirSelector.lsItems.OnDblClick := nil;
  currSel := AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text)
    [Length(AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text))];
  case currSel of
  LINUX_SP,LINUX_LNK:
    AndroidUILinuxDirSelector.edtSeledPath.Text :=
      TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text);
  else
    begin
      AndroidUILinuxDirSelector.edtSeledPath.Text :=
        TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text);
      AndroidUILinuxDirSelector.edtSeledPath.Text :=
        TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text);
    end;
  end;
  AndroidUI.Command.InitDir(AndroidUILinuxDirSelector.edtSeledPath.Text);
  AndroidUILinuxDirSelector.lsItems.OnDblClick := AndroidUI.Operation.lsItemsDblClick;
end;

procedure TAndroidOperations.lblAppMailClick(Sender: TObject);
begin
  ShellExecute(0, SHELL_CMD_OPEN, PChar(
    Format(FMT_APP_MAIL_TO_CMD, [AndroidUIAppDetail.Item.AppMail])), nil, nil, SW_SHOW);
end;

procedure TAndroidOperations.lblAppWebsiteClick(Sender: TObject);
begin
  ShellExecute(0, SHELL_CMD_OPEN, PChar(AndroidUIAppDetail.Item.AppWebsite), nil, nil, SW_SHOW);
end;

procedure TAndroidOperations.lsItemAppDblClick(Sender: TObject);
begin
  AndroidUI.Command.ShowAppDetail(
    TAppStoreItem(AndroidUIAppStore.lsItems.ItemByIndex(AndroidUIAppStore.lsItems.ItemIndex)).ItemData,
    TAppStoreItem(AndroidUIAppStore.lsItems.ItemByIndex(AndroidUIAppStore.lsItems.ItemIndex)).Icon.Bitmap);
end;

procedure TAndroidOperations.lsItemsDblClick(Sender: TObject);
var
  imgIdx: TAndroidItemType;
  currSel: Char;
  cmdStr: string;
begin
  //
  if AndroidUILinuxDirSelector.lsItems.Count = 0 then
    Exit;
  if AndroidUILinuxDirSelector.lsItems.ItemIndex = -1 then
    Exit;
  imgIdx := TAndroidListItem(AndroidUILinuxDirSelector.lsItems.ItemByIndex(
    AndroidUILinuxDirSelector.lsItems.ItemIndex)).ItemType;

  if imgIdx = itBack then
  begin
    btnBackClick(nil);
    Exit;
  end;

  if (imgIdx in [itFile, itLink, itUnknown]) and AndroidUI.OnlySelectDirs then
    Exit;
  AndroidUILinuxDirSelector.lsItems.OnDblClick := nil;
  currSel := AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text)
    [Length(AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text))];

  case currSel of
    LINUX_SP,LINUX_LNK: AndroidUILinuxDirSelector.edtSeledPath.Text :=
      LeftStr(AndroidUILinuxDirSelector.edtSeledPath.Text, Length(AndroidUILinuxDirSelector.edtSeledPath.Text) - 1) +
      LINUX_SP + TAndroidListItem(AndroidUILinuxDirSelector.lsItems.ItemByIndex(
      AndroidUILinuxDirSelector.lsItems.ItemIndex)).ItemName;
  else
    AndroidUILinuxDirSelector.edtSeledPath.Text := TAndroidUtils.ExtractFilePathLinux(AndroidUILinuxDirSelector.edtSeledPath.Text) +
      TAndroidListItem(AndroidUILinuxDirSelector.lsItems.ItemByIndex(
      AndroidUILinuxDirSelector.lsItems.ItemIndex)).ItemName;
  end;

  currSel := AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text)
    [Length(AnsiString(AndroidUILinuxDirSelector.edtSeledPath.Text))];
  case currSel of
    LINUX_SP,LINUX_LNK:
      begin
        cmdStr := LeftStr(AndroidUILinuxDirSelector.edtSeledPath.Text, Length(AndroidUILinuxDirSelector.edtSeledPath.Text) - 1);
        AndroidUI.Command.InitDir(cmdStr);
      end;
  end;
  AndroidUILinuxDirSelector.lsItems.OnDblClick := lsItemsDblClick;
end;

procedure TAndroidOperations.lsItemWin32DblClick(Sender: TObject);
var
  imgIdx: TAndroidItemType;
begin
  if AndroidUIWin32DirSelector.lsItems.Count = 0 then
    Exit;
  if AndroidUIWin32DirSelector.lsItems.ItemIndex = -1 then
    Exit;
  imgIdx := TWin32ListItem(AndroidUIWin32DirSelector.lsItems.ItemByIndex(
    AndroidUIWin32DirSelector.lsItems.ItemIndex)).ItemType;

  if imgIdx = itBack then
  begin
    BtnBackWin32Click(nil);
    Exit;
  end;

  if (imgIdx in [itFile, itLink, itUnknown]) and AndroidUI.OnlySelectDirs then
    Exit;

  if FileExists(AndroidUIWin32DirSelector.edtSeledPath.Text) then
  begin
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      ExtractFilePath(AndroidUIWin32DirSelector.edtSeledPath.Text);
    AndroidUIWin32DirSelector.edtSeledPath.Text := AndroidUIWin32DirSelector.edtSeledPath.Text +
      TWin32ListItem(AndroidUIWin32DirSelector.lsItems.ItemByIndex(
      AndroidUIWin32DirSelector.lsItems.ItemIndex)).ItemName;
    if not FileExists(AndroidUIWin32DirSelector.edtSeledPath.Text) then
    begin
      if DirectoryExists(AndroidUIWin32DirSelector.edtSeledPath.Text) then
        AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);
    end;  
    Exit;
  end;   
  AndroidUIWin32DirSelector.lsItems.OnDblClick := nil;
  if AndroidUIWin32DirSelector.edtSeledPath.Text = EmptyStr then
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      TWin32ListItem(AndroidUIWin32DirSelector.lsItems.ItemByIndex(
      AndroidUIWin32DirSelector.lsItems.ItemIndex)).ItemName
  else
    AndroidUIWin32DirSelector.edtSeledPath.Text :=
      AndroidUIWin32DirSelector.edtSeledPath.Text + WIN32_SP +
      TWin32ListItem(AndroidUIWin32DirSelector.lsItems.ItemByIndex(
      AndroidUIWin32DirSelector.lsItems.ItemIndex)).ItemName;
  if FileExists(AndroidUIWin32DirSelector.edtSeledPath.Text) then
  begin
    AndroidUIWin32DirSelector.lsItems.OnDblClick := lsItemWin32DblClick;
    Exit;
  end;  

  AndroidUI.Command.InitWin32Dir(AndroidUIWin32DirSelector.edtSeledPath.Text);

end;

procedure TAndroidOperations.edtReceiveFileChange(Sender: TObject);
begin
  AndroidUI.Config.ReceiveFile := AndroidUI.edtReceiveFile.Text;
end;

procedure TAndroidOperations.edtReceiveFileButtonClick(Sender: TObject);
var
  currSel: Char;
  dir: string;
begin
  if AndroidUI.CurrentDevice = EmptyStr then
    Exit;
  currSel := AnsiString(AndroidUI.edtReceiveFile.Text)
    [Length(AnsiString(AndroidUI.edtReceiveFile.Text))];
  case currSel of
    LINUX_SP,LINUX_LNK: dir := AndroidUI.edtReceiveFile.Text;
  else
    dir := TAndroidUtils.ExtractFilePathLinux(AndroidUI.edtReceiveFile.Text);
  end;

  if dir[Length(dir)] <> LINUX_SP then
    dir := dir + LINUX_SP;

  AndroidUI.edtReceiveFile.Text := TAndroidUtils.SelectLinuxFile(STR_DIR_FILE, dir, False, False);
  AndroidUI.Config.ReceiveFile := AndroidUI.edtReceiveFile.Text;
end;

end.
