unit UI;

{$I AndroidLanguage.inc} 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, vg_scene, vg_controls, vg_layouts, vg_tabcontrol, vg_textbox,
  vg_objects, vg_colors, vg_memo, Operations, Commands, Config, StringConsts,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidHelperUI = class(TAndroidUIDialogBase)
  private
    FOperation: TAndroidOperations;
    FCurrentDevice: string;
    FConfig: TConfig;
    FCommand: TAndroidCommands;
    FOnlySelectDirs: Boolean;
    FOnlyFile: Boolean;
    FSQLite3Name: string;
  public
    // Main UI Elements
    btnAppStore: TvgImage;
    btnSqlite: TvgImage;

    // Global UI Elements
    btnDevice: TvgHudButton;
    btnCurrentDevice: TvgHudButton;
    tabInstallApk: TvgHudButton;
    tabSendFile: TvgHudButton;
    tabRecvFile: TvgHudButton;
    tabReboot: TvgHudButton;
    tabHelp: TvgHudButton;
    tabUISettings: TvgHudButton;

    layInstallApk: TvgHudWindow;
    laySendFile: TvgHudWindow;
    layRecvFile: TvgHudWindow;
    layReboot: TvgHudWindow;
    layHelp: TvgHudWindow;
    layUISettings: TvgHudWindow;

    mmConsole: TvgHudMemo;

    // Install APK
    lblInstall: TvgHudLabel;
    lblApkPath: TvgHudLabel;
    edtInstall: TvgHudTextBox;
    edtSaveApk: TvgHudTextBox;
    btnInstall: TvgHudButton;
    btnUninstall: TvgHudButton;
    lblInstalling: TvgHudLabel;
    chkSaveApk: TvgHudCheckBox;
    chkReinstall: TvgHudCheckBox;
    btnInstallB: TvgHudButton;
    btnSaveApkB: TvgHudButton;

    // Send File
    edtSendFile: TvgHudTextBox;
    edtSendPath: TvgHudTextBox;
    lblSendFile: TvgHudLabel;
    lblSendPath: TvgHudLabel;
    lblSending: TvgHudLabel;
    btnSend: TvgHudButton;
    btnSendFileB2: TvgHudButton;
    btnSendPathB: TvgHudButton;

    // Receive File
    edtReceiveFile: TvgHudTextBox;
    edtReceivePath: TvgHudTextBox;
    lblReceiveFile: TvgHudLabel;
    lblReceivePath: TvgHudLabel;
    lblReceving: TvgHudLabel;
    btnReceive: TvgHudButton;
    btnReceiveFileB: TvgHudButton;
    btnReceivePathB: TvgHudButton;

    // Reboot
    gbReboot: TvgHudGroupBox;
    rbReboot: TvgHudRadioButton;
    rbBootLoader: TvgHudRadioButton;
    rbRecovery: TvgHudRadioButton;
    btnReboot: TvgHudButton;
    lblHint: TvgHudLabel;

    // Help
    lblHelp: TvgHudLabel;
    lblNeedRoot: TvgHudLabel;
    btnSurvey: TvgHudButton;
    imgAbout: TvgImage;

    // UI Settings
    Quad: TvgColorQuad;
    Picker: TvgColorPicker;
    lblColor: TvgHudLabel;
    lblTransparent: TvgHudLabel;
    lblScale: TvgHudLabel;
    edtColor: TvgHudTextBox;
    tbTransparent: TvgHudTrackBar;
    tbScale: TvgHudTrackBar;
    lblTransparentPercent: TvgHudLabel;
    lblScalePercent: TvgHudLabel;
  protected
    procedure InitUI; override;
    procedure InitInstallApkTab;
    procedure InitSendFileTab;
    procedure InitRecvFileTab;
    procedure InitRebootTab;
    procedure InitHelpTab;
    procedure InitUISettingsTab;
    procedure InitTitleButtons;
  public
    constructor CreateApplication; override;
    procedure CreateOperation;
    destructor Destroy; override;
    procedure ShowUI;
  public
    property Operation: TAndroidOperations read FOperation write FOperation;
    property CurrentDevice: string read FCurrentDevice write FCurrentDevice;
    property Config: TConfig read FConfig write FConfig;
    property Command: TAndroidCommands read FCommand write FCommand;
    property OnlySelectDirs: Boolean read FOnlySelectDirs write FOnlySelectDirs;
    property OnlyFile: Boolean read FOnlyFile write FOnlyFile;
    property SQLite3Name: string read FSQLite3Name write FSQLite3Name;
  end;

var
  AndroidUI: TAndroidHelperUI;

implementation

{ TAndroidHelperUI }

constructor TAndroidHelperUI.CreateApplication;
begin
  FConfig:= TConfig.Create;
  FConfig.LoadFromIni(ChangeFileExt(ParamStr(0), V_INI));
  // use application.CreateForm instead of Create directly
  // Application will set a base message loop on the main form.
  inherited;
  SetSize(555, 360);
  InitInstallApkTab;
  InitSendFileTab;
  InitRecvFileTab;
  InitRebootTab;
  InitUISettingsTab;
  InitHelpTab;
  InitTitleButtons;
end;

procedure TAndroidHelperUI.CreateOperation;
begin
  FOperation := TAndroidOperations.Create;
  FCommand := TAndroidCommands.Create;
end;

destructor TAndroidHelperUI.Destroy;
begin
  FOperation.Free;
  FConfig.SaveToIni(ChangeFileExt(ParamStr(0), V_INI));
  FConfig.Free;
  FCommand.Free;
  inherited;
end;

procedure TAndroidHelperUI.InitHelpTab;
var
  img: TResourceStream;
begin
  lblHelp:= TvgHudLabel.Create(layHelp);
  lblHelp.Parent := layHelp;
  lblHelp.Position.X := 22;
  lblHelp.Position.Y := 22;
  lblHelp.Width := 375;
  lblHelp.Height := 85;
  lblHelp.Font.Style := vgFontBold;
  lblHelp.TextAlign := vgTextAlignNear;
  lblHelp.Text := STR_HELP;

  lblNeedRoot:= TvgHudLabel.Create(layHelp);
  lblNeedRoot.Parent := layHelp;
  lblNeedRoot.Position.X := 110;
  lblNeedRoot.Position.Y := 115;
  lblNeedRoot.Width := 290;
  lblNeedRoot.Height := 15;
  lblNeedRoot.Font.Style := vgFontBold;
  lblNeedRoot.Text := STR_NEED_ROOT;
  
  btnSurvey:= TvgHudButton.Create(layHelp);
  btnSurvey.Parent := layHelp;
  btnSurvey.Position.X := 22;
  btnSurvey.Position.Y := 112;
  btnSurvey.Width := 80;
  btnSurvey.Height := 22;
  btnSurvey.Font.Style := vgFontBold;
  btnSurvey.Text := STR_JOIN_SURVEY;
  btnSurvey.OnClick := FOperation.btnSurveyClick;

  imgAbout:= TvgImage.Create(layHelp);
  imgAbout.Parent := layHelp;
  imgAbout.Position.X := 405;
  imgAbout.Position.Y := 22;
  imgAbout.Width := 100;
  imgAbout.Height := 75;
  imgAbout.WrapMode := vgImageOriginal;
  img := TResourceStream.Create(HInstance, IMG_P5, STR_PNG_EXT);
  imgAbout.Bitmap.LoadFromStream(img);
  img.Free;
  imgAbout.Cursor := crHandPoint;
  imgAbout.OnClick := FOperation.btnImageAboutClick;
end;

procedure TAndroidHelperUI.InitInstallApkTab;
begin
  lblInstall:= TvgHudLabel.Create(layInstallApk);
  lblInstall.Parent := layInstallApk;
  lblInstall.Position.X := 22;
  lblInstall.Position.Y := 22;
  lblInstall.Width := 120;
  lblInstall.Height := 15;
  lblInstall.TextAlign := vgTextAlignNear;
  lblInstall.Text := STR_APK_TO_INSTALL;
  lblInstall.Font.Style := vgFontBold;

  edtInstall:= TvgHudTextBox.Create(layInstallApk);
  edtInstall.Parent := layInstallApk;
  edtInstall.Position.X := 135;
  edtInstall.Position.Y := 20;
  edtInstall.Width := 330;
  edtInstall.Height := 21;
  edtInstall.Font.Style := vgFontBold;

  btnInstallB:= TvgHudButton.Create(layInstallApk);
  btnInstallB.Parent := layInstallApk;
  btnInstallB.Position.X := 470;
  btnInstallB.Position.Y := 19;
  btnInstallB.Width := 32;
  btnInstallB.Height := 22;
  btnInstallB.Text := STR_DOT;
  btnInstallB.Font.Style := vgFontBold;
  btnInstallB.OnClick := FOperation.edtInstallButtonClick;

  chkSaveApk:= TvgHudCheckBox.Create(layInstallApk);
  chkSaveApk.Parent := layInstallApk;
  chkSaveApk.Position.X := 22;
  chkSaveApk.Position.Y := 50;
  chkSaveApk.Width := 250;
  chkSaveApk.Height := 19;
  chkSaveApk.TextAlign := vgTextAlignNear;
  chkSaveApk.Text := STR_SAVE_APK_TO_DEVICE;
  chkSaveApk.Font.Style := vgFontBold;
  chkSaveApk.OnChange := FOperation.chkSaveApkClick;

  chkReinstall:= TvgHudCheckBox.Create(layInstallApk);
  chkReinstall.Parent := layInstallApk;
  chkReinstall.Position.X := 270;
  chkReinstall.Position.Y := 50;
  chkReinstall.Width := 250;
  chkReinstall.Height := 19;
  chkReinstall.TextAlign := vgTextAlignNear;
  chkReinstall.Text := STR_EXIST_REINSTALL;
  chkReinstall.Font.Style := vgFontBold;

  lblApkPath:= TvgHudLabel.Create(layInstallApk);
  lblApkPath.Parent := layInstallApk;
  lblApkPath.Position.X := 22;
  lblApkPath.Position.Y := 80;
  lblApkPath.Width := 110;
  lblApkPath.Height := 15;
  lblApkPath.TextAlign := vgTextAlignNear;
  lblApkPath.Text := STR_APK_SAVE_PATH;
  lblApkPath.Font.Style := vgFontBold;

  edtSaveApk:= TvgHudTextBox.Create(layInstallApk);
  edtSaveApk.Parent := layInstallApk;
  edtSaveApk.Position.X := 135;
  edtSaveApk.Position.Y := 78;
  edtSaveApk.Width := 330;
  edtSaveApk.Height := 21;
  edtSaveApk.Font.Style := vgFontBold;
  edtSaveApk.OnChange := FOperation.edtSaveApkChange;

  btnSaveApkB:= TvgHudButton.Create(layInstallApk);
  btnSaveApkB.Parent := layInstallApk;
  btnSaveApkB.Position.X := 470;
  btnSaveApkB.Position.Y := 77;
  btnSaveApkB.Width := 32;
  btnSaveApkB.Height := 22;
  btnSaveApkB.Text := STR_DOT;
  btnSaveApkB.Font.Style := vgFontBold;
  btnSaveApkB.OnClick := FOperation.edtSaveApkButtonClick;

  lblInstalling:= TvgHudLabel.Create(layInstallApk);
  lblInstalling.Parent := layInstallApk;
  lblInstalling.Position.X := 22;
  lblInstalling.Position.Y := 120;
  lblInstalling.Width := 250;
  lblInstalling.Height := 15;
  lblInstalling.TextAlign := vgTextAlignNear;
  lblInstalling.Font.Style := vgFontBold;

  btnInstall:= TvgHudButton.Create(layInstallApk);
  btnInstall.Parent := layInstallApk;
  btnInstall.Position.X := 215;
  btnInstall.Position.Y := 115;
  btnInstall.Width := 140;
  btnInstall.Height := 24;
  btnInstall.Text := STR_INSTALL;
  btnInstall.Font.Style := vgFontBold;
  btnInstall.OnClick := FOperation.btnInstallClick;

  btnUninstall:= TvgHudButton.Create(layInstallApk);
  btnUninstall.Parent := layInstallApk;
  btnUninstall.Position.X := 360;
  btnUninstall.Position.Y := 115;
  btnUninstall.Width := 140;
  btnUninstall.Height := 24;
  btnUninstall.Text := STR_UNINSTALL;
  btnUninstall.Font.Style := vgFontBold;
  btnUninstall.OnClick := FOperation.btnUninstallClick;

  lblInstalling.Visible := False;
end;

procedure TAndroidHelperUI.InitRebootTab;
begin
  gbReboot:= TvgHudGroupBox.Create(layReboot);
  gbReboot.Parent := layReboot;
  gbReboot.Position.X := 30;
  gbReboot.Position.Y := 24;
  gbReboot.Width := 470;
  gbReboot.Height := 65;
  gbReboot.Text := STR_REBOOT_OPT;
  gbReboot.Font.Style := vgFontBold;

  btnReboot:= TvgHudButton.Create(layReboot);
  btnReboot.Parent := layReboot;
  btnReboot.Position.X := 330;
  btnReboot.Position.Y := 115;
  btnReboot.Width := 170;
  btnReboot.Height := 24;
  btnReboot.Text := STR_REBOOT;
  btnReboot.Font.Style := vgFontBold;
  btnReboot.OnClick := FOperation.btnRebootClick;

  rbReboot := TvgHudRadioButton.Create(gbReboot);
  rbReboot.Parent := gbReboot;
  rbReboot.Position.X := 20;
  rbReboot.Position.Y := 24;
  rbReboot.Width := 120;
  rbReboot.Height := 19;
  rbReboot.Text := STR_NORMAL_REBOOT;
  rbReboot.Font.Style := vgFontBold;
  rbReboot.IsChecked := True;

  rbBootLoader := TvgHudRadioButton.Create(gbReboot);
  rbBootLoader.Parent := gbReboot;
  rbBootLoader.Position.X := 160;
  rbBootLoader.Position.Y := 24;
  rbBootLoader.Width := 120;
  rbBootLoader.Height := 19;
  rbBootLoader.Text := STR_BOOTLOADER;
  rbBootLoader.Font.Style := vgFontBold;

  rbRecovery := TvgHudRadioButton.Create(gbReboot);
  rbRecovery.Parent := gbReboot;
  rbRecovery.Position.X := 300;
  rbRecovery.Position.Y := 24;
  rbRecovery.Width := 120;
  rbRecovery.Height := 19;
  rbRecovery.Text := STR_RECOVERY;
  rbRecovery.Font.Style := vgFontBold;
end;

procedure TAndroidHelperUI.InitRecvFileTab;
begin
  lblReceiveFile:= TvgHudLabel.Create(layRecvFile);
  lblReceiveFile.Parent := layRecvFile;
  lblReceiveFile.Position.X := 22;
  lblReceiveFile.Position.Y := 22;
  lblReceiveFile.Width := 110;
  lblReceiveFile.Height := 15;
  lblReceiveFile.TextAlign := vgTextAlignNear;
  lblReceiveFile.Text := STR_SEL_FILE_DIR;
  lblReceiveFile.Font.Style := vgFontBold;

  edtReceiveFile:= TvgHudTextBox.Create(layRecvFile);
  edtReceiveFile.Parent := layRecvFile;
  edtReceiveFile.Position.X := 135;
  edtReceiveFile.Position.Y := 20;
  edtReceiveFile.Width := 330;
  edtReceiveFile.Height := 21;
  edtReceiveFile.Font.Style := vgFontBold;
  edtReceiveFile.OnChange := FOperation.edtReceiveFileChange;

  lblReceivePath:= TvgHudLabel.Create(layRecvFile);
  lblReceivePath.Parent := layRecvFile;
  lblReceivePath.Position.X := 22;
  lblReceivePath.Position.Y := 53;
  lblReceivePath.Width := 110;
  lblReceivePath.Height := 15;
  lblReceivePath.TextAlign := vgTextAlignNear;
  lblReceivePath.Text := STR_PC_SAVE_PATH;
  lblReceivePath.Font.Style := vgFontBold;

  edtReceivePath:= TvgHudTextBox.Create(layRecvFile);
  edtReceivePath.Parent := layRecvFile;
  edtReceivePath.Position.X := 135;
  edtReceivePath.Position.Y := 50;
  edtReceivePath.Width := 330;
  edtReceivePath.Height := 21;
  edtReceivePath.Font.Style := vgFontBold;
  edtReceivePath.OnChange := FOperation.edtReceivePathChange;

  lblReceving:= TvgHudLabel.Create(layRecvFile);
  lblReceving.Parent := layRecvFile;
  lblReceving.Position.X := 22;
  lblReceving.Position.Y := 120;
  lblReceving.Width := 250;
  lblReceving.Height := 15;
  lblReceving.Text := STR_RECEIVING;
  lblReceving.TextAlign := vgTextAlignNear;
  lblReceving.Font.Style := vgFontBold;

  btnReceive:= TvgHudButton.Create(layRecvFile);
  btnReceive.Parent := layRecvFile;
  btnReceive.Position.X := 330;
  btnReceive.Position.Y := 115;
  btnReceive.Width := 170;
  btnReceive.Height := 24;
  btnReceive.Text := STR_RECEIVE_TO_PC;
  btnReceive.Font.Style := vgFontBold;
  btnReceive.OnClick := FOperation.btnReceiveClick;

  btnReceiveFileB:= TvgHudButton.Create(layRecvFile);
  btnReceiveFileB.Parent := layRecvFile;
  btnReceiveFileB.Position.X := 470;
  btnReceiveFileB.Position.Y := 19;
  btnReceiveFileB.Width := 32;
  btnReceiveFileB.Height := 22;
  btnReceiveFileB.Text := STR_DOT;
  btnReceiveFileB.Font.Style := vgFontBold;
  btnReceiveFileB.OnClick := FOperation.edtReceiveFileButtonClick;

  btnReceivePathB := TvgHudButton.Create(layRecvFile);
  btnReceivePathB.Parent := layRecvFile;
  btnReceivePathB.Position.X := 470;
  btnReceivePathB.Position.Y := 49;
  btnReceivePathB.Width := 32;
  btnReceivePathB.Height := 22;
  btnReceivePathB.Text := STR_DOT;
  btnReceivePathB.Font.Style := vgFontBold;
  btnReceivePathB.OnClick := FOperation.edtReceivePathButtonClick;

  lblReceving.Visible := False;
end;

procedure TAndroidHelperUI.InitSendFileTab;
begin
  lblSendFile:= TvgHudLabel.Create(laySendFile);
  lblSendFile.Parent := laySendFile;
  lblSendFile.Position.X := 22;
  lblSendFile.Position.Y := 22;
  lblSendFile.Width := 110;
  lblSendFile.Height := 15;
  lblSendFile.TextAlign := vgTextAlignNear;
  lblSendFile.Text := STR_SEL_FILE_SEND;
  lblSendFile.Font.Style := vgFontBold;

  edtSendFile:= TvgHudTextBox.Create(laySendFile);
  edtSendFile.Parent := laySendFile;
  edtSendFile.Position.X := 135;
  edtSendFile.Position.Y := 20;
  edtSendFile.Width := 330;      // 295
  edtSendFile.Height := 21;
  edtSendFile.Font.Style := vgFontBold;

  lblSendPath:= TvgHudLabel.Create(laySendFile);
  lblSendPath.Parent := laySendFile;
  lblSendPath.Position.X := 22;
  lblSendPath.Position.Y := 53;
  lblSendPath.Width := 110;
  lblSendPath.Height := 15;
  lblSendPath.TextAlign := vgTextAlignNear;
  lblSendPath.Text := STR_MOBILE_SAVE_PATH;
  lblSendPath.Font.Style := vgFontBold;

  edtSendPath:= TvgHudTextBox.Create(laySendFile);
  edtSendPath.Parent := laySendFile;
  edtSendPath.Position.X := 135;
  edtSendPath.Position.Y := 50;
  edtSendPath.Width := 330;
  edtSendPath.Height := 21;
  edtSendPath.Font.Style := vgFontBold;
  edtSendPath.OnChange := FOperation.edtSendPathChange;

  lblSending:= TvgHudLabel.Create(laySendFile);
  lblSending.Parent := laySendFile;
  lblSending.Position.X := 22;
  lblSending.Position.Y := 120;
  lblSending.Width := 250;
  lblSending.Height := 15;
  lblSending.Text := STR_SENDING;
  lblSending.TextAlign := vgTextAlignNear;
  lblSending.Font.Style := vgFontBold;

  btnSend:= TvgHudButton.Create(laySendFile);
  btnSend.Parent := laySendFile;
  btnSend.Position.X := 330;
  btnSend.Position.Y := 115;
  btnSend.Width := 170;
  btnSend.Height := 24;
  btnSend.Text := STR_SEND_TO_MOBILE;
  btnSend.Font.Style := vgFontBold;
  btnSend.OnClick := FOperation.btnSendClick;

  btnSendFileB2:= TvgHudButton.Create(laySendFile);
  btnSendFileB2.Parent := laySendFile;
  btnSendFileB2.Position.X := 470;
  btnSendFileB2.Position.Y := 19;
  btnSendFileB2.Width := 32;
  btnSendFileB2.Height := 22;
  btnSendFileB2.Text := STR_DOT;
  btnSendFileB2.Font.Style := vgFontBold;
  btnSendFileB2.OnClick := FOperation.edtSendFileButtonClick;

  btnSendPathB := TvgHudButton.Create(laySendFile);
  btnSendPathB.Parent := laySendFile;
  btnSendPathB.Position.X := 470;
  btnSendPathB.Position.Y := 49;
  btnSendPathB.Width := 32;
  btnSendPathB.Height := 22;
  btnSendPathB.Text := STR_DOT;
  btnSendPathB.Font.Style := vgFontBold;
  btnSendPathB.OnClick := FOperation.edtSendPathButtonClick;

  lblSending.Visible := False;
end;

procedure TAndroidHelperUI.InitTitleButtons;
var
  img: TResourceStream;
begin
  btnAppStore:= TvgImage.Create(Window);
  btnAppStore.Parent := Window;
  btnAppStore.Position.X := Root.Width - 55;
  btnAppStore.Position.Y := 14;
  btnAppStore.Width := 18;
  btnAppStore.Height := 18;
  btnAppStore.WrapMode := vgImageStretch;
  img := TResourceStream.Create(HInstance, IMG_U1, STR_PNG_EXT);
  btnAppStore.Bitmap.LoadFromStream(img);
  img.Free;
  btnAppStore.OnMouseEnter := FOperation.UIAppStoreEnter;
  btnAppStore.OnMouseLeave := FOperation.UIAPPStoreLeave;
  btnAppStore.OnClick := FOperation.btnAppStoreClick;
  btnAppStore.ShowHint := True;
  btnAppStore.Hint := STR_HINT_APP_STORE;

  btnSqlite:= TvgImage.Create(Window);
  btnSqlite.Parent := Window;
  btnSqlite.Position.X := Root.Width - 74;
  btnSqlite.Position.Y := 15;
  btnSqlite.Width := 18;
  btnSqlite.Height := 18;
  btnSqlite.WrapMode := vgImageStretch;
  img := TResourceStream.Create(HInstance, IMG_U3, STR_PNG_EXT);
  btnSqlite.Bitmap.LoadFromStream(img);
  img.Free;
  btnSqlite.OnMouseEnter := FOperation.UISQLiteEnter;
  btnSqlite.OnMouseLeave := FOperation.UISQLiteLeave;
  btnSqlite.OnClick := FOperation.btnSQLiteClick;
  btnSqlite.ShowHint := True;
  btnSqlite.Hint := STR_HINT_SQLITE;
end;

procedure TAndroidHelperUI.InitUI;
begin
  inherited;
  Window.Text := STR_TITLE;
  // Window.Opacity := 0.85;

  btnDevice:= TvgHudButton.Create(Window);
  btnDevice.Parent := Window;
  btnDevice.Position.X := 22;
  btnDevice.Position.Y := 40;
  btnDevice.Width := 250;
  btnDevice.Height := 22;
  btnDevice.Text := STR_CHECK_CONNECTED;
  btnDevice.Font.Style := vgFontBold;
  btnDevice.OnClick := FOperation.btnDeviceClick;
  
  btnCurrentDevice:= TvgHudButton.Create(Window);
  btnCurrentDevice.Parent := Window;
  btnCurrentDevice.Position.X := 280;
  btnCurrentDevice.Position.Y := 40;
  btnCurrentDevice.Width := 250;
  btnCurrentDevice.Height := 22;
  btnCurrentDevice.Font.Style := vgFontBold;
  btnCurrentDevice.OnClick := FOperation.btnCurrentDeviceClick;

  tabInstallApk:= TvgHudButton.Create(Window);
  tabInstallApk.Parent := Window;
  tabInstallApk.Position.X := 22;
  tabInstallApk.Position.Y := 70;
  tabInstallApk.Width := 80;
  tabInstallApk.Height := 22;
  tabInstallApk.Text := STR_TAB_INSTALL_APK;
  tabInstallApk.Font.Style := vgFontBold;
  tabInstallApk.Tag := 0;
  tabInstallApk.OnClick := FOperation.TabClick;

  tabSendFile:= TvgHudButton.Create(Window);
  tabSendFile.Parent := Window;
  tabSendFile.Position.X := 105;
  tabSendFile.Position.Y := 70;
  tabSendFile.Width := 80;
  tabSendFile.Height := 22;
  tabSendFile.Text := STR_TAB_SEND_FILE;
  tabSendFile.Font.Style := vgFontBold;
  tabSendFile.Tag := 1;
  tabSendFile.OnClick := FOperation.TabClick;

  tabRecvFile:= TvgHudButton.Create(Window);
  tabRecvFile.Parent := Window;
  tabRecvFile.Position.X := 188;
  tabRecvFile.Position.Y := 70;
  tabRecvFile.Width := 80;
  tabRecvFile.Height := 22;
  tabRecvFile.Text := STR_TAB_RECEIVE_FILE;
  tabRecvFile.Font.Style := vgFontBold;
  tabRecvFile.Tag := 2;
  tabRecvFile.OnClick := FOperation.TabClick;

  tabReboot:= TvgHudButton.Create(Window);
  tabReboot.Parent := Window;
  tabReboot.Position.X := 271;
  tabReboot.Position.Y := 70;
  tabReboot.Width := 80;
  tabReboot.Height := 22;
  tabReboot.Text := STR_TAB_REBOOT;
  tabReboot.Font.Style := vgFontBold;
  tabReboot.Tag := 3;
  tabReboot.OnClick := FOperation.TabClick;

  tabUISettings:= TvgHudButton.Create(Window);
  tabUISettings.Parent := Window;
  tabUISettings.Position.X := 354;
  tabUISettings.Position.Y := 70;
  tabUISettings.Width := 80;
  tabUISettings.Height := 22;
  tabUISettings.Text := STR_TAB_UI_SETTING;
  tabUISettings.Font.Style := vgFontBold;
  tabUISettings.Tag := 5;
  tabUISettings.OnClick := FOperation.TabClick;

  tabHelp:= TvgHudButton.Create(Window);
  tabHelp.Parent := Window;
  tabHelp.Position.X := 437;
  tabHelp.Position.Y := 70;
  tabHelp.Width := 80;
  tabHelp.Height := 22;
  tabHelp.Text := STR_TAB_HELP;
  tabHelp.Font.Style := vgFontBold;
  tabHelp.Tag := 4;
  tabHelp.OnClick := FOperation.TabClick;

  layInstallApk:= TvgHudWindow.Create(Window);
  layInstallApk.Parent := Window;
  layInstallApk.ShowCaption := False;
  layInstallApk.ShowCloseButton := False;
  layInstallApk.ShowSizeGrip := False;
  layInstallApk.Position.X := 12;
  layInstallApk.Position.Y := 85;
  layInstallApk.Width := 530;
  layInstallApk.Height := 160;
  layInstallApk.Opacity := 0.85;

  laySendFile:= TvgHudWindow.Create(Window);
  laySendFile.Parent := Window;
  laySendFile.ShowCaption := False;
  laySendFile.ShowCloseButton := False;
  laySendFile.ShowSizeGrip := False;
  laySendFile.Position.X := 12;
  laySendFile.Position.Y := 85;
  laySendFile.Width := 530;
  laySendFile.Height := 160;
  laySendFile.Opacity := 0.85;

  layRecvFile:= TvgHudWindow.Create(Window);
  layRecvFile.Parent := Window;
  layRecvFile.ShowCaption := False;
  layRecvFile.ShowCloseButton := False;
  layRecvFile.ShowSizeGrip := False;
  layRecvFile.Position.X := 12;
  layRecvFile.Position.Y := 85;
  layRecvFile.Width := 530;
  layRecvFile.Height := 160;
  layRecvFile.Opacity := 0.85;

  layReboot:= TvgHudWindow.Create(Window);
  layReboot.Parent := Window;
  layReboot.ShowCaption := False;
  layReboot.ShowCloseButton := False;
  layReboot.ShowSizeGrip := False;
  layReboot.Position.X := 12;
  layReboot.Position.Y := 85;
  layReboot.Width := 530;
  layReboot.Height := 160;
  layReboot.Opacity := 0.85;

  layUISettings:= TvgHudWindow.Create(Window);
  layUISettings.Parent := Window;
  layUISettings.ShowCaption := False;
  layUISettings.ShowCloseButton := False;
  layUISettings.ShowSizeGrip := False;
  layUISettings.Position.X := 12;
  layUISettings.Position.Y := 85;
  layUISettings.Width := 530;
  layUISettings.Height := 160;
  layUISettings.Opacity := 0.85;

  layHelp:= TvgHudWindow.Create(Window);
  layHelp.Parent := Window;
  layHelp.ShowCaption := False;
  layHelp.ShowCloseButton := False;
  layHelp.ShowSizeGrip := False;
  layHelp.Position.X := 12;
  layHelp.Position.Y := 85;
  layHelp.Width := 530;
  layHelp.Height := 160;
  layHelp.Opacity := 0.85;

  mmConsole:= TvgHudMemo.Create(Window);
  mmConsole.Parent := Window;
  mmConsole.Position.X := 22;
  mmConsole.Position.Y := 240;
  mmConsole.Width := 507;
  mmConsole.Height := 95;
  mmConsole.Font.Style := vgFontBold;
  mmConsole.ScrollBars := ssVertical;
  mmConsole.HideCaret;
  mmConsole.ReadOnly := True;
end;

procedure TAndroidHelperUI.InitUISettingsTab;
begin
  Picker:= TvgColorPicker.Create(layUISettings);
  Picker.Parent := layUISettings;
  Picker.Position.X := 22;
  Picker.Position.Y := 22;
  Picker.Width := 22;
  Picker.Height := 115;

  Quad:= TvgColorQuad.Create(layUISettings);
  Quad.Parent := layUISettings;
  Quad.Position.X := 42;
  Quad.Position.Y := 22;
  Quad.Width := 200;
  Quad.Height := 115;

  Picker.ColorQuad := Quad;
  Quad.OnChange := FOperation.UIColorChange;

  lblColor:= TvgHudLabel.Create(layUISettings);
  lblColor.Parent := layUISettings;
  lblColor.Position.X := 255;
  lblColor.Position.Y := 40;
  lblColor.Width := 70;
  lblColor.Height := 15;
  lblColor.Font.Style := vgFontBold;
  lblColor.Text := STR_UI_COLOR;
  
  lblTransparent:= TvgHudLabel.Create(layUISettings);
  lblTransparent.Parent := layUISettings;
  lblTransparent.Position.X := 255;
  lblTransparent.Position.Y := 70;
  lblTransparent.Width := 70;
  lblTransparent.Height := 15;
  lblTransparent.Font.Style := vgFontBold;
  lblTransparent.Text := STR_UI_TRANSPARENT;

  lblScale := TvgHudLabel.Create(layUISettings);
  lblScale.Parent := layUISettings;
  lblScale.Position.X := 255;
  lblScale.Position.Y := 100;
  lblScale.Width := 70;
  lblScale.Height := 15;
  lblScale.Font.Style := vgFontBold;
  lblScale.Text := STR_UI_SCALE;
  
  edtColor:= TvgHudTextBox.Create(layUISettings);
  edtColor.Parent := layUISettings;
  edtColor.Position.X := 320;
  edtColor.Position.Y := 38;
  edtColor.Width := 150;
  edtColor.Height := 21;
  edtColor.ReadOnly := True;
  
  tbTransparent:= TvgHudTrackBar.Create(layUISettings);
  tbTransparent.Parent := layUISettings;
  tbTransparent.Position.X := 320;
  tbTransparent.Position.Y := 70;
  tbTransparent.Width := 150;
  tbTransparent.Height := 15;
  tbTransparent.OnChange := FOperation.UITransparentChange;

  tbScale := TvgHudTrackBar.Create(layUISettings);
  tbScale.Parent := layUISettings;
  tbScale.Position.X := 320;
  tbScale.Position.Y := 100;
  tbScale.Width := 150;
  tbScale.Height := 15;
  tbScale.Max := 3;
  tbScale.Min := 0.1;
  tbScale.OnChange := FOperation.UIScaleChange;

  lblTransparentPercent:= TvgHudLabel.Create(layUISettings);
  lblTransparentPercent.Parent := layUISettings;
  lblTransparentPercent.Position.X := 470;
  lblTransparentPercent.Position.Y := 70;
  lblTransparentPercent.Width := 40;
  lblTransparentPercent.Height := 15;
  lblTransparentPercent.Font.Style := vgFontBold;
  lblTransparentPercent.TextAlign := vgTextAlignNear;
  
  lblScalePercent:= TvgHudLabel.Create(layUISettings);
  lblScalePercent.Parent := layUISettings;
  lblScalePercent.Position.X := 470;
  lblScalePercent.Position.Y := 100;
  lblScalePercent.Width := 40;
  lblScalePercent.Height := 15;
  lblScalePercent.Font.Style := vgFontBold;
  lblScalePercent.TextAlign := vgTextAlignNear;
end;

procedure TAndroidHelperUI.ShowUI;
begin
  // Execute only once at start.
  if Assigned(FUI) then
  begin
    FOperation.SwitchLayout(0);
    FOperation.ChangeUIColor(Config.UIColor);
    FOperation.ChangeUITransparent(Config.UITransparent);
    FOperation.ChangeUIScale(Config.UIScale);
  end;
end;

end.
