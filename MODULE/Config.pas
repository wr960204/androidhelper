unit Config;

interface

uses
  Classes, SysUtils, IniFiles, StringConsts;

type
  TConfig = class
  private
    FSaveApk: Boolean;
    FSaveApkPath: string;
    FSendPath: string;
    FReceivePath: string;
    FReceiveFile: string;
    FDeviceType: integer;
    FUIColor: string;
    FUITransparent: Single;
    FUIScale: Single;
    FServer: string;
  public
    procedure LoadFromIni(AFileName: string);
    procedure SaveToIni(AFileName: string);
  public
    property SaveApk: Boolean read FSaveApk write FSaveApk;
    property SaveApkPath: string read FSaveApkPath write FSaveApkPath;
    property SendPath: string read FSendPath write FSendPath;
    property ReceiveFile: string read FReceiveFile write FReceiveFile;
    property ReceivePath: string read FReceivePath write FReceivePath;
    property DeviceType: integer read FDeviceType write FDeviceType;
    property UIColor: string read FUIColor write FUIColor;
    property UITransparent: Single read FUITransparent write FUITransparent;
    property UIScale: Single read FUIScale write FUIScale;
    property Server: string read FServer write FServer;
  end;

implementation

{ TConfig }

procedure TConfig.LoadFromIni(AFileName: string);
begin
  with TIniFile.Create(AFileName) do
  begin
    FSaveApk := ReadBool(CFG_CONFIG, CFG_SAVE_APK, True);
    FSaveApkPath := ReadString(CFG_CONFIG, CFG_SAVE_APK_PATH, CFG_DEF_SAVE_APK);
    FSendPath := ReadString(CFG_CONFIG, CFG_SEND_PATH, CFG_DEF_FILES);
    FReceiveFile := ReadString(CFG_CONFIG, CFG_RECEIVE_FILE, CFG_DEF_FILES);
    FReceivePath := ReadString(CFG_CONFIG, CFG_RECEIVE_PATH, ExtractFilePath(ParamStr(0))+CFG_DEF_LOCAL_FILES);
    FDeviceType := ReadInteger(CFG_CONFIG, CFG_DEVICE_TYPE, 0);
    FUIColor := ReadString(CFG_CONFIG, CFG_UI_COLOR, DEF_COLOR);
    FUITransparent := ReadFloat(CFG_CONFIG, CFG_UI_TRANSPARENT, 0.85);
    FUIScale := ReadFloat(CFG_CONFIG, CFG_UI_SCALE, 1);
    FServer := ReadString(CFG_CONFIG, CFG_SERVER, CFG_DEF_SERVER);
    Free;
  end;
end;

procedure TConfig.SaveToIni(AFileName: string);
begin
  with TIniFile.Create(AFileName) do
  begin
    WriteBool(CFG_CONFIG, CFG_SAVE_APK, FSaveApk);
    WriteString(CFG_CONFIG, CFG_SAVE_APK_PATH, FSaveApkPath);
    WriteString(CFG_CONFIG, CFG_SEND_PATH, FSendPath);
    WriteString(CFG_CONFIG, CFG_RECEIVE_FILE, FReceiveFile);
    WriteString(CFG_CONFIG, CFG_RECEIVE_PATH, FReceivePath);
    WriteInteger(CFG_CONFIG, CFG_DEVICE_TYPE, FDeviceType);
    WriteString(CFG_CONFIG, CFG_UI_COLOR, FUIColor);
    WriteFloat(CFG_CONFIG, CFG_UI_TRANSPARENT, FUITransparent);
    WriteFloat(CFG_CONFIG, CFG_UI_SCALE, FUIScale);
    WriteString(CFG_CONFIG, CFG_SERVER, FServer);
    Free;
  end;
end;

end.
