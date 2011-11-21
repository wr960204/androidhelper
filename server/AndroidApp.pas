unit AndroidApp;

interface

uses
  Classes, SysUtils, Graphics;

type
  TAndroidApp = class
  private
    FAppId: integer;
    FAppIcon: string;
    FAppName: string;
    FAppDesc: string;
    FAppLevel: Double;
    FAppWebsite: string;
    FAppMail: string;
    FAppAuthor: string;
    FAppDownCount: integer;
    FAppPlatform: string;
    FAppSize: string;
    FAppLevelCount: integer;
    FAppAddDate: string;
    FAppFile: string;
    FAppCategoryEn: string;
    FAppCategoryCn: string;
    FAppVersion: string;
    { Private Declarations }
  public
    constructor Create;
  public
    property AppId: integer read FAppId write FAppId;
    property AppName: string read FAppName write FAppName;
    property AppIcon: string read FAppIcon write FAppIcon;
    property AppDesc: string read FAppDesc write FAppDesc;
    property AppSize: string read FAppSize write FAppSize;
    property AppLevel: Double read FAppLevel write FAppLevel;
    property AppLevelCount: integer read FAppLevelCount write FAppLevelCount;
    property AppAuthor: string read FAppAuthor write FAppAuthor;
    property AppWebsite: string read FAppWebsite write FAppWebsite;
    property AppMail: string read FAppMail write FAppMail;
    property AppCategoryEn: string read FAppCategoryEn write FAppCategoryEn;
    property AppCategoryCn: string read FAppCategoryCn write FAppCategoryCn;
    property AppDownCount: integer read FAppDownCount write FAppDownCount;
    property AppPlatform: string read FAppPlatform write FAppPlatform;
    property AppAddDate: string read FAppAddDate write FAppAddDate;
    property AppFile: string read FAppFile write FAppFile;
    property AppVersion: string read FAppVersion write FAppVersion;
  end;

  TAndroidApps = array of TAndroidApp;

implementation

constructor TAndroidApp.Create;
begin
  inherited Create;
  // TODO: Add any constructor code here
end;

end.
