// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://localhost:8080/ASServer/ASService.asmx?WSDL
//  >Import : http://localhost:8080/ASServer/ASService.asmx?WSDL:0
// Encoding : utf-8
// Version  : 1.0
// (2010-2-14 11:13:55 - - $Rev: 10138 $)
// ************************************************************************ //

unit ASServiceSoap;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_NLBL = $0004;
  IS_REF  = $0080;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:double          - "http://www.w3.org/2001/XMLSchema"[Gbl]

  TAndroidApp          = class;                 { "http://tempuri.org/"[GblCplx] }

  ArrayOfTAndroidApp = array of TAndroidApp;    { "http://tempuri.org/"[GblCplx] }


  // ************************************************************************ //
  // XML       : TAndroidApp, global, <complexType>
  // Namespace : http://tempuri.org/
  // ************************************************************************ //
  TAndroidApp = class(TRemotable)
  private
    FAppId: Integer;
    FAppName: WideString;
    FAppName_Specified: boolean;
    FAppIcon: WideString;
    FAppIcon_Specified: boolean;
    FAppDesc: WideString;
    FAppDesc_Specified: boolean;
    FAppSize: WideString;
    FAppSize_Specified: boolean;
    FAppLevel: Double;
    FAppLevelCount: Integer;
    FAppAuthor: WideString;
    FAppAuthor_Specified: boolean;
    FAppWebsite: WideString;
    FAppWebsite_Specified: boolean;
    FAppMail: WideString;
    FAppMail_Specified: boolean;
    FAppCategoryEn: WideString;
    FAppCategoryEn_Specified: boolean;
    FAppCategoryCn: WideString;
    FAppCategoryCn_Specified: boolean;
    FAppDownCount: Integer;
    FAppPlatform: WideString;
    FAppPlatform_Specified: boolean;
    FAppAddDate: WideString;
    FAppAddDate_Specified: boolean;
    FAppFile: WideString;
    FAppFile_Specified: boolean;
    FAppVersion: WideString;
    FAppVersion_Specified: boolean;
    procedure SetAppName(Index: Integer; const AWideString: WideString);
    function  AppName_Specified(Index: Integer): boolean;
    procedure SetAppIcon(Index: Integer; const AWideString: WideString);
    function  AppIcon_Specified(Index: Integer): boolean;
    procedure SetAppDesc(Index: Integer; const AWideString: WideString);
    function  AppDesc_Specified(Index: Integer): boolean;
    procedure SetAppSize(Index: Integer; const AWideString: WideString);
    function  AppSize_Specified(Index: Integer): boolean;
    procedure SetAppAuthor(Index: Integer; const AWideString: WideString);
    function  AppAuthor_Specified(Index: Integer): boolean;
    procedure SetAppWebsite(Index: Integer; const AWideString: WideString);
    function  AppWebsite_Specified(Index: Integer): boolean;
    procedure SetAppMail(Index: Integer; const AWideString: WideString);
    function  AppMail_Specified(Index: Integer): boolean;
    procedure SetAppCategoryEn(Index: Integer; const AWideString: WideString);
    function  AppCategoryEn_Specified(Index: Integer): boolean;
    procedure SetAppCategoryCn(Index: Integer; const AWideString: WideString);
    function  AppCategoryCn_Specified(Index: Integer): boolean;
    procedure SetAppPlatform(Index: Integer; const AWideString: WideString);
    function  AppPlatform_Specified(Index: Integer): boolean;
    procedure SetAppAddDate(Index: Integer; const AWideString: WideString);
    function  AppAddDate_Specified(Index: Integer): boolean;
    procedure SetAppFile(Index: Integer; const AWideString: WideString);
    function  AppFile_Specified(Index: Integer): boolean;
    procedure SetAppVersion(Index: Integer; const AWideString: WideString);
    function  AppVersion_Specified(Index: Integer): boolean;
  published
    property AppId:         Integer     read FAppId write FAppId;
    property AppName:       WideString  Index (IS_OPTN) read FAppName write SetAppName stored AppName_Specified;
    property AppIcon:       WideString  Index (IS_OPTN) read FAppIcon write SetAppIcon stored AppIcon_Specified;
    property AppDesc:       WideString  Index (IS_OPTN) read FAppDesc write SetAppDesc stored AppDesc_Specified;
    property AppSize:       WideString  Index (IS_OPTN) read FAppSize write SetAppSize stored AppSize_Specified;
    property AppLevel:      Double      read FAppLevel write FAppLevel;
    property AppLevelCount: Integer     read FAppLevelCount write FAppLevelCount;
    property AppAuthor:     WideString  Index (IS_OPTN) read FAppAuthor write SetAppAuthor stored AppAuthor_Specified;
    property AppWebsite:    WideString  Index (IS_OPTN) read FAppWebsite write SetAppWebsite stored AppWebsite_Specified;
    property AppMail:       WideString  Index (IS_OPTN) read FAppMail write SetAppMail stored AppMail_Specified;
    property AppCategoryEn: WideString  Index (IS_OPTN) read FAppCategoryEn write SetAppCategoryEn stored AppCategoryEn_Specified;
    property AppCategoryCn: WideString  Index (IS_OPTN) read FAppCategoryCn write SetAppCategoryCn stored AppCategoryCn_Specified;
    property AppDownCount:  Integer     read FAppDownCount write FAppDownCount;
    property AppPlatform:   WideString  Index (IS_OPTN) read FAppPlatform write SetAppPlatform stored AppPlatform_Specified;
    property AppAddDate:    WideString  Index (IS_OPTN) read FAppAddDate write SetAppAddDate stored AppAddDate_Specified;
    property AppFile:       WideString  Index (IS_OPTN) read FAppFile write SetAppFile stored AppFile_Specified;
    property AppVersion:    WideString  Index (IS_OPTN) read FAppVersion write SetAppVersion stored AppVersion_Specified;
  end;


  // ************************************************************************ //
  // Namespace : http://tempuri.org/
  // soapAction: http://tempuri.org/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : TASServicesSoap
  // service   : TASServices
  // port      : TASServicesSoap
  // URL       : http://127.0.0.1:8080/ASServer/ASService.asmx
  // ************************************************************************ //
  TASServicesSoap = interface(IInvokable)
  ['{94BBBDEC-8982-E79D-1760-4431D831B9ED}']
    function  HelloWorld: WideString; stdcall;
    function  GetAppList20: ArrayOfTAndroidApp; stdcall;
    function  GetAppListByCategory(const ACategory: WideString): ArrayOfTAndroidApp; stdcall;
    function  GetAppListByName(const AName: WideString): ArrayOfTAndroidApp; stdcall;
    function  GetAppListByCategoryAndName(const ACategory: WideString; const AName: WideString): ArrayOfTAndroidApp; stdcall;
    procedure DownloadFile(const AId: Integer); stdcall;
    procedure AddLevel(const AId: Integer; const ALevel: Integer); stdcall;
  end;

function GetTASServicesSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): TASServicesSoap;


implementation
  uses SysUtils;

function GetTASServicesSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): TASServicesSoap;
const
  defWSDL = 'http://localhost:8080/ASServer/ASService.asmx?WSDL';
  defURL  = 'http://127.0.0.1:8080/ASServer/ASService.asmx';
  defSvc  = 'TASServices';
  defPrt  = 'TASServicesSoap';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as TASServicesSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


procedure TAndroidApp.SetAppName(Index: Integer; const AWideString: WideString);
begin
  FAppName := AWideString;
  FAppName_Specified := True;
end;

function TAndroidApp.AppName_Specified(Index: Integer): boolean;
begin
  Result := FAppName_Specified;
end;

procedure TAndroidApp.SetAppIcon(Index: Integer; const AWideString: WideString);
begin
  FAppIcon := AWideString;
  FAppIcon_Specified := True;
end;

function TAndroidApp.AppIcon_Specified(Index: Integer): boolean;
begin
  Result := FAppIcon_Specified;
end;

procedure TAndroidApp.SetAppDesc(Index: Integer; const AWideString: WideString);
begin
  FAppDesc := AWideString;
  FAppDesc_Specified := True;
end;

function TAndroidApp.AppDesc_Specified(Index: Integer): boolean;
begin
  Result := FAppDesc_Specified;
end;

procedure TAndroidApp.SetAppSize(Index: Integer; const AWideString: WideString);
begin
  FAppSize := AWideString;
  FAppSize_Specified := True;
end;

function TAndroidApp.AppSize_Specified(Index: Integer): boolean;
begin
  Result := FAppSize_Specified;
end;

procedure TAndroidApp.SetAppAuthor(Index: Integer; const AWideString: WideString);
begin
  FAppAuthor := AWideString;
  FAppAuthor_Specified := True;
end;

function TAndroidApp.AppAuthor_Specified(Index: Integer): boolean;
begin
  Result := FAppAuthor_Specified;
end;

procedure TAndroidApp.SetAppWebsite(Index: Integer; const AWideString: WideString);
begin
  FAppWebsite := AWideString;
  FAppWebsite_Specified := True;
end;

function TAndroidApp.AppWebsite_Specified(Index: Integer): boolean;
begin
  Result := FAppWebsite_Specified;
end;

procedure TAndroidApp.SetAppMail(Index: Integer; const AWideString: WideString);
begin
  FAppMail := AWideString;
  FAppMail_Specified := True;
end;

function TAndroidApp.AppMail_Specified(Index: Integer): boolean;
begin
  Result := FAppMail_Specified;
end;

procedure TAndroidApp.SetAppCategoryEn(Index: Integer; const AWideString: WideString);
begin
  FAppCategoryEn := AWideString;
  FAppCategoryEn_Specified := True;
end;

function TAndroidApp.AppCategoryEn_Specified(Index: Integer): boolean;
begin
  Result := FAppCategoryEn_Specified;
end;

procedure TAndroidApp.SetAppCategoryCn(Index: Integer; const AWideString: WideString);
begin
  FAppCategoryCn := AWideString;
  FAppCategoryCn_Specified := True;
end;

function TAndroidApp.AppCategoryCn_Specified(Index: Integer): boolean;
begin
  Result := FAppCategoryCn_Specified;
end;

procedure TAndroidApp.SetAppPlatform(Index: Integer; const AWideString: WideString);
begin
  FAppPlatform := AWideString;
  FAppPlatform_Specified := True;
end;

function TAndroidApp.AppPlatform_Specified(Index: Integer): boolean;
begin
  Result := FAppPlatform_Specified;
end;

procedure TAndroidApp.SetAppAddDate(Index: Integer; const AWideString: WideString);
begin
  FAppAddDate := AWideString;
  FAppAddDate_Specified := True;
end;

function TAndroidApp.AppAddDate_Specified(Index: Integer): boolean;
begin
  Result := FAppAddDate_Specified;
end;

procedure TAndroidApp.SetAppFile(Index: Integer; const AWideString: WideString);
begin
  FAppFile := AWideString;
  FAppFile_Specified := True;
end;

function TAndroidApp.AppFile_Specified(Index: Integer): boolean;
begin
  Result := FAppFile_Specified;
end;

procedure TAndroidApp.SetAppVersion(Index: Integer; const AWideString: WideString);
begin
  FAppVersion := AWideString;
  FAppVersion_Specified := True;
end;

function TAndroidApp.AppVersion_Specified(Index: Integer): boolean;
begin
  Result := FAppVersion_Specified;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(TASServicesSoap), 'http://tempuri.org/', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(TASServicesSoap), 'http://tempuri.org/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(TASServicesSoap), ioDocument);
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfTAndroidApp), 'http://tempuri.org/', 'ArrayOfTAndroidApp');
  RemClassRegistry.RegisterXSClass(TAndroidApp, 'http://tempuri.org/', 'TAndroidApp');

end.