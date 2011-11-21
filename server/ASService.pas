unit ASService;

interface

uses
  System.Collections, System.ComponentModel,
  System.Data, System.Diagnostics, System.Web,
  System.Web.Services, AndroidApp, Classes, DataManager;

type
  /// <summary>
  /// Summary description for WebService1.
  /// </summary>
  [WebService(Namespace = 'http://tempuri.org/')]
  [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
  TASServices = class(System.Web.Services.WebService)
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: IContainer;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(disposing: boolean); override;
  private
    { Private Declarations }
  public
    constructor Create;
    // Sample Web Service Method
    [WebMethod]
    function HelloWorld: string;
    [WebMethod]
    function GetAppList20: TAndroidApps;
    [WebMethod]
    function GetAppListByCategory(ACategory: string): TAndroidApps;
    [WebMethod]
    function GetAppListByName(AName: string): TAndroidApps;
    [WebMethod]
    function GetAppListByCategoryAndName(ACategory: string; AName: string): TAndroidApps;
    [WebMethod]
    procedure DownloadFile(AId: Integer);
    [WebMethod]
    procedure AddLevel(AId: Integer; ALevel: Integer);
  end;

implementation

/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TASServices.InitializeComponent;
begin

end;

procedure TASServices.AddLevel(AId, ALevel: Integer);
begin
  // add level
end;

constructor TASServices.Create;
begin
  inherited;
  //
  // Required for Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
end;

/// <summary>
/// Clean up any resources being used.
/// </summary>
procedure TASServices.Dispose(disposing: boolean);
begin
  if disposing and (components <> nil) then
    components.Dispose;
  inherited Dispose(disposing);
end;

procedure TASServices.DownloadFile(AId: Integer);
begin
  // download file
  // add download count
  DM.SetAppDownCount(AId);
end;

function TASServices.GetAppList20: TAndroidApps;
begin
  // todo: get app list
  Result := DM.GetAndroidApps;
end;

function TASServices.GetAppListByCategory(ACategory: string): TAndroidApps;
begin
  Result := DM.GetAndroidApps(0, ACategory);
end;

function TASServices.GetAppListByCategoryAndName(ACategory,
  AName: string): TAndroidApps;
begin
  Result := DM.GetAndroidApps(0, ACategory, AName);
end;

function TASServices.GetAppListByName(AName: string): TAndroidApps;
begin
  Result := DM.GetAndroidApps(0, '', AName);
end;

// Sample Web Service Method
// The following method is provided to allow for testing a new web service.
function TASServices.HelloWorld: string;
begin
  Result := 'Hello World';
end;

end.

