unit DataManager;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Borland.Vcl.Db, Borland.Vcl.ADODB, System.ComponentModel,
  AndroidApp, SQLConsts, System.Configuration;

type
  TDM = class(TDataModule)
    Conn: TADOConnection;
    Qry: TADOQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    
    function GetAndroidApps(ATop: Integer = 0; ACategory: string = ''; AName: string = ''): TAndroidApps;
    procedure SetAppLevel(AId: Integer; ALevel: Integer);
    procedure SetAppDownCount(AId: Integer);
  end;

var
  DM: TDM;

implementation

{$R *.nfm}

{ TDM }

procedure TDM.DataModuleCreate(Sender: TObject);
var
  AMthd: string;
  AHost, ADbName, AUser, APwd: string;
  connStr: string;
begin
  AMthd := ConfigurationManager.AppSettings[APP_DBTYPE].ToString; 
  if AMthd = APP_SQL then
  begin
    AHost := ConfigurationManager.AppSettings[APP_DBHOST].ToString;
    ADbName := ConfigurationManager.AppSettings[APP_DBNAME].ToString;
    AUser := ConfigurationManager.AppSettings[APP_DBUSER].ToString;
    APwd := ConfigurationManager.AppSettings[APP_DBPWD].ToString;

    connStr := Format(CONN_SQL, [APwd, AUser, ADbName, AHost]);
    Conn.Close;
    Conn.ConnectionString := connStr;
    try
      Conn.Open;
    except on E: Exception do

    end;
  end
  else if AMthd = APP_ACCESS then
  begin
    AHost := ConfigurationManager.AppSettings[APP_DBFILE].ToString;
    connStr := Format(CONN_ACCESS,[AHost]);
    Conn.Close;
    Conn.ConnectionString := connStr;
    try
      Conn.Open;
    except on E: Exception do
    end;
  end;  
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  Conn.Close;
end;

function TDM.GetAndroidApps(ATop: Integer; ACategory,
  AName: string): TAndroidApps;
var
  sql: string;
  tp: Integer;
  i: Integer;
begin
  Qry.Close;
  if ATop = 0 then
    tp := 20
  else
    tp := ATop;
  sql := Format(SQL_SEARCH_APP_BASE, [tp]);
  if ACategory <> EmptyStr then
    sql := sql + Format(SQL_ADD_CATEGORY, [ACategory, ACategory]);
  if AName <> EmptyStr then
    sql := sql + Format(SQL_ADD_NAME, [AName]);
  sql := sql + SQL_SEARCH_ORDER;
  Qry.SQL.Text := sql;
  Qry.Open;
  SetLength(Result, Qry.RecordCount);
  Qry.First;
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := TAndroidApp.Create;
    Result[i].AppId := Qry.FieldByName(FIELD_APP_ID).AsInteger;
    Result[i].AppName := Qry.FieldByName(FIELD_APP_NAME).AsString;
    Result[i].AppIcon := Qry.FieldByName(FIELD_APP_ICON).AsString;
    Result[i].AppDesc := Qry.FieldByName(FIELD_APP_DESC).AsString;
    Result[i].AppSize := Qry.FieldByName(FIELD_APP_SIZE).AsString;
    Result[i].AppLevel := Qry.FieldByName(FIELD_APP_LEVEL).AsFloat;
    Result[i].AppLevelCount := Qry.FieldByName(FIELD_APP_LEVEL_COUNT).AsInteger;
    Result[i].AppAuthor := Qry.FieldByName(FIELD_APP_AUTHOR).AsString;
    Result[i].AppWebsite := Qry.FieldByName(FIELD_APP_WEBSITE).AsString;
    Result[i].AppMail := Qry.FieldByName(FIELD_APP_MAIL).AsString;
    Result[i].AppCategoryEn := Qry.FieldByName(FIELD_APP_CATEGORY_EN).AsString;
    Result[i].AppCategoryCn := Qry.FieldByName(FIELD_APP_CATEGORY_CN).AsString;
    Result[i].AppDownCount := Qry.FieldByName(FIELD_APP_DOWN_COUNT).AsInteger;
    Result[i].AppPlatform := Qry.FieldByName(FIELD_APP_PLATFORM).AsString;
    Result[i].AppAddDate := Qry.FieldByName(FIELD_APP_ADD_DATE).AsString;
    Result[i].AppFile := Qry.FieldByName(FIELD_APP_FILE).AsString;
    Result[i].AppVersion := Qry.FieldByName(FIELD_APP_VERSION).AsString;
    Qry.Next;
  end;
  Qry.Close;
end;

procedure TDM.SetAppDownCount(AId: Integer);
begin
  Qry.Close;
  Qry.SQL.Text := Format(SQL_SET_DOWN_COUNT, [AId]);
  Qry.ExecSQL;
end;

procedure TDM.SetAppLevel(AId, ALevel: Integer);
var
  lv: Real;
  lvcnt: Integer;
begin
  Qry.Close;
  Qry.SQL.Text := Format(SQL_GET_TOTAL_LEVEL, [AId]);
  Qry.Open;
  lv := Qry.FieldByName(FIELD_LEVEL_COUNT).AsFloat;
  lvcnt := Qry.FieldByName(FIELD_APP_LEVEL_COUNT).AsInteger;
  lv := (lv + ALevel) / (lvcnt + 1);
  Qry.Close;
  Qry.SQL.Text := Format(SQL_SET_LEVEL, [lv, AId]);
  Qry.ExecSQL;
  Qry.Close;
end;

initialization
  DM := TDM.Create(nil);
    
finalization
  DM.Free;
  
end.
