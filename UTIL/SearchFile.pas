unit SearchFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  FileCtrl, StringConsts, ApkConsts;

type
  TOnFound = procedure (Sender: TObject;Path:string) of object;
  TOnChangedDir = procedure (Sender: TObject;Directory:string) of object;
  TSearchFile = class(TComponent)
  private
    FRecursive: boolean;
    FMask: string;
    FOnFound: TOnFound;
    FOnChangedDir: TOnChangedDir;
    FOnStart: TNotifyEvent;
    FonEnd: TNotifyEvent;
    FAbout: String;
    procedure Search(StartPath: string);
  protected
  public
    constructor Create(AOwner: TComponent);override;
  published
    property About : String read FAbout write FAbout;
    procedure Execute(StartPath:string);

    property Mask:string read FMask write FMask;
    property Recursive:boolean read FRecursive write FRecursive default true;

    property OnFound:TOnFound read FOnFound write FOnFound;
    property OnChangedDir:TonChangedDir read FOnChangedDir write FOnChangedDir;
    property OnStart:TNotifyEvent read FOnStart write FOnStart;
    property OnEnded:TNotifyEvent read Fonend write FOnEnd;
  end;

implementation

constructor TSearchFile.Create(AOwner: TComponent);
begin
  inherited;
  FRecursive:=true;
  FMask:=STR_MASK;
end;

procedure TSearchFile.Search(StartPath:string);
var
   t:TSearchRec;
   res:Integer;
begin
  if Assigned(FOnFound) then
  begin
    if Assigned(FOnChangedDir) then
       FOnChangedDir(self,StartPath);

    res:=FindFirst(StartPath+FMask,faAnyFile,t);
    while res=0 do
    begin
      if (t.name<>WIN32_CU) and (t.name<>WIN32_UP) then
         FOnFound(self,StartPath+t.name);
      res:=FindNext(t);
    end;
    FindClose(t);

    if FRecursive then
    begin
      res:=FindFirst(startpath+STR_MASK,faAnyFile,t);
      while res=0 do
      begin
        if (t.name<>WIN32_CU)and(t.name<>WIN32_UP)then
          if (DirectoryExists(StartPath+t.name+WIN32_SP)) then
            Search(StartPath+t.name+WIN32_SP);
        res:=FindNext(t);
      end;
      FindClose(t);
    end;
  end;
end;

procedure TSearchFile.Execute(StartPath:string);
begin
  if Assigned(FOnStart) then
    FOnStart(self);
  if StartPath=EmptyStr then
    Exit; 
  if StartPath[length(StartPath)]<>WIN32_SP then
    StartPath:=StartPath+WIN32_SP;
  Search(StartPath);
  if Assigned(FOnEnd) then
    FOnEnd(self);
end;

end.
