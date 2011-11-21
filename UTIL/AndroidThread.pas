unit AndroidThread;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, AndroidThreadBase,
  StringConsts;

type
  TNotifyEventParams = procedure(Sender: TObject; params: IAndroidMap) of object;

  TAndroidThread = class(TComponent)
  private
    FThreadCount: Integer;
    FExclusif: Boolean;
    FRunOnCreate: Boolean;
    FOnbegin: TNotifyEvent;
    FOnExecute: TNotifyEventParams;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    FAbout: string;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function Execute(p: IAndroidMap): Thandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: Thandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    procedure QuitThread(Thread: Thandle);
    procedure Suspend(Thread: Thandle);
    procedure Resume(Thread: Thandle);

    property About: string read FAbout write FAbout;
    property Exclusif: Boolean read FExclusif write FExclusif;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Onbegin: TNotifyEvent read FOnbegin write FOnBegin;
    property OnExecute: TNotifyEventParams read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
  end;

  THideThread = class(TThread)
  private
    FExecuteEvent: TNotifyEventParams;
    FParams: IAndroidMap;
  public
    constructor Create(event: TNotifyEventParams; params: IAndroidMap); virtual;
    procedure Execute; override;
  end;

procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TNotifyEventParams; p: IAndroidMap);

implementation

var
  mtx: THandle;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil);
  ReleaseMutex(mtx);
end;

procedure SynchronizeParams(Method: TNotifyEventParams; p: IAndroidMap);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil, p);
  ReleaseMutex(mtx);
end;

constructor TAndroidThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := true;
  FExclusif := true;
  FreeOnTerminate := true;
end;

destructor TAndroidThread.Destroy;
begin
  inherited Destroy;
end;

function TAndroidThread.Execute(p: IAndroidMap): Thandle;
var
  HideThread: THideThread;
begin
  result := 0;
  if Assigned(FOnExecute) then
  begin
    if Exclusif then
      if OneThreadIsRunning then
        exit;
    inc(FThreadCount);
    HideThread := THideThread.Create(FOnExecute, p);
    HideThread.FreeOnTerminate := FFreeOnTerminate;
    HideThread.OnTerminate := DoTerminate;
    DoCreate;
    if FRunOnCreate then
      HideThread.Resume;
    result := HideThread.Handle;   { HideThread.ThreadID }
  end;
end;

function TAndroidThread.GetPriority(Thread: Thandle): TThreadPriority;
begin
  result := tpIdle;
  if Thread <> 0 then
    result := TThreadPriority(GetThreadPriority(thread));
end;

procedure TAndroidThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, integer(priority));
end;

procedure TAndroidThread.QuitThread(Thread: Thandle);
begin
  TerminateThread(Thread, 0);
end;

procedure TAndroidThread.Suspend(Thread: Thandle);
begin
  SuspendThread(Thread);
end;

procedure TAndroidThread.Resume(Thread: Thandle);
begin
  ResumeThread(thread);
end;

procedure TAndroidThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

procedure TAndroidThread.DoTerminate;
begin
  Dec(FThreadCount);
  if Assigned(FOnFinish) then
    FOnFinish(nil);
  if FThreadCount = 0 then
    if Assigned(FOnFinishAll) then
      FOnFinishAll(nil);
end;

function TAndroidThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

constructor THideThread.Create(event: TNotifyEventParams; params: IAndroidMap);
begin
  inherited Create(true);
  FExecuteEvent := event;
  FParams := params;
end;

procedure THideThread.Execute;
begin
  FExecuteEvent(nil, FParams);
end;

initialization
  mtx := CreateMutex(nil, False, STR_THREAD_MUTEX);

finalization
  CloseHandle(mtx); 

end.
