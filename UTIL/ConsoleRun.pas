unit ConsoleRun;

interface

uses
  SysUtils, Classes, windows;

type
  TOnGetConsole = procedure(ConsoleText: string) of object;
  TOnException = procedure(Ex: string) of object;
  TConsoleRun = class(TComponent)
  private
    FExitCode: DWORD;
    FCommandLine: string;
    FProgramName: string;
    FDir: string;
    FOnException: TOnException;
    FOnGetConsole: TOnGetConsole;
    FTimeout: Integer;
    { Private declarations }
  protected
    procedure CheckResult(b: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function RunProg(const Prog, CommandLine, Dir: string; var ExitCode: DWORD): string; overload;
    function RunProg: string; overload;
  published
    property ProgramName: string read FProgramName write FProgramName;
    property CommandLine: string read FCommandLine write FCommandLine;
    property Dir: string read FDir write FDir;
    property ExitCode: DWORD read FExitCode;
    property Timeout: Integer read FTimeout write FTimeout;
    property OnGetConsole: TOnGetConsole read FOnGetConsole write FOnGetConsole;
    property OnException: TOnException read FOnException write FOnException;
  end;

implementation

procedure TConsoleRun.CheckResult(b: Boolean);
begin
  if not b then
    if Assigned(OnException) then
      OnException(SysErrorMessage(GetLastError))
    else
      raise Exception.Create(SysErrorMessage(GetLastError));
end;

constructor TConsoleRun.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExitCode := 0;
  FCommandLine := EmptyStr;
  FProgramName := EmptyStr;
  FDir := EmptyStr;
  FTimeout := -1;
end;

function TConsoleRun.RunProg(const Prog, CommandLine, Dir: string;
  var ExitCode: DWORD): string;
var
  hread, hwrite: THandle;
  StartInfo: TStartupInfo;
  ProceInfo: TProcessInformation;
  b: Boolean;
  sa: TSecurityAttributes;
  inS: THandleStream;
  sRet: TStrings;
begin
  Result := EmptyStr;
  FillChar(sa, SizeOf(sa), 0);
  // Allows inherit for get output under win2000 and NT
  sa.nLength := SizeOf(sa);
  sa.bInheritHandle := True;
  sa.lpSecurityDescriptor := nil;
  b := CreatePipe(hread, hwrite, @sa, 0);
  CheckResult(b);

  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.wShowWindow := SW_HIDE;
  // pointed I/O handle and pointed show method
  StartInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
  StartInfo.hStdError := hwrite;
  StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE); //HRead;
  StartInfo.hStdOutput := hwrite;

  b := CreateProcess(PChar(Prog),   // lpApplicationName: PChar
    PChar(CommandLine),             // lpCommandLine: PChar
    nil,                            // lpProcessAttributes: PSecurityAttributes
    nil,                            // lpThreadAttributes: PSecurityAttributes
    True,                           // bInheritHandles: BOOL
    CREATE_NEW_CONSOLE,
    nil,
    PChar(Dir),
    StartInfo,
    ProceInfo);
  CheckResult(b);
  WaitForSingleObject(ProceInfo.hProcess, FTimeout);
  GetExitCodeProcess(ProceInfo.hProcess, ExitCode);
  inS := THandleStream.Create(hread);
  if inS.Size > 0 then
  begin
    sRet := TStringList.Create;
    sRet.LoadFromStream(inS);
    Result := sRet.Text;
    sRet.Free;
  end;
  inS.Free;
  FExitCode := ExitCode;
  CloseHandle(hread);
  CloseHandle(hwrite);
  if Assigned(OnGetConsole) then
    OnGetConsole(Result);
end;

function TConsoleRun.RunProg: string;
begin
  Result := RunProg(FProgramName, FCommandLine, FDir, FExitCode);
end;

end.

 