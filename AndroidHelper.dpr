program AndroidHelper;

uses
  Windows,
  Forms,
  Utils,
  StringConsts,
  UI in 'UI.pas';

{$R *.res}
{$R imgres.RES}

begin
  Application.Initialize;
  AndroidUI := TAndroidHelperUI.CreateApplication;
  AndroidUI.CreateOperation;
  TAndroidUtils.AdjustProcessPrivilege(GetCurrentProcess, STR_POWER_DEBUG);
  Application.OnException := AndroidUI.Operation.AppException;
  AndroidUI.ShowUI;
  Application.Run;  // v2.0.16 hook ESC and ENTER, and make them do not effect on main form.
  AndroidUI.Free;
end.
