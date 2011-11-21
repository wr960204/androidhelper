unit UINewSQLite3;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_controls, vg_textbox, vg_scene,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUINewSQLite3 = class(TAndroidUIDialogBase)
  public
    // UI Elements
    edtDbName: TvgHudTextBox;
    btnOK: TvgHudButton;
    btnCancel: TvgHudButton;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute: Cardinal; override;
  end;

var
  AndroidUINewSQLite3: TAndroidUINewSQLite3;

implementation

uses
  UI;

{ TAndroidUINewSQLite3 }

constructor TAndroidUINewSQLite3.Create;
begin
  inherited;
  AndroidUINewSQLite3 := Self;
  SetSize(350, 130);
  SetScale(350, 130);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUINewSQLite3.Destroy;
begin
  AndroidUINewSQLite3 := nil;
  inherited;
end;

function TAndroidUINewSQLite3.Execute: Cardinal;
begin
  edtDbName.SetFocus;
  Result := inherited Execute;
end;

procedure TAndroidUINewSQLite3.InitUI;
begin
  inherited;

  Window.Text := STR_CREATE_NEW_DB;

  edtDbName := TvgHudTextBox.Create(Window);
  edtDbName.Parent := Window;
  edtDbName.Position.X := 25;
  edtDbName.Position.Y := 50;
  edtDbName.Width := 295;
  edtDbName.Height := 21;
  edtDbName.Font.Style := vgFontBold;
  btnOK := TvgHudButton.Create(Window);
  btnOK.Parent := Window;
  btnOK.Position.X := 155;
  btnOK.Position.Y := 80;
  btnOK.Width := 80;
  btnOK.Height := 22;
  btnOK.Font.Style := vgFontBold;
  btnOK.Text := STR_BTN_OK;
  btnOK.ModalResult := mrOk;
  btnCancel := TvgHudButton.Create(Window);
  btnCancel.Parent := Window;
  btnCancel.Position.X := 240;
  btnCancel.Position.Y := 80;
  btnCancel.Width := 80;
  btnCancel.Height := 22;
  btnCancel.Font.Style := vgFontBold;
  btnCancel.Text := STR_BTN_CANCEL;
  btnCancel.ModalResult := mrCancel;

end;

end.
