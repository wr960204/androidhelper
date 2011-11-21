unit UISelectDevice;

{$I AndroidLanguage.inc} 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, vg_scene, vg_controls, vg_layouts, vg_tabcontrol, vg_textbox,
  vg_objects, vg_colors, vg_memo, StringConsts, vg_listbox, vg_extctrls,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUISelectDevice = class(TAndroidUIDialogBase)
  public

    lsItem: TvgHudListBox;
    btnOK: TvgHudButton;
    btnCancel: TvgHudButton;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUISelectDevice: TAndroidUISelectDevice;

implementation

uses
  UI;

{ TAndroidUISelectDevice }

constructor TAndroidUISelectDevice.Create;
begin
  inherited;
  AndroidUISelectDevice := self;
  SetSize(280, 270);
  SetScale(280, 270);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUISelectDevice.Destroy;
begin
  AndroidUISelectDevice := nil;
  inherited;
end;

procedure TAndroidUISelectDevice.InitUI;
begin
  inherited;

  Window.Text := STR_SEL_DEVICE;

  btnOK:= TvgHudButton.Create(Window);
  btnOK.Parent := Window;
  btnOK.Position.X := 90;
  btnOK.Position.Y := 220;
  btnOK.Width := 80;
  btnOK.Height := 22;
  btnOK.Text := STR_BTN_OK;
  btnOK.Font.Style := vgFontBold;
  btnOK.ModalResult := mrOk;

  btnCancel:= TvgHudButton.Create(Window);
  btnCancel.Parent := Window;
  btnCancel.Position.X := 175;
  btnCancel.Position.Y := 220;
  btnCancel.Width := 80;
  btnCancel.Height := 22;
  btnCancel.Text := STR_BTN_CANCEL;
  btnCancel.Font.Style := vgFontBold;
  btnCancel.ModalResult := mrCancel;

  lsItem:= TvgHudListBox.Create(Window);
  lsItem.Parent := Window;
  lsItem.Position.X := 22;
  lsItem.Position.Y := 42;
  lsItem.Width := 235;
  lsItem.Height := 170;

end;

end.
