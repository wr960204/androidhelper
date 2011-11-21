unit UIUninstllApk;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_scene, vg_controls, vg_layouts, vg_tabcontrol,
  vg_textbox, vg_objects, vg_colors, vg_memo, vg_listbox, vg_extctrls, StringConsts,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUIUninstall = class(TAndroidUIDialogBase)
  public

    // UI Elements
    lsItems: TvgHudImageListBox;
    btnClosel: TvgHudButton;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUIUninstall: TAndroidUIUninstall;

implementation

uses
  UI;

{ TAndroidUIUninstall }

constructor TAndroidUIUninstall.Create;
begin
  inherited;
  AndroidUIUninstall := Self;
  SetSize(350, 390);
  SetScale(350, 390);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUIUninstall.Destroy;
begin
  AndroidUIUninstall := nil;
  inherited;
end;

procedure TAndroidUIUninstall.InitUI;
begin
  inherited;
  
  btnClosel:= TvgHudButton.Create(Window);
  btnClosel.Parent := Window;
  btnClosel.Position.X := 245;
  btnClosel.Position.Y := 345;
  btnClosel.Width := 80;
  btnClosel.Height := 22;
  btnClosel.Text := STR_BTN_CLOSE;
  btnClosel.Font.Style := vgFontBold;
  btnClosel.ModalResult := mrCancel;

  lsItems:= TvgHudImageListBox.Create(Window);
  lsItems.Parent := Window;
  lsItems.Position.X := 22;
  lsItems.Position.Y := 42;
  lsItems.Width := 304;
  lsItems.Height :=292;
end;

end.
