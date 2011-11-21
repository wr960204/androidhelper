unit UIMessageBox;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils, vg_scene, vg_controls, vg_layouts, vg_tabcontrol, vg_textbox,
  vg_objects, vg_colors, vg_memo, StringConsts, Forms, Controls, UIDialogBase,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUIMessageBox = class(TAndroidUIDialogBase)
  public
    lblMessage: TvgHudLabel;
    btnOK: TvgHudButton;
    btnCancel: TvgHudButton;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetMessage(AMessage: string);
    procedure SetButton(ACnt: Integer);
    procedure SetCloseCaption(ACaption: string);
  end;

implementation

uses
  UI;

{ TAndroidUIMessageBox }

constructor TAndroidUIMessageBox.Create;
begin
  inherited;
  SetSize(330, 150);
  SetScale(330, 150);
end;

destructor TAndroidUIMessageBox.Destroy;
begin
  inherited;
end;

procedure TAndroidUIMessageBox.InitUI;
begin
  inherited;

  lblMessage:= TvgHudLabel.Create(Window);
  lblMessage.Parent := Window;
  lblMessage.Position.X := 30;
  lblMessage.Position.Y := 44;
  lblMessage.Width := 275;
  lblMessage.Height := 50;
  lblMessage.Font.Size := 14;
  lblMessage.Font.Style := vgFontBold;
  lblMessage.TextAlign := vgTextAlignNear;

  btnOK:= TvgHudButton.Create(Window);
  btnOK.Parent := Window;
  btnOK.Position.X := 140;
  btnOK.Position.Y := 105;
  btnOK.Width := 80;
  btnOK.Height := 22;
  btnOK.Text := STR_BTN_OK;
  btnOK.Font.Style := vgFontBold;
  btnOK.ModalResult := mrOk;
  
  btnCancel:= TvgHudButton.Create(Window);
  btnCancel.Parent := Window;
  btnCancel.Position.X := 225;
  btnCancel.Position.Y := 105;
  btnCancel.Width := 80;
  btnCancel.Height := 22;
  btnCancel.Text := STR_BTN_CANCEL;
  btnCancel.Font.Style := vgFontBold;
  btnCancel.ModalResult := mrCancel;

end;

procedure TAndroidUIMessageBox.SetButton(ACnt: Integer);
begin
  btnOK.Visible := ACnt = 2;
  btnCancel.Visible := ACnt >= 1;
end;

procedure TAndroidUIMessageBox.SetCloseCaption(ACaption: string);
begin
  btnCancel.Text := ACaption;
end;

procedure TAndroidUIMessageBox.SetMessage(AMessage: string);
begin
  lblMessage.Text := AMessage;
end;

end.
