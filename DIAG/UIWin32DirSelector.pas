unit UIWin32DirSelector;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_scene, vg_controls, vg_layouts, vg_tabcontrol,
  vg_textbox, vg_objects, vg_colors, vg_memo, vg_listbox, vg_extctrls, StringConsts,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUIWin32DirSelector = class(TAndroidUIDialogBase)
  public
    // UI Elements
    lblPath: TvgHudLabel;
    edtSeledPath: TvgHudTextBox;
    btnBack: TvgHudButton;
    lsItems: TvgHudImageListBox;
    btnOK: TvgHudButton;
    btnCancel: TvgHudButton;

    // common icons
    btnDesk: TvgImage;
    btnHome: TvgImage;
    btnDocument: TvgImage;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUIWin32DirSelector: TAndroidUIWin32DirSelector;

implementation

uses
  UI;

constructor TAndroidUIWin32DirSelector.Create;
begin
  inherited;
  AndroidUIWin32DirSelector := Self;
  SetSize(350, 390);
  SetScale(350, 390);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUIWin32DirSelector.Destroy;
begin
  AndroidUIWin32DirSelector := nil;
  inherited;
end;

procedure TAndroidUIWin32DirSelector.InitUI;
var
  img: TResourceStream;
begin
  inherited;

  lblPath:= TvgHudLabel.Create(Window);
  lblPath.Parent := Window;
  lblPath.Position.X := 22;
  lblPath.Position.Y := 45;
  lblPath.Width := 65;
  lblPath.Height := 15;
  lblPath.TextAlign := vgTextAlignNear;
  lblPath.Text := STR_BTN_CURR_PATH;
  lblPath.Font.Style := vgFontBold;

  edtSeledPath:= TvgHudTextBox.Create(Window);
  edtSeledPath.Parent := Window;
  edtSeledPath.Position.X := 85;
  edtSeledPath.Position.Y := 43;
  edtSeledPath.Width := 185;
  edtSeledPath.Height := 21;
  edtSeledPath.Font.Style := vgFontBold;
  edtSeledPath.ReadOnly := True;

  btnBack:= TvgHudButton.Create(Window);
  btnBack.Parent := Window;
  btnBack.Position.X := 275;
  btnBack.Position.Y := 42;
  btnBack.Width := 50;
  btnBack.Height := 22;
  btnBack.Text := STR_BTN_BACK;
  btnBack.Font.Style := vgFontBold;
  btnBack.OnClick := AndroidUI.Operation.BtnBackWin32Click;

  btnOK:= TvgHudButton.Create(Window);
  btnOK.Parent := Window;
  btnOK.Position.X := 160;
  btnOK.Position.Y := 345;
  btnOK.Width := 80;
  btnOK.Height := 22;
  btnOK.Text := STR_BTN_OK;
  btnOK.Font.Style := vgFontBold;
  btnOK.ModalResult := mrOk;
  
  btnCancel:= TvgHudButton.Create(Window);
  btnCancel.Parent := Window;
  btnCancel.Position.X := 245;
  btnCancel.Position.Y := 345;
  btnCancel.Width := 80;
  btnCancel.Height := 22;
  btnCancel.Text := STR_BTN_CANCEL;
  btnCancel.Font.Style := vgFontBold;
  btnCancel.ModalResult := mrCancel;

  btnDesk:= TvgImage.Create(Window);
  btnDesk.Parent := Window;
  btnDesk.Position.X := 22;
  btnDesk.Position.Y := 344;
  btnDesk.Width := 24;
  btnDesk.Height := 24;
  btnDesk.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance, IMG_F1, STR_PNG_EXT);
  btnDesk.Bitmap.LoadFromStream(img);
  img.Free;
  btnDesk.ShowHint := True;
  btnDesk.Hint := STR_HINT_WIN32_DESK;
  btnDesk.OnClick := AndroidUI.Operation.btnWin32DeskClick;
  
  btnHome:= TvgImage.Create(Window);
  btnHome.Parent := Window;
  btnHome.Position.X := 50;
  btnHome.Position.Y := 344;
  btnHome.Width := 24;
  btnHome.Height := 24;
  btnHome.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance,IMG_F2, STR_PNG_EXT);
  btnHome.Bitmap.LoadFromStream(img);
  img.Free;
  btnHome.ShowHint := True;
  btnHome.Hint := STR_HINT_WIN32_HOME;
  btnHome.OnClick := AndroidUI.Operation.btnWin32HomeClick;

  btnDocument:= TvgImage.Create(Window);
  btnDocument.Parent := Window;
  btnDocument.Position.X := 78;
  btnDocument.Position.Y := 344;
  btnDocument.Width := 24;
  btnDocument.Height := 24;
  btnDocument.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance, IMG_F3, STR_PNG_EXT);
  btnDocument.Bitmap.LoadFromStream(img);
  img.Free;
  btnDocument.ShowHint := True;
  btnDocument.Hint := STR_HINT_WIN32_DOCUMENT;
  btnDocument.OnClick := AndroidUI.Operation.btnWin32DocumentClick;

  lsItems:= TvgHudImageListBox.Create(Window);
  lsItems.Parent := Window;
  lsItems.Position.X := 22;
  lsItems.Position.Y := 70;
  lsItems.Width := 304;
  lsItems.Height :=265;

end;

end.
