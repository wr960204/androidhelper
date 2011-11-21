unit UILinuxDirSelector;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_scene, vg_controls, vg_layouts, vg_tabcontrol,
  vg_textbox, vg_objects, vg_colors, vg_memo, vg_listbox, vg_extctrls, StringConsts,
  UIDialogBase, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUILinuxDirSelector = class(TAndroidUIDialogBase)
  public
    // UI elements
    lblPath: TvgHudLabel;
    edtSeledPath: TvgHudTextBox;
    btnBack: TvgHudButton;
    lsItems: TvgHudImageListBox;
    btnOK: TvgHudButton;
    btnCancel: TvgHudButton;

    btnHome: TvgImage;
    btnSDCard: TvgImage;
    btnNewDB: TvgImage;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUILinuxDirSelector: TAndroidUILinuxDirSelector;

implementation

uses
  UI;

{ TAndroidUILinuxDirSelector }

constructor TAndroidUILinuxDirSelector.Create;
begin
  inherited;
  AndroidUILinuxDirSelector := Self;
  SetSize(350, 390);
  SetScale(350, 390);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUILinuxDirSelector.Destroy;
begin
  AndroidUILinuxDirSelector := nil;
  inherited;
end;

procedure TAndroidUILinuxDirSelector.InitUI;
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
  btnBack.OnClick := AndroidUI.Operation.btnBackClick;

  btnOK:= TvgHudButton.Create(Window);
  btnOK.Parent := Window;
  btnOK.Position.X := 160;
  btnOK.Position.Y := 345;
  btnOK.Width := 80;
  btnOK.Height := 22;
  btnOK.Text := STR_BTN_OK;
  btnOK.Font.Style := vgFontBold;
  btnOK.OnClick :=AndroidUI.Operation.btnLinexDirOKClick;
  
  btnCancel:= TvgHudButton.Create(Window);
  btnCancel.Parent := Window;
  btnCancel.Position.X := 245;
  btnCancel.Position.Y := 345;
  btnCancel.Width := 80;
  btnCancel.Height := 22;
  btnCancel.Text := STR_BTN_CANCEL;
  btnCancel.Font.Style := vgFontBold;
  btnCancel.ModalResult := mrCancel;

  btnHome:= TvgImage.Create(Window);
  btnHome.Parent := Window;
  btnHome.Position.X := 22;
  btnHome.Position.Y := 344;
  btnHome.Width := 24;
  btnHome.Height := 24;
  btnHome.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance, IMG_F1, STR_PNG_EXT);
  btnHome.Bitmap.LoadFromStream(img);
  img.Free;
  btnHome.ShowHint := True;
  btnHome.Hint := STR_HINT_LINUX_HOME;
  btnHome.OnClick := AndroidUI.Operation.btnLinuxHomeClick;

  btnSDCard:= TvgImage.Create(Window);
  btnSDCard.Parent := Window;
  btnSDCard.Position.X := 50;
  btnSDCard.Position.Y := 344;
  btnSDCard.Width := 24;
  btnSDCard.Height := 24;
  btnSDCard.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance, IMG_F4, STR_PNG_EXT);
  btnSDCard.Bitmap.LoadFromStream(img);
  img.Free;
  btnSDCard.ShowHint := True;
  btnSDCard.Hint := STR_HINT_LINUX_SDCARD;
  btnSDCard.OnClick := AndroidUI.Operation.btnLinuxSDCardClick;

  btnNewDB:= TvgImage.Create(Window);
  btnNewDB.Parent := Window;
  btnNewDB.Position.X := 78;
  btnNewDB.Position.Y := 344;
  btnNewDB.Width := 24;
  btnNewDB.Height := 24;
  btnNewDB.WrapMode := vgImageFit;
  img := TResourceStream.Create(HInstance, IMG_F5, STR_PNG_EXT);
  btnNewDB.Bitmap.LoadFromStream(img);
  img.Free;
  btnNewDB.ShowHint := True;
  btnNewDB.Hint := STR_HINT_LINUX_NEW_DB;
  btnNewDB.OnClick := AndroidUI.Operation.btnLinuxNewDBClick;
  btnNewDB.Visible := False;

  lsItems:= TvgHudImageListBox.Create(Window);
  lsItems.Parent := Window;
  lsItems.Position.X := 22;
  lsItems.Position.Y := 70;
  lsItems.Width := 304;
  lsItems.Height :=265;

end;

end.
