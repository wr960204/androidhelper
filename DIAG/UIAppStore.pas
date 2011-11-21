unit UIAppStore;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_scene, vg_controls, vg_layouts, vg_tabcontrol,
  vg_textbox, vg_objects, vg_colors, vg_memo, vg_listbox, vg_extctrls, StringConsts,
  UIDialogBase, UIListItem, {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUIAppStore = class(TAndroidUIDialogBase)
  public
    // UI Elements
    lblCategory: TvgHudLabel;
    lblName: TvgHudLabel;
    btnCategory: TvgHudButton;
    edtName: TvgHudTextBox;
    btnSearch: TvgHudButton;
    lsItems: TvgHudImageListBox;

  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUIAppStore: TAndroidUIAppStore;

implementation

uses
  UI;

{ TAndroidAppStore }

constructor TAndroidUIAppStore.Create;
begin
  inherited;
  AndroidUIAppStore := Self;
  SetSize(555, 360);
  SetScale(555, 360);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUIAppStore.Destroy;
begin
  AndroidUIAppStore := nil;
  inherited;
end;

procedure TAndroidUIAppStore.InitUI;
begin
  inherited;

  Window.Text := STR_APP_STORE;

  lblCategory:= TvgHudLabel.Create(Window);
  lblCategory.Parent := Window;
  lblCategory.Position.X := 25;
  lblCategory.Position.Y := 46;
  lblCategory.Width := 40;
  lblCategory.Height := 15;
  lblCategory.Font.Style := vgFontBold;
  lblCategory.TextAlign := vgTextAlignNear;
  lblCategory.Text := STR_CATEGORY;

  lblName:= TvgHudLabel.Create(Window);
  lblName.Parent := Window;
  lblName.Position.X := 182;
  lblName.Position.Y := 46;
  lblName.Width := 40;
  lblName.Height := 15;
  lblName.Font.Style := vgFontBold;
  lblName.TextAlign := vgTextAlignNear;
  lblName.Text := STR_APP_NAME;

  edtName:= TvgHudTextBox.Create(Window);
  edtName.Parent := Window;
  edtName.Position.X := 222;
  edtName.Position.Y := 44;
  edtName.Width := 170;
  edtName.Height := 21;

  btnSearch:= TvgHudButton.Create(Window);
  btnSearch.Parent := Window;
  btnSearch.Position.X := 400;
  btnSearch.Position.Y := 43;
  btnSearch.Width := 50;
  btnSearch.Height := 22;
  btnSearch.Font.Style := vgFontBold;
  btnSearch.Text := STR_SEARCH;
  btnSearch.OnClick := AndroidUI.Operation.btnSearchAppClick;

  btnCategory:= TvgHudButton.Create(Window);
  btnCategory.Parent := Window;
  btnCategory.Position.X := 66;
  btnCategory.Position.Y := 43;
  btnCategory.Width := 100;
  btnCategory.Height := 22;
  btnCategory.Font.Style := vgFontBold;
  btnCategory.Text := STRARR_CATEGORY[0];
  btnCategory.OnClick := AndroidUI.Operation.btnSelCategoryClick;

  lsItems:= TvgHudImageListBox.Create(Window);
  lsItems.Parent := Window;
  lsItems.Position.X := 25;
  lsItems.Position.Y := 74;
  lsItems.Width := 504;
  lsItems.Height := 260;
  lsItems.OnDblClick := AndroidUI.Operation.lsItemAppDblClick;

end;

end.
