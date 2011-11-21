unit UIAppDetail;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils, Controls, Forms, vg_scene, vg_controls, vg_layouts, vg_tabcontrol,
  vg_textbox, vg_objects, vg_colors, vg_memo, vg_listbox, vg_extctrls, StringConsts,
  UIDialogBase, UIListItem, ASServiceSoap,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUIAppDetail = class(TAndroidUIDialogBase)
  public
    // UI Elements
    AppIco: TvgImage;
    lbl2DCode: TvgHudLabel;
    App2DCode: TvgImage;
    lblSize: TvgHudLabel;
    lblDownCnt: TvgHudLabel;
    lblDate: TvgHudLabel;
    lblPlatform: TvgHudLabel;
    lblAuthor: TvgHudLabel;
    lblWebsite: TvgHudLabel;
    lblMail: TvgHudLabel;
    mmDesc: TvgHudMemo;
    btnDownAndInstall: TvgHudButton;
    btnClose: TvgHudButton;
    barLevel: TAndroidStarBar;
    procDownload : TvgProgressBar;
    lblInstalling: TvgHudLabel;
  private
    FItem: TAndroidApp;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Item: TAndroidApp read FItem write FItem;
  end;

var
  AndroidUIAppDetail: TAndroidUIAppDetail;

implementation

uses
  UI;

{ TAndroidUIAppDetail }

constructor TAndroidUIAppDetail.Create;
begin
  inherited;
  AndroidUIAppDetail := Self;
  SetSize(385, 355);
  SetScale(385, 355);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDown;
end;

destructor TAndroidUIAppDetail.Destroy;
begin
  AndroidUIAppDetail := nil;
  inherited;
end;

procedure TAndroidUIAppDetail.InitUI;
begin
  inherited;

  // create UI
  AppIco:= TvgImage.Create(Window);
  AppIco.Parent := Window;
  AppIco.Position.X := 34;
  AppIco.Position.Y := 50;
  AppIco.Width := 48;
  AppIco.Height := 48;

  lbl2DCode := TvgHudLabel.Create(Window);
  lbl2DCode.Parent := Window;
  lbl2DCode.Position.X := 22;
  lbl2DCode.Position.Y := 105;
  lbl2DCode.Width := 80;
  lbl2DCode.Height := 15;
  lbl2DCode.TextAlign := vgTextAlignNear;
  lbl2DCode.Font.Style := vgFontBold;
  lbl2DCode.Text := STR_2D_CODE;

  App2DCode := TvgImage.Create(Window);
  App2DCode.Parent := Window;
  App2DCode.Position.X := 34;
  App2DCode.Position.Y := 120;
  App2DCode.Width := 48;
  App2DCode.Height := 48;
  
  lblSize:= TvgHudLabel.Create(Window);
  lblSize.Parent := Window;
  lblSize.Position.X := 90;
  lblSize.Position.Y := 52;
  lblSize.Width := 125;
  lblSize.Height := 15;
  lblSize.Font.Style := vgFontBold;
  lblSize.TextAlign := vgTextAlignNear;
  
  lblDownCnt:= TvgHudLabel.Create(Window);
  lblDownCnt.Parent := Window;
  lblDownCnt.Position.X := 90;
  lblDownCnt.Position.Y := 76;
  lblDownCnt.Width := 125;
  lblDownCnt.Height := 15;
  lblDownCnt.Font.Style := vgFontBold;
  lblDownCnt.TextAlign := vgTextAlignNear;
  
  lblDate:= TvgHudLabel.Create(Window);
  lblDate.Parent := Window;
  lblDate.Position.X := 230;
  lblDate.Position.Y := 76;
  lblDate.Width := 125;
  lblDate.Height := 15;
  lblDate.Font.Style := vgFontBold;
  lblDate.TextAlign := vgTextAlignNear;
  
  lblPlatform:= TvgHudLabel.Create(Window);
  lblPlatform.Parent := Window;
  lblPlatform.Position.X := 90;
  lblPlatform.Position.Y := 100;
  lblPlatform.Width := 265;
  lblPlatform.Height := 15;
  lblPlatform.Font.Style := vgFontBold;
  lblPlatform.TextAlign := vgTextAlignNear;
  
  lblAuthor:= TvgHudLabel.Create(Window);
  lblAuthor.Parent := Window;
  lblAuthor.Position.X := 90;
  lblAuthor.Position.Y := 124;
  lblAuthor.Width := 265;
  lblAuthor.Height := 15;
  lblAuthor.Font.Style := vgFontBold;
  lblAuthor.TextAlign := vgTextAlignNear;
  
  lblWebsite:= TvgHudLabel.Create(Window);
  lblWebsite.Parent := Window;
  lblWebsite.Position.X := 90;
  lblWebsite.Position.Y := 148;
  lblWebsite.Width := 125;
  lblWebsite.Height := 15;
  lblWebsite.Font.Style := vgFontBold;
  lblWebsite.TextAlign := vgTextAlignNear;
  lblWebsite.Text := STR_VIEW_WEBSITE;
  lblWebsite.Cursor := crHandPoint;
  lblWebsite.HitTest := True;
  lblWebsite.OnMouseEnter := AndroidUI.Operation.UILabelMouseEnter;
  lblWebsite.OnMouseLeave := AndroidUI.Operation.UILabelMouseLeave;
  lblWebsite.OnClick := AndroidUI.Operation.lblAppWebsiteClick;
  
  lblMail:= TvgHudLabel.Create(Window);
  lblMail.Parent := Window;
  lblMail.Position.X :=  230;
  lblMail.Position.Y := 148;
  lblMail.Width := 125;
  lblMail.Height := 15;
  lblMail.Font.Style := vgFontBold;
  lblMail.TextAlign := vgTextAlignNear;
  lblMail.Text := STR_VIEW_MAIL;
  lblMail.HitTest := True;
  lblMail.Cursor := crHandPoint;
  lblMail.OnMouseEnter := AndroidUI.Operation.UILabelMouseEnter;
  lblMail.OnMouseLeave := AndroidUI.Operation.UILabelMouseLeave;
  lblMail.OnClick := AndroidUI.Operation.lblAppMailClick;
  
  mmDesc:= TvgHudMemo.Create(Window);
  mmDesc.Parent := Window;
  mmDesc.Position.X := 90;
  mmDesc.Position.Y := 175;
  mmDesc.Width := 265;
  mmDesc.Height := 125;
  mmDesc.ScrollBars := ssVertical;
  mmDesc.ReadOnly := True;
  mmDesc.Font.Style := vgFontBold;

  procDownload := TvgProgressBar.Create(Window);
  procDownload.Parent := Window;
  procDownload.Position.X := 90;
  procDownload.Position.Y := 310;
  procDownload.Width := 75;
  procDownload.Height := 22;
  procDownload.Visible := False;

  lblInstalling := TvgHudLabel.Create(Window);
  lblInstalling.Parent := Window;
  lblInstalling.Position.X := 90;
  lblInstalling.Position.Y := 310;
  lblInstalling.Width := 75;
  lblInstalling.Height := 22;
  lblInstalling.Font.Style := vgFontBold;
  lblInstalling.TextAlign := vgTextAlignNear;
  lblInstalling.Visible := False;

  btnDownAndInstall:= TvgHudButton.Create(Window);
  btnDownAndInstall.Parent := Window;
  btnDownAndInstall.Position.X := 170;
  btnDownAndInstall.Position.Y := 310;
  btnDownAndInstall.Width := 100;
  btnDownAndInstall.Height := 22;
  btnDownAndInstall.Font.Style := vgFontBold;
  btnDownAndInstall.Text := STR_BTN_DOWN_AND_INSTALL;
  btnDownAndInstall.OnClick := AndroidUI.Operation.btnDownAndInstallClick;

  btnClose:= TvgHudButton.Create(Window);
  btnClose.Parent := Window;
  btnClose.Position.X := 275;
  btnClose.Position.Y := 310;
  btnClose.Width := 80;
  btnClose.Height := 22;
  btnClose.Font.Style := vgFontBold;
  btnClose.Text := STR_BTN_CLOSE;
  btnClose.ModalResult := mrCancel;

  barLevel:= TAndroidStarBar.Create(Window);
  barLevel.Parent := Window;
  barLevel.Position.X := 230;
  barLevel.Position.Y := 48;
  barLevel.OnClickDelegate := AndroidUI.Operation.AppStarClick;
  barLevel.SetCustomShowHint(True);
  // barLevel.Hint := STR_HINT_SET_LEVEL;
end;

end.
