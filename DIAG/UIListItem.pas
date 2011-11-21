unit UIListItem;

{$I AndroidLanguage.inc} 

interface

uses
  Windows, Classes, SysUtils, Controls, vg_listbox, vg_extctrls, vg_controls, vg_scene, vg_objects,
  StrUtils, ApkConsts, StringConsts, ASServiceSoap, vg_layouts, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidItemType = (itFile = 0, itFolder = 1, itLink = 2, itUnknown = 3, itBack = 4);

  TAndroidListItem = class(TvgImageListBoxItem)
  private
    txt: TvgHudLabel;
    img: TvgImage;
    FItemType: TAndroidItemType;
    FItemName: string;
    procedure SetItemName(const Value: string); virtual;
    procedure SetItemType(const Value: TAndroidItemType); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ItemType: TAndroidItemType read FItemType write SetItemType;
    property ItemName: string read FItemName write SetItemName;
  end;

  TWin32ListItem = class(TAndroidListItem)
  protected
    procedure SetItemName(const Value: string); override;
    procedure SetItemType(const Value: TAndroidItemType); override;
  end;

  TDeviceListItem = class(TvgImageListBoxItem)
  private
    txt: TvgHudLabel;
    FTextStr: string;
    procedure SetTextStr(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property TextStr: string read FTextStr write SetTextStr;
  end;

  TAppListItem = class(TvgImageListBoxItem)
  private
    caption: TvgHudLabel;
    size: TvgHudLabel;
    btnDel: TvgHudButton;
    FTextStr: string;
    procedure SetTextStr(const Value: string);
    function GetNameSpace: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property TextStr: string read FTextStr write SetTextStr;
    property NameSpace: string Read GetNameSpace;
  end;

  TCategoryItem = class(TvgListBoxItem)
  private
    caption: TvgHudLabel;
    FCategory: string;
    procedure SetCategory(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Category: string read FCategory write SetCategory;
  end;

  TAndroidStarBar = class(TvgLayout)
  private
    FStars: array[0..4] of TvgImage;
    FStarCount: Double;
    FOnClickDelegate: TNotifyEvent;
    procedure SetStarCount(const Value: Double);
  protected
    procedure StarClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCustomShowHint(AHint: boolean);
  public
    property StarCount: Double read FStarCount write SetStarCount;
    property OnClickDelegate: TNotifyEvent read FOnClickDelegate write FOnClickDelegate;
  end;

  TAppStoreItem = class(TvgImageListBoxItem)
  private
    FItemData: TAndroidApp;
    FIcon: TvgImage;
    FCaption: TvgHudLabel;
    FAuthor: TvgHudLabel;
    FStar: TAndroidStarBar;
    procedure SetItemData(const Value: TAndroidApp);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Icon: TvgImage read FIcon write FIcon;
    property ItemData: TAndroidApp read FItemData write SetItemData;
  end;

implementation

uses
  Utils, UI;

{ TAndroidListItem }

constructor TAndroidListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 40;
  img:= TvgImage.Create(Self);
  img.Parent := Self;
  img.Width := 40;
  img.Height := 40;
  img.Align := vaLeft;
  img.WrapMode := vgImageFit;
  txt := TvgHudLabel.Create(Self);
  txt.Parent := Self;
  txt.Align := vaClient;
  txt.TextAlign := vgTextAlignNear;
  txt.Padding.left := 10;
  txt.Font.Size := 12;
  txt.Font.Style := vgFontBold;
end;

destructor TAndroidListItem.Destroy;
begin

  inherited;
end;

procedure TAndroidListItem.SetItemName(const Value: string);
var
  imgIdx: TAndroidItemType;
  resimg: TResourceStream;
begin
  FItemName := Value;
  imgIdx := TAndroidUtils.GetImageIndex(FItemName);
  if imgIdx = itUnknown then
    txt.Text := FItemName
  else
    txt.Text := LeftStr(FItemName, Length(FItemName) - 1);
  FItemType := imgIdx;
  resimg := TResourceStream.Create(HInstance, Format(FMT_IMG_P, [ord(imgIdx)]), STR_PNG_EXT);
  img.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

procedure TAndroidListItem.SetItemType(const Value: TAndroidItemType);
begin
  FItemType := Value;
end;

{ TWin32ListItem }

procedure TWin32ListItem.SetItemName(const Value: string);
var
  resimg: TResourceStream;
  ErrorMode: Cardinal;
begin
  FItemName := Value;
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  if FileExists(FItemName) then
    FItemType := itFile
  else if DirectoryExists(FItemName) then
    FItemType := itFolder
  else if FItemName[Length(FItemName)] = LINUX_UP then
    FItemType := itBack
  else
    FItemType := itUnknown;
  SetErrorMode(ErrorMode);
  
  if FItemType = itBack then
    txt.Text := LeftStr(FItemName, Length(FItemName) - 1)
  else
    txt.Text := FItemName;
  resimg := TResourceStream.Create(HInstance, Format(FMT_IMG_P, [ord(FItemType)]), STR_PNG_EXT);
  img.Bitmap.LoadFromStream(resimg);
  resimg.Free;
  
end;

procedure TWin32ListItem.SetItemType(const Value: TAndroidItemType);
var
  resimg: TResourceStream;
begin
  FItemType := Value;
  resimg := TResourceStream.Create(HInstance, Format(FMT_IMG_P, [ord(FItemType)]), STR_PNG_EXT);
  img.Bitmap.LoadFromStream(resimg);
  resimg.Free;
end;

{ TDeviceListItem }

constructor TDeviceListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Height := 40;
  txt := TvgHudLabel.Create(Self);
  txt.Parent := Self;
  txt.Align := vaClient;
  txt.TextAlign := vgTextAlignNear;
  txt.Padding.left := 10;
  txt.Font.Size := 12;
  txt.Font.Style := vgFontBold;
end;

destructor TDeviceListItem.Destroy;
begin

  inherited;
end;

procedure TDeviceListItem.SetTextStr(const Value: string);
begin
  FTextStr := Value;
  txt.Text := Value;
end;

{ TAppListItem }

constructor TAppListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 45;
  caption := TvgHudLabel.Create(Self);
  caption.Parent := Self;
  caption.Position.X := 12;
  caption.Position.Y := 6;
  caption.Width := 225;
  caption.Height := 18;
  caption.Font.Size := 14;
  caption.Font.Style := vgFontBold;
  caption.TextAlign := vgTextAlignNear;

  size := TvgHudLabel.Create(Self);
  size.Parent := Self;
  size.Position.X := 12;
  size.Position.Y := 25;
  size.Width := 225;
  size.Height := 15;
  size.TextAlign := vgTextAlignNear;

  btnDel := TvgHudButton.Create(Self);
  btnDel.Parent := Self;
  btnDel.Position.X := 230;
  btnDel.Position.Y := 11;
  btnDel.Width := 48;
  btnDel.Height := 22;
  btnDel.Font.Style := vgFontBold;
  btnDel.Text := STR_BTN_UNINSTALL;
  btnDel.ShowHint := True;
  btnDel.Hint := STR_HINT_DELAPK;
  btnDel.OnClick := AndroidUI.Operation.btnSubUninstallClick;
end;

destructor TAppListItem.Destroy;
begin

  inherited;
end;

function TAppListItem.GetNameSpace: string;
begin
  Result := caption.Text;
end;

procedure TAppListItem.SetTextStr(const Value: string);
var
  p: Integer;
begin
  FTextStr := Value;
  p := Pos(LINUX_EQUAL, Value);
  caption.Text := LeftStr(Value, p - 1);
  size.Text := Format(FMT_SIZE, [RightStr(Value, Length(Value) - p)]);
end;

{ TCategoryItem }

constructor TCategoryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 24;
  caption := TvgHudLabel.Create(Self);
  caption.Parent := Self;
  caption.Align := vaClient;
  caption.Height := 24;
  caption.TextAlign := vgTextAlignNear;
  caption.Padding.left := 5;
  caption.Font.Size := 12;
  caption.Font.Style := vgFontBold;
end;

destructor TCategoryItem.Destroy;
begin

  inherited;
end;

procedure TCategoryItem.SetCategory(const Value: string);
begin
  FCategory := Value;
  caption.Text := Value;
end;

constructor TAndroidStarBar.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  Height := 24;
  Width := 24 * 5;
  for i := 0 to 4 do
  begin
    FStars[i] := TvgImage.Create(Self);
    FStars[i].Parent := Self;
    FStars[i].Position.X := 24 * i;
    FStars[i].Width := 24;
    FStars[i].Height := 24;
    FStars[i].Align := vaLeft;
    FStars[i].OnClick := StarClick;
    FStars[i].Hint := STR_HINT_SET_LEVEL;
    FStars[i].Cursor := crHandPoint;
  end;
  FStarCount := 0;
  Cursor := crHandPoint;
  Hint := STR_HINT_SET_LEVEL;
end;

procedure TAndroidStarBar.SetCustomShowHint(AHint: boolean);
var
  i: integer;
begin
  for i := 0 to 4 do
  begin
    FStars[i].ShowHint := AHint;
  end;
  ShowHint := AHint;  
end;

procedure TAndroidStarBar.SetStarCount(const Value: Double);
var
  i: Integer;
  s1, s2: TResourceStream;
begin
  FStarCount := Value;
  for i := 0 to 4 do
    FStars[i].Bitmap.Clear;
  s1 := TResourceStream.Create(HInstance, IMG_S1, STR_PNG_EXT);
  s2 := TResourceStream.Create(HInstance, IMG_S2, STR_PNG_EXT);
  if (Value >= 0) and (Value < 0.75) then
  begin
    FStars[4].Bitmap.LoadFromStream(s2);     // half
    Exit;
  end;
  if (Value >= 0.75) and (Value < 1.25) then
  begin
    FStars[4].Bitmap.LoadFromStream(s1);    // full
    Exit;
  end;
  if (Value >= 1.25) and (Value < 1.75) then
  begin
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s2);
    Exit;
  end;
  if (Value >= 1.75) and (Value < 2.25) then
  begin
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s1);
    Exit;
  end;
  if (Value >= 2.25) and (Value < 2.75) then
  begin
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s2);
    Exit;
  end;
  if (Value >= 2.75) and (Value < 3.25) then
  begin
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s1);
    Exit;
  end;
  if (Value >= 3.25) and (Value < 3.75) then
  begin
    FStars[1].Bitmap.LoadFromStream(s1);
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s2);
    Exit;
  end;
  if (Value >= 3.75) and (Value < 4.25) then
  begin
    FStars[1].Bitmap.LoadFromStream(s1);
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s1);
    Exit;
  end;
  if (Value >= 4.25) and (Value < 4.75) then
  begin
    FStars[0].Bitmap.LoadFromStream(s1);
    FStars[1].Bitmap.LoadFromStream(s1);
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s2);
    Exit;
  end;
  if (Value >= 4.75) then
  begin
    FStars[0].Bitmap.LoadFromStream(s1);
    FStars[1].Bitmap.LoadFromStream(s1);
    FStars[2].Bitmap.LoadFromStream(s1);
    FStars[3].Bitmap.LoadFromStream(s1);
    FStars[4].Bitmap.LoadFromStream(s1);
    Exit;
  end;
end;

procedure TAndroidStarBar.StarClick(Sender: TObject);
begin

  if Assigned(FOnClickDelegate) then
    OnClickDelegate(Sender);
end;

{ TAppStoreItem }

constructor TAppStoreItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 48;
  FIcon:= TvgImage.Create(Self);
  FIcon.Parent := Self;
  FIcon.Position.X := 0;
  FIcon.Position.Y := 0;
  FIcon.Width := 42;
  FIcon.Height := 42;
  FIcon.Align := vaLeft;
  FIcon.Padding.top := 3;
  FIcon.Padding.bottom := 3;
  FIcon.Padding.left := 3;
  
  FCaption:= TvgHudLabel.Create(Self);
  FCaption.Parent := Self;
  FCaption.Position.X := 55;
  FCaption.Position.Y := 5;
  FCaption.Width := 310;
  FCaption.Height := 25;
  FCaption.Font.Size := 18;
  FCaption.Font.Style := vgFontBold;
  FCaption.TextAlign := vgTextAlignNear;
  
  FAuthor:= TvgHudLabel.Create(Self);
  FAuthor.Parent := Self;
  FAuthor.Position.X := 55;
  FAuthor.Position.Y := 32;
  FAuthor.Width := 310;
  FAuthor.Height := 15;
  FAuthor.TextAlign := vgTextAlignNear;

  FStar:= TAndroidStarBar.Create(Self);
  FStar.Parent := Self;
  FStar.Align := vaRight;
  FStar.Padding.right := 10;
  FStar.SetCustomShowHint(False);
end;

destructor TAppStoreItem.Destroy;
begin

  inherited;
end;

procedure TAppStoreItem.SetItemData(const Value: TAndroidApp);
var
  http: TIdHTTP;
  ms: TMemoryStream;
  url: string;
begin
  FItemData := Value;
  FCaption.Text := Value.AppName + Format(FMT_QUOTED, [Value.AppVersion]);
  FAuthor.Text := Value.AppAuthor;
  FStar.StarCount := Value.AppLevel;
  // download and load icon
  http := TIdHTTP.Create(nil);
  ms := TMemoryStream.Create;
  url := AndroidUI.Config.Server + SRV_ICON + Value.AppIcon;
  http.Get(url, ms);
  FIcon.Bitmap.LoadFromStream(ms);
  ms.Free;
  http.Free;
end;

end.
