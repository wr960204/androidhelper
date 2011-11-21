unit vg_extctrls;

{$I vg_define.inc}

interface

uses Classes, Controls, SysUtils, vg_utils, vg_listbox,
  vg_scene, vg_objects, vg_ani, vg_layouts, vg_controls, vg_textbox;

type

  TvgIPhoneButton = class(TvgBitmapButton)
  private
    FBackground: TvgBrush;
    procedure SetBackground(const Value: TvgBrush);
  protected
    procedure BackChanged(Sender: TObject);
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { props }
    property Background: TvgBrush read FBackground write SetBackground;
    { inherited }
    property Resource;
  end;

  TvgDockBar = class(TvgControl)
  private
    FMousePos: TvgPoint;
    FMaxSize: single;
    FMinSize: single;
    FAmplitude: single;
    procedure SetMaxSize(const Value: single);
    procedure SetMinSize(const Value: single);
  protected
    procedure Realign; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseLeave; override;
    procedure Paint; override;
  published
    property MinSize: single read FMinSize write SetMinSize;
    property MaxSize: single read FMaxSize write SetMaxSize;
    property Resource;
  end;

  TvgImageThread = class(TThread)
  private
    FImage: TvgImage;
    FTempBitmap: TvgBitmap;
    FFileName: string;
  protected
    procedure Execute; override;
    procedure Finished;
  public
    constructor Create(const AImage: TvgImage; const AFileName: string);
    destructor Destroy; override;
  end;

  TvgImageListBoxItem = class(TvgListBoxItem)
  private
    function TextBorder: TvgVisualObject;
  protected
    procedure ApplyStyle; override;
  public
    function Text: TvgText;
  published
  end;

  TvgImageListBox = class(TvgListBox)
  private
    FFolder: string;
    FShowFileName: boolean;
    FItemHeight: single;
    function GetSelectedFileName: string;
    procedure SetShowFileName(const Value: boolean);
    procedure SetItemHeight(const Value: single);
  protected
    procedure DoThumbPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddFolder(const Folder: string);
    procedure AddFile(const AFile: string);
    procedure Clear; override;
    property SelectedFileName: string read GetSelectedFileName;
  published
    property ShowFileName: boolean read FShowFileName write SetShowFileName;
    property ItemHeight: single read FItemHeight write SetItemHeight;
  end;

  TvgHudImageListBox = class(TvgImageListBox)
  private
  protected
  public
  published
  end;

  TvgHorzImageListBox = class(TvgHorzListBox)
  private
    FFolder: string;
    FShowFileName: boolean;
    FItemWidth: single;
    function GetSelectedFileName: string;
    procedure SetShowFileName(const Value: boolean);
    procedure SetItemWidth(const Value: single);
  protected
    procedure DoThumbPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddFolder(const Folder: string);
    procedure AddFile(const AFile: string);
    procedure Clear; override;
    property SelectedFileName: string read GetSelectedFileName;
  published
    property ShowFileName: boolean read FShowFileName write SetShowFileName;
    property ItemWidth: single read FItemWidth write SetItemWidth;
  end;

  TvgHudHorzImageListBox = class(TvgHorzImageListBox)
  private
  protected
  public
  published
  end;

  TvgDropTarget = class(TvgTextControl)
  private
    FOnDrop: TvgDragDropEvent;
    FFilter: string;
  protected
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); override;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Filter: string read FFilter write FFilter;
    property Font;
    property Text;
    property OnDroped: TvgDragDropEvent read FOnDrop write FOnDrop;
  end;

  TvgPlotGrid = class(TvgVisualObject)
  private
    FMarks: single;
    FFrequency: single;
    FLineFill: TvgBrush;
    procedure SetFrequency(const Value: single);
    procedure SetMarks(const Value: single);
    procedure SetLineFill(const Value: TvgBrush);
    procedure LineFillChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineFill: TvgBrush read FLineFill write SetLineFill;
    property Marks: single read FMarks write SetMarks;
    property Frequency: single read FFrequency write SetFrequency;
  end;

{ Compound controls }

  TvgCompoundTrackBar = class(TvgControl)
  private
    FValueLabel: TvgLabel;
    FTextLabel: TvgLabel;
    FTrackBar: TvgTrackBar;
    FDecimalDigits: integer;
    FOnChange: TNotifyEvent;
    FSuffix: WideString;
    function GetValue: single;
    procedure SetValue(const Value: single);
    procedure SetDecimalDigits(const Value: integer);
    procedure SetSuffix(const Value: WideString);
  protected
    procedure DoTrack(Sender: TObject);
    procedure DoTracking(Sender: TObject);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DecimalDigits: integer read FDecimalDigits write SetDecimalDigits default 2;
    property TextLabel: TvgLabel read FTextLabel;
    property TrackBar: TvgTrackBar read FTrackBar;
    property ValueLabel: TvgLabel read FValueLabel;
    property Suffix: WideString read FSuffix write SetSuffix;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundAngleBar = class(TvgControl)
  private
    FValueLabel: TvgLabel;
    FTextLabel: TvgLabel;
    FAngleBar: TvgAngleButton;
    FDecimalDigits: integer;
    FOnChange: TNotifyEvent;
    function GetValue: single;
    procedure SetValue(const Value: single);
  protected
    procedure DoChange(Sender: TObject);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property AngleButton: TvgAngleButton read FAngleBar;
    property ValueLabel: TvgLabel read FValueLabel;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundTextBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FTextBox: TvgTextBox;
    FOnChange: TNotifyEvent;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property TextBox: TvgTextBox read FTextBox;
    property Value: WideString read GetText write SetText stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundNumberBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FNumberBox: TvgNumberBox;
    FOnChange: TNotifyEvent;
    function GetValue: single;
    procedure SetValue(const Value: single);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property NumberBox: TvgNumberBox read FNumberBox;
    property Value: single read GetValue write SetValue stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgCompoundPopupBox = class(TvgControl)
  private
    FTextLabel: TvgLabel;
    FPopupBox: TvgPopupBox;
    FOnChange: TNotifyEvent;
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
  protected
    procedure DoChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLabel: TvgLabel read FTextLabel;
    property PopupBox: TvgPopupBox read FPopupBox;
    property Value: integer read GetItemIndex write SetItemIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation {===============================================================}

{ TvgIPhoneButton ===================================================================}

constructor TvgIPhoneButton.Create(AOwner: TComponent);
begin
  inherited;
  FBackground := TvgBrush.Create(vgBrushSolid, $FF808080);
  FBackground.OnChanged := BackChanged;
end;

destructor TvgIPhoneButton.Destroy;
begin
  FBackground.Free;
  inherited;
end;

procedure TvgIPhoneButton.BackChanged(Sender: TObject);
var
  T: TvgObject;
begin
  T := FindResource('background');
  if (T <> nil) and (T is TvgShape) then
    TvgShape(T).Fill.Assign(FBackground);
end;

procedure TvgIPhoneButton.ApplyStyle;
var
  T: TvgObject;
begin
  inherited ;
  T := FindResource('background');
  if (T <> nil) and (T is TvgShape) then
    TvgShape(T).Fill.Assign(FBackground);
end;

procedure TvgIPhoneButton.SetBackground(const Value: TvgBrush);
begin
  
end;

{ TvgDockBar }

constructor TvgDockBar.Create(AOwner: TComponent);
begin
  inherited;
  FMinSize := 32;
  FMaxSize := 64;
end;

destructor TvgDockBar.Destroy;
begin
  inherited;
end;

procedure TvgDockBar.MouseLeave;
begin
  inherited;
  FMousePos := vgPoint(0, 0);
  Realign;
end;

procedure TvgDockBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgDockBar.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  FMousePos := vgPoint(X, Y);
  Realign;
end;

procedure TvgDockBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgDockBar.Paint;
begin
  inherited;
end;

procedure TvgDockBar.Realign;
var
  i, j: integer;
  dist, Pos, MaxWidth, Amplitude: single;
  hot: boolean;
  List: TList;
begin
  inherited;
  if FChildren = nil then Exit;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  try
    { make order left to right list }
    List := TList.Create;
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      if not TvgVisualObject(FChildren[i]).Visible then Continue;

      if List.Count > 0 then
      begin
        for j := 0 to List.Count - 1 do
        begin
          if TvgVisualObject(FChildren[i]).Position.X < TvgVisualObject(List[j]).Position.X then
            Break;
        end;
        List.Insert(j, FChildren[i]);
      end
      else
        List.Add(FChildren[i]);
    end;
    { align }
    if FMousePos.X = 0 then
    begin
      { mouse leave }
      MaxWidth := List.Count * MinSize;
      for i := 0 to List.Count - 1 do
      begin
        TvgVisualObject(List[i]).Position.Y := Height - MinSize;
        TvgVisualObject(List[i]).Position.X := (Width / 2) - (MaxWidth / 2) + (i * MinSize);
        TvgVisualObject(List[i]).Height := MinSize;
        TvgVisualObject(List[i]).Width := MinSize;
      end;
    end
    else
    begin
      Amplitude := FMaxSize * 1.5;
      MaxWidth := (List.Count * MinSize);
      { check hot object }
      hot := false;
      for i := 0 to List.Count - 1 do
      begin
        if (FMousePos.X >= TvgVisualObject(List[i]).Position.X) and (FMousePos.X <= TvgVisualObject(List[i]).Position.X + TvgVisualObject(List[i]).Width) then
        begin
          hot := true;
          Break;
        end
      end;
      { set sizes }
      for i := 0 to List.Count - 1 do
      begin
        if (FMousePos.X >= TvgVisualObject(List[i]).Position.X) and (FMousePos.X <= TvgVisualObject(List[i]).Position.X + TvgVisualObject(List[i]).Width) then
        begin
          TvgVisualObject(List[i]).Width := MaxSize;
          TvgVisualObject(List[i]).Height := MaxSize;
          MaxWidth := MaxWidth + (MaxSize - MinSize);
        end
        else
        begin
          dist := (TvgVisualObject(List[i]).Position.X + (TvgVisualObject(List[i]).Width / 2)) - FMousePos.X;
          if (Abs(dist) < Amplitude) and (hot) then
          begin
            TvgVisualObject(List[i]).Width := MinSize + Sin((Pi / 2) + ((dist / Amplitude) * (Pi / 2))) * (MaxSize - MinSize);
            TvgVisualObject(List[i]).Height := TvgVisualObject(List[i]).Width;
            MaxWidth := MaxWidth + (TvgVisualObject(List[i]).Width - MinSize);
          end
          else
          begin
            TvgVisualObject(List[i]).Width := MinSize;
            TvgVisualObject(List[i]).Height := MinSize;
          end;
        end;
      end;
      { align }
      Pos := ((Width / 2) - (MaxWidth / 2));
      for i := 0 to List.Count - 1 do
      begin
        TvgVisualObject(List[i]).Position.Y := Height - TvgVisualObject(List[i]).Height - 1;
        TvgVisualObject(List[i]).Position.X := Pos;
        Pos := Pos + TvgVisualObject(List[i]).Height;
      end;
    end;
    { }
    List.Free;
  finally
    FDisableAlign := false;
  end;
end;

procedure TvgDockBar.SetMaxSize(const Value: single);
begin
  FMaxSize := Value;
end;

procedure TvgDockBar.SetMinSize(const Value: single);
begin
  FMinSize := Value;
end;

{ TvgImageThread }

constructor TvgImageThread.Create(const AImage: TvgImage; const AFileName: string);
begin
  inherited Create(true);
  FFileName := AFileName;
  FImage := AImage;
  Priority := tpIdle;
  FreeOnTerminate := true;
  Resume;
end;

destructor TvgImageThread.Destroy;
begin
  inherited;
end;

procedure TvgImageThread.Execute;
begin
  FTempBitmap := TvgBitmap.Create(0, 0);
  FTempBitmap.LoadThumbnailFromFile(FFileName, FImage.Width, FImage.Height, true);
  Synchronize(Finished);
end;

procedure TvgImageThread.Finished;
begin
  if FImage <> nil then
    FImage.Bitmap.Assign(FTempBitmap);
  FTempBitmap.Free;
end;

{ TvgImageListBoxItem }

procedure TvgImageListBoxItem.ApplyStyle;
begin
  inherited;
  if (ListBox is TvgImageListBox) then
  begin
    if (TextBorder <> nil) then
      TextBorder.Visible := TvgImageListBox(ListBox).ShowFileName;
    if (Text <> nil) then
      Text.Text := ExtractFileName(TagString);
  end;
  if (ListBox is TvgHorzImageListBox) then
  begin
    if (TextBorder <> nil) then
      TextBorder.Visible := TvgHorzImageListBox(ListBox).ShowFileName;
    if (Text <> nil) then
      Text.Text := ExtractFileName(TagString);
  end;
end;

function TvgImageListBoxItem.Text: TvgText;
begin
  Result := TvgText(FindResource('text'));
end;

function TvgImageListBoxItem.TextBorder: TvgVisualObject;
begin
  Result := TvgVisualObject(FindResource('textborder'));
end;

{ TvgImageListBox }

constructor TvgImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItemHeight := Trunc(Width);
  if Self is TvgHudImageListBox then
    FResource := 'HudListBoxStyle'
  else
    FResource := 'listboxstyle';
end;

procedure TvgImageListBox.AddFile(const AFile: string);
var
  Dir, Ext: string;
  SR: TSearchRec;
  Item: TvgListBoxItem;
  Thumb: TvgImage;
begin
  Ext := LowerCase(ExtractFileExt(AFile));
  if Pos(Ext, DefaultFilterClass.GetFileTypes) > 0 then
  begin
    // Create ListboxItem
    Item := TvgImageListBoxItem.Create(Self);
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Thumb := TvgImage.Create(Item);
    Thumb.Parent := Item;
    if ShowFileName then
      Thumb.Padding.Rect := vgRect(4, 4, 4, 20)
    else
      Thumb.Padding.Rect := vgRect(4, 4, 4, 4);
    Thumb.Align := vaClient;
    Thumb.Stored := false;
    Thumb.Locked := true;
    Thumb.HitTest := false;
    // Use Tag property as image loading state 1 - unload 0 - loaded
    Item.Tag := 1;
    // Set TagString property to image FileName
    Item.TagString := AFile;
    // Set OnPaint event - because we need to load thumnail only for image which shown
    Item.OnPaint := DoThumbPaint;
    // Set the Item.TagObject as Thumb for later access
    Item.TagObject := Thumb;
  end;
  Realign;
  if Count = 1 then
    ItemIndex := 0;
end;

procedure TvgImageListBox.AddFolder(const Folder: string);
var
  Dir: string;
  SR: TSearchRec;
begin
  { add folder }
  { add SelectDialog }
  Dir := Folder;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then Dir := Dir + '\';
  if Scene <> nil then
    Scene.BeginUpdate;
  if FindFirst(Dir + '*.*', $FFFF, SR) = 0 then
  begin
    repeat
      if SR.Name = '.' then Continue;
      if SR.Name = '..' then Continue;
      if SR.Attr and faDirectory = faDirectory then Continue;
      AddFile(Dir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  if Scene <> nil then
    Scene.EndUpdateWOInvalidate;
  Realign;
  if Count > 0 then
    ItemIndex := 0;
end;

procedure TvgImageListBox.DoThumbPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
begin
  if (TvgImageListboxItem(Sender).Tag = 1) and (TvgImageListboxItem(Sender).TagString <> '') then
  begin
    // Create ImageThread
    with TvgImageThread.Create(TvgImage(TvgImageListboxItem(Sender).TagObject), TvgImageListboxItem(Sender).TagString) do
    begin
      // Clear Tag state
      TvgImageListboxItem(Sender).Tag := 0;
      // Resume thread
      Resume;
    end;
  end;
end;

function TvgImageListBox.GetSelectedFileName: string;
begin
  if (Selected <> nil) then
    Result := Selected.TagString
  else
    Result := '';
end;

procedure TvgImageListBox.SetShowFileName(const Value: boolean);
begin
  if FShowFileName <> Value then
  begin
    FShowFileName := Value;
  end;
end;

procedure TvgImageListBox.SetItemHeight(const Value: single);
var
  i: integer;
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    FDisableAlign := true;
    try
      for i := 0 to Count - 1 do
        ItemByIndex(i).Height := ItemHeight;
    finally
      FDisableAlign := false;
      Realign;
    end;
  end;
end;

procedure TvgImageListBox.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ItemByIndex(i).Tag := 0;
  inherited;
end;

{ TvgHorzImageListBox }

constructor TvgHorzImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItemWidth := Trunc(Height + 10);
  if Self is TvgHudHorzImageListBox then
    FResource := 'HudListBoxStyle'
  else
    FResource := 'ListBoxStyle';
end;

procedure TvgHorzImageListBox.AddFile(const AFile: string);
var
  Dir, Ext: string;
  SR: TSearchRec;
  Item: TvgListBoxItem;
  Thumb: TvgImage;
begin
  Ext := LowerCase(ExtractFileExt(AFile));
  if Pos(Ext, DefaultFilterClass.GetFileTypes) > 0 then
  begin
    // Create ListboxItem
    Item := TvgImageListBoxItem.Create(Self);
    Item.Parent := Self;
    Item.Width := FItemWidth;
    Item.Stored := false;
    Thumb := TvgImage.Create(Item);
    Thumb.Parent := Item;
    if ShowFileName then
      Thumb.Padding.Rect := vgRect(4, 4, 4, 20)
    else
      Thumb.Padding.Rect := vgRect(4, 4, 4, 4);
    Thumb.Align := vaClient;
    Thumb.Stored := false;
    Thumb.Locked := true;
    Thumb.HitTest := false;
    // Use Tag property as image loading state 1 - unload 0 - loaded
    Item.Tag := 1;
    // Set TagString property to image FileName
    Item.TagString := AFile;
    // Set OnPaint event - because we need to load thumnail only for image which shown
    Item.OnPaint := DoThumbPaint;
    // Set the Item.TagObject as Thumb for later access
    Item.TagObject := Thumb;
  end;
  Realign;
  if Count = 1 then
    ItemIndex := 0;
end;

procedure TvgHorzImageListBox.AddFolder(const Folder: string);
var
  Dir, Ext: string;
  SR: TSearchRec;
  Item: TvgListBoxItem;
  Thumb: TvgImage;
  R: TvgRectangle;
  Text: TvgLabel;
begin
  { add folder }
  { add SelectDialog }
  Dir := Folder;
  if Scene <> nil then
    Scene.BeginUpdate;
  if FindFirst(Dir + '*.*', $FFFF, SR) = 0 then
  begin
    repeat
      if SR.Name = '.' then Continue;
      if SR.Name = '..' then Continue;
      if SR.Attr and faDirectory = faDirectory then Continue;

      Ext := LowerCase(ExtractFileExt(SR.Name));
      if Pos(Ext, DefaultFilterClass.GetFileTypes) > 0 then
      begin
        AddFile(Dir + Sr.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  if Scene <> nil then
    Scene.EndUpdateWOInvalidate;
  Realign;
  if Count > 0 then
    ItemIndex := 0;
end;

procedure TvgHorzImageListBox.DoThumbPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
begin
  if (TvgImageListboxItem(Sender).Tag = 1) and (TvgImageListboxItem(Sender).TagString <> '') then
  begin
    // Create ImageThread
    with TvgImageThread.Create(TvgImage(TvgImageListboxItem(Sender).TagObject), TvgImageListboxItem(Sender).TagString) do
    begin
      // Clear Tag state
      TvgImageListboxItem(Sender).Tag := 0;
      // Resume thread
      Resume;
    end;
  end;
end;

function TvgHorzImageListBox.GetSelectedFileName: string;
begin
  if (Selected <> nil) then
    Result := Selected.TagString
  else
    Result := '';
end;

procedure TvgHorzImageListBox.SetShowFileName(const Value: boolean);
begin
  FShowFileName := Value;
end;

procedure TvgHorzImageListBox.SetItemWidth(const Value: single);
var
  i: integer;
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    FDisableAlign := true;
    try
      for i := 0 to Count - 1 do
        ItemByIndex(i).Width := ItemWidth;
    finally
      FDisableAlign := false;
      Realign;
    end;
  end;
end;

procedure TvgHorzImageListBox.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ItemByIndex(i).Tag := 0;
  inherited;
end;

{ TvgDropTarget }

constructor TvgDropTarget.Create(AOwner: TComponent);
begin
  inherited;
  DragDisableHighlight := true;
  Width := 120;
  Height := 120;
end;

procedure TvgDropTarget.DragDrop(const Data: TvgDragObject;
  const Point: TvgPoint);
begin
  inherited;
  if Assigned(FOnDrop) then
    FOnDrop(Self, Data, Point);
end;

procedure TvgDropTarget.DragOver(const Data: TvgDragObject;
  const Point: TvgPoint; var Accept: Boolean);
begin
  inherited;
  Accept := true; //(Length(Data.Files) > 0) and FileExists(Data.Files[0]) and (Pos(ExtractFileExt(LowerCase(Data.Files[0])), Filter) > 0);
end;

{ Graph objects ===============================================================}

{ TvgPlotGrid }

constructor TvgPlotGrid.Create(AOwner: TComponent);
begin
  inherited;
  FLineFill := TvgBrush.Create(vgBrushSolid, $FF505050);
  FLineFill.OnChanged := LineFillChanged;
  FMarks := 25;
  FFrequency := 5;
end;

destructor TvgPlotGrid.Destroy;
begin
  FLineFill.Free;
  inherited;
end;

procedure TvgPlotGrid.LineFillChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TvgPlotGrid.Paint;
var
  x, y: single;
begin
  x := 0;
  y := 0;
  Canvas.Stroke.Assign(FLineFill);
  while x < Width / 2 do
  begin
    if (x = 0) then
    begin
      Canvas.StrokeThickness := 2;
      Canvas.Stroke.SolidColor := FLineFill.SolidColor
    end
    else
    begin
      if (frac(x) = 0) and (frac(x / frequency / marks) = 0) then
        Canvas.Stroke.SolidColor := FLineFill.SolidColor
      else
        Canvas.Stroke.SolidColor := vgOpacity(FLineFill.SolidColor, 0.4);
      Canvas.StrokeThickness := 1;
    end;

    Canvas.DrawLine(vgPoint(round(Width / 2) + x + (Canvas.StrokeThickness / 2), 0), vgPoint(round(Width / 2) + x + (Canvas.StrokeThickness / 2), Height), AbsoluteOpacity);
    if x <> 0 then
      Canvas.DrawLine(vgPoint(round(Width / 2) - x + (Canvas.StrokeThickness / 2), 0), vgPoint(round(Width / 2) - x + (Canvas.StrokeThickness / 2), Height), AbsoluteOpacity);
    x := x + FFrequency;
  end;
  while y < Height / 2 do
  begin
    if (y = 0) then
    begin
      Canvas.StrokeThickness := 2;
      Canvas.Stroke.SolidColor := FLineFill.SolidColor
    end
    else
    begin
      if (frac(y) = 0) and (frac(y / frequency / marks) = 0) then
        Canvas.Stroke.SolidColor := FLineFill.SolidColor
      else
        Canvas.Stroke.SolidColor := vgOpacity(FLineFill.SolidColor, 0.4);
      Canvas.StrokeThickness := 1;
    end;

    Canvas.DrawLine(vgPoint(0, round(Height / 2) + y + (Canvas.StrokeThickness / 2)), vgPoint(Width, round(Height / 2) + y + (Canvas.StrokeThickness / 2)), AbsoluteOpacity);
    if y <> 0 then
      Canvas.DrawLine(vgPoint(0, round(Height / 2) - y + (Canvas.StrokeThickness / 2)), vgPoint(Width, round(Height / 2) - y + (Canvas.StrokeThickness / 2)), AbsoluteOpacity);
    y := y + FFrequency;
  end;
end;

procedure TvgPlotGrid.SetFrequency(const Value: single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    if FFrequency < 0.001 then
      FFrequency := 0.001;
    Repaint;
  end;
end;

procedure TvgPlotGrid.SetLineFill(const Value: TvgBrush);
begin
  FLineFill.Assign(Value);
end;

procedure TvgPlotGrid.SetMarks(const Value: single);
begin
  if FMarks <> Value then
  begin
    FMarks := Value;
    Repaint;
  end;
end;

{ TvgCompoundTrackBar }

constructor TvgCompoundTrackBar.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  FDecimalDigits := 2;
  
  Width := 200;
  Height := 20;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    Padding.right := 5;
    WordWrap := false;
    TextAlign := vgTextAlignFar;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FTrackBar := TvgTrackbar.Create(Self);
  with FTrackBar do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'TrackBar';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoTrack;
    OnTracking := DoTracking;
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaRight;
  end;
  FValueLabel := TvgValueLabel.Create(Self);
  with FValueLabel do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'ValueLabel';
    Text := '0';
    Padding.left := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
end;

procedure TvgCompoundTrackBar.UpdateLabel;
begin
  if (FTrackBar.Frequency <> 0) and (frac(FTrackBar.Frequency) = 0) then
    FValueLabel.Text := IntToStr(Trunc(Value)) + Suffix
  else
    FValueLabel.Text := Format('%.' + IntToStr(FDecimalDigits) + 'f', [Self.Value]) + Suffix;
end;

procedure TvgCompoundTrackBar.DoTrack(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TvgCompoundTrackBar.DoTracking(Sender: TObject);
begin
  UpdateLabel;
end;

function TvgCompoundTrackBar.GetValue: single;
begin
  Result := FTrackBar.Value;
end;

procedure TvgCompoundTrackBar.SetDecimalDigits(const Value: integer);
begin
  if FDecimalDigits <> Value then
  begin
    FDecimalDigits := Value;
    UpdateLabel;
  end;
end;

procedure TvgCompoundTrackBar.SetValue(const Value: single);
begin
  FTrackBar.Value := Value;
  UpdateLabel;
end;

procedure TvgCompoundTrackBar.SetSuffix(const Value: WideString);
begin
  if FSuffix <> Value then
  begin
    FSuffix := Value;
    UpdateLabel;
  end;
end;

{ TvgCompoundAngleBar }

constructor TvgCompoundAngleBar.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  FDecimalDigits := 2;

  Width := 200;
  Height := 20;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    WordWrap := false;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Width := 40;
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FAngleBar := TvgAngleButton.Create(Self);
  with FAngleBar do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaFit;
    Name := 'AngleBar';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaRight;
  end;
  FValueLabel := TvgValueLabel.Create(Self);
  with FValueLabel do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'ValueLabel';
    Text := '0';
    Padding.left := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
end;

procedure TvgCompoundAngleBar.UpdateLabel;
begin
  FValueLabel.Text := IntToStr(Trunc(Value)) + '°';
end;

procedure TvgCompoundAngleBar.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
  UpdateLabel;
end;

function TvgCompoundAngleBar.GetValue: single;
begin
  Result := FAngleBar.Value;
end;

procedure TvgCompoundAngleBar.SetValue(const Value: single);
begin
  FAngleBar.Value := Value;
  UpdateLabel;
end;

{ TvgCompoundTextBox }

constructor TvgCompoundTextBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FTextBox := TvgTextBox.Create(Self);
  with FTextBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'TextBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundTextBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundTextBox.GetText: WideString;
begin
  Result := FTextBox.Text;
end;

procedure TvgCompoundTextBox.SetText(const Value: WideString);
begin
  FTextBox.Text := Value;
end;

{ TvgCompoundNumberBox }

constructor TvgCompoundNumberBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    WordWrap := false;
    Width := 70;
    TextAlign := vgTextAlignFar;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FNumberBox := TvgNumberBox.Create(Self);
  with FNumberBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'NumberBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundNumberBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundNumberBox.GetValue: single;
begin
  Result := FNumberBox.Value;
end;

procedure TvgCompoundNumberBox.SetValue(const Value: single);
begin
  FNumberBox.Value := Value;
end;

{ TvgCompoundPopupBox }

constructor TvgCompoundPopupBox.Create(AOwner: TComponent);
var
  C: TvgControl;
begin
  inherited;
  Width := 200;
  Height := 21;
  FTextLabel := TvgLabel.Create(Self);
  with FTextLabel do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaLeft;
    Name := 'TextLabel';
    Text := 'Caption';
    Width := 70;
    TextAlign := vgTextAlignFar;
    WordWrap := false;
    Padding.right := 5;
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
  end;
  C := TvgControl.Create(Self);
  with C do
  begin
    Parent := Self;
    Stored := false;
    Locked := true;
    Align := vaClient;
  end;
  FPopupBox := TvgPopupBox.Create(Self);
  with FPopupBox do
  begin
    Parent := C;
    Stored := false;
    Locked := true;
    Align := vaVertCenter;
    Name := 'PopupBox';
    {$ifdef ks_compiler7_up}
    SetSubComponent(True);
    {$endif}
    {$ifdef FPC}
    SetSubComponent(True);
    {$endif}
    OnChange := DoChange;
  end;
end;

procedure TvgCompoundPopupBox.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

function TvgCompoundPopupBox.GetItemIndex: integer;
begin
  Result := FPopupBox.ItemIndex;
end;

procedure TvgCompoundPopupBox.SetItemIndex(const Value: integer);
begin
  FPopupBox.ItemIndex := Value;
end;

initialization
  RegisterVGObjects('Ext. Controls', [TvgIPhoneButton, {TvgDockBar, }TvgImageListBox, TvgHorzImageListBox, TvgDropTarget]);
  RegisterVGObjects('Math', [TvgPlotGrid]);
  RegisterVGObjects('HUD', [TvgHudImageListBox, TvgHudHorzImageListBox]);
  RegisterVGObjects('Items', [TvgImageListBoxItem]);
  RegisterVGObjects('Compound', [TvgCompoundTrackBar, TvgCompoundAngleBar, TvgCompoundTextBox, TvgCompoundNumberBox, TvgCompoundPopupBox]);
end.


