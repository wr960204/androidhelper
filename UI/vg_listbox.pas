unit vg_listbox;

{$I vg_define.inc}

interface

uses
  {$IFDEF FPC}
  LCLType, LMessages, LResources,
  {$ENDIF}
  {$IFDEF WIN32} windows, {$ENDIF}
  Classes, Controls, SysUtils, vg_utils,
  vg_classes, vg_scene, vg_objects, vg_controls;

type

  TvgListBox = class;
  TvgComboBox = class;

  { TvgListBoxItem }

  TvgListBoxItem = class(TvgControl)
  private
    FIndex: integer;
    procedure SetIndex(const Value: integer);
  protected
    function GetParentComponent: TComponent; override;
    procedure ApplyStyle; override;
    procedure DesignInsert; override;
    procedure DesignSelect; override;
    function ListBox: TvgListBox;
    function ComboBox: TvgComboBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Remove;
    property Index: integer read FIndex write SetIndex;
  published
    property Resource;
  end;

  TvgListBoxSelection = class(TvgControl)
  private
  protected
    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Resource;
  end;

  TvgListBox = class(TvgControl)
  private
    FContent: TvgContent;
    FItemIndex: integer;
    FMouseSelecting: boolean;
    FOnChange: TNotifyEvent;
    FDisableMouseWheel: boolean;
    FScrollBarSize: single;
    FColumns: integer;
    FItemWidth: single;
    FItemHeight: single;
    FUpdating: integer;
    FHideSelectionUnfocused: boolean;
    function GetCount: integer;
    function GetSelected: TvgListBoxItem;
    function GetVScrollBar: TvgScrollBar;
    function GetSelection: TvgListBoxSelection;
    procedure SetColumns(const Value: integer);
    procedure SetItemHeight(const Value: single);
    procedure SetItemWidth(const Value: single);
  protected
    procedure ApplyStyle; override;
    procedure VScrollChange(Sender: TObject);
    function GetListRect: TvgRect;
    procedure SetScrollBarSize(const Value: single); virtual;
    procedure SetItemIndex(const Value: integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure ScrollList; virtual;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Realign; override;
    procedure Clear; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function ItemByPoint(const X, Y: single): TvgListBoxItem;
    function ItemByIndex(const Idx: integer): TvgListBoxItem;
    procedure AddObject(AObject: TvgObject); override;
    property Count: integer read GetCount;
    property Selected: TvgListBoxItem read GetSelected;
    property VScrollBar: TvgScrollBar read GetVScrollBar;
    property Selection: TvgListBoxSelection read GetSelection;
  published
    property Resource;
    property DisableMouseWheel: boolean read FDisableMouseWheel write FDisableMouseWheel;
    property Columns: integer read FColumns write SetColumns default 1;
    property HideSelectionUnfocused: boolean read FHideSelectionUnfocused write FHideSelectionUnfocused default true;
    property ScrollBarSize: single read FScrollBarSize write SetScrollBarSize;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemWidth: single read FItemWidth write SetItemWidth;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgComboListBox = class(TvgListbox)
  private
    FComboBox: TvgComboBox;
  protected
    procedure ApplyStyle; override;
    procedure ApplyResource; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetParentComponent: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure KillFocus; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
  published
  end;

  TvgComboBox = class(TvgControl)
  private
    FDropDownCount: integer;
    FPopup: TvgPopup;
    FListBox: TvgComboListBox;
    FOnChange: TNotifyEvent;
    FPlacement: TvgPlacement;
    procedure SetItemIndex(const Value: integer);
    function GetScrollBarSize: single;
    procedure SetScrollBarSize(const Value: single);
    function GetItemIndex: integer;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ApplyStyle; override;
    procedure DoListBoxChange(Sender: TObject);
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure DesignClick; override;
    procedure ChangeParent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Paint; override;
    procedure PaintChildren; override;
    procedure Realign; override;
    procedure Clear; virtual;
    procedure DropDown;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer;
      var Handled: boolean); override;
    property ListBox: TvgComboListBox read FListBox write FListBox;
  published
    property Resource;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property Placement: TvgPlacement read FPlacement write FPlacement;
    property ScrollBarSize: single read GetScrollBarSize write SetScrollBarSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgStringListBox = class(TvgListBox)
  private
    FItems: TvgWideStrings;
    FTextAlign: TvgTextAlign;
    procedure SetItems(const Value: TvgWideStrings);
    procedure SetTextAlign(const Value: TvgTextAlign);
  protected
    procedure ApplyStyle; override;
    procedure RebuildList;
    procedure DoItemsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EndUpdate; override;
  published
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignCenter;
    property Items: TvgWideStrings read FItems write SetItems;
    property ItemIndex;
  end;

  TvgStringComboBox = class(TvgComboBox)
  private
    FItemHeight: single;
    FItems: TvgWideStrings;
    FTextAlign: TvgTextAlign;
    procedure SetItemHeight(const Value: single);
    procedure SetItems(const Value: TvgWideStrings);
    procedure SetTextAlign(const Value: TvgTextAlign);
  protected
    procedure ApplyStyle; override;
    procedure RebuildList;
    procedure DoItemsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignCenter;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property Items: TvgWideStrings read FItems write SetItems;
    property ItemIndex;
  end;

  TvgHorzListBox = class(TvgListBox)
  private
  protected
    procedure SetScrollBarSize(const Value: single); override;
    procedure SetItemIndex(const Value: integer); override;
    procedure ScrollList; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
  end;

  TvgHudListBox = class(TvgListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudHorzListBox = class(TvgHorzListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudStringListBox = class(TvgStringListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudComboBox = class(TvgComboBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudStringComboBox = class(TvgStringComboBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation {===============================================================}

uses vg_ani;

type
  TvgHackObject = class(TvgVisualObject);

{ TvgListBoxItem }

constructor TvgListBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  Height := 19;
  Width := 19;
  HitTest := false;
end;

destructor TvgListBoxItem.Destroy;
begin
  if (ListBox <> nil) and not (csDestroying in ListBox.ComponentState) then
    Remove;
  inherited;
end;

procedure TvgListBoxItem.Remove;
var
  SaveListBox: TvgListBox;
begin
  if ListBox <> nil then
  begin
    SaveListBox := ListBox;
    if Parent <> nil then
      Parent.RemoveObject(Self);
    SaveListBox.Realign;
    if SaveListBox.ItemIndex >= SaveListBox.Count then
      SaveListBox.ItemIndex := SaveListBox.Count - 1;
  end;
end;

procedure TvgListBoxItem.ApplyStyle;
begin
  inherited;
end;

function TvgListBoxItem.ComboBox: TvgComboBox;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgComboBox then
    begin
      Result := TvgComboBox(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

function TvgListBoxItem.ListBox: TvgListBox;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgListBox then
    begin
      Result := TvgListBox(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TvgListBoxItem.DesignSelect;
begin
  inherited;
  if (ComboBox <> nil) then ComboBox.DesignClick;
  if ListBox <> nil then
  begin
    ListBox.ItemIndex := Index;
  end;
end;

procedure TvgListBoxItem.DesignInsert;
begin
  inherited;
  if (ComboBox <> nil) then ComboBox.DesignClick;
end;

procedure TvgListBoxItem.Paint;
begin
  inherited Paint;
end;

procedure TvgListBoxItem.SetIndex(const Value: integer);
begin
  FIndex := Value;
end;

function TvgListBoxItem.GetParentComponent: TComponent;
begin
  if (ComboBox <> nil) then
    Result := ComboBox
  else
  if (ListBox <> nil) then
    Result := ListBox
  else
    Result := Parent;
{  if Result = nil then
    Result := Scene;}
end;

{ TvgListBoxSelection }

constructor TvgListBoxSelection.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := false;
end;

procedure TvgListBoxSelection.Paint;
begin
  inherited;
end;

procedure TvgListBoxSelection.PaintChildren;
begin
  if csDesigning in ComponentState then Exit;
  inherited;
end;

{ TvgListBox ==================================================================}

constructor TvgListBox.Create(AOwner: TComponent);
begin
  inherited;
  FScrollBarSize := 16;
  FColumns := 1;
  FHideSelectionUnfocused := true;
  CanFocused := true;
  AutoCapture := true;
  Width := 100;
  Height := 100;
  FContent := TvgContent.Create(Self);
  FContent.Parent := Self;
  FContent.ClipChildren := true;
  FContent.HitTest := false;
  FContent.Stored := false;
  FContent.Locked := true;
end;

destructor TvgListBox.Destroy;
begin
  inherited;
end;

function TvgListBox.ItemClass: string;
begin
  Result := 'TvgListBoxItem';
end;

procedure TvgListBox.ApplyStyle;
begin
  inherited;
  if VScrollBar <> nil then
  begin
    VScrollBar.OnChange := VScrollChange;
    if Self is TvgHorzListBox then
    begin
      VScrollBar.Orientation := vgHorizontal;
      VScrollBar.Height := ScrollBarSize;
    end
    else
    begin
      VScrollBar.Orientation := vgVertical;
      VScrollBar.Width := ScrollBarSize;
    end;
    VScrollBar.Locked := true;
  end;
  Realign;
end;

function TvgListBox.GetSelection: TvgListBoxSelection;
var
  B: TvgObject;
begin
  B := FindResource('selection');
  if (B <> nil) and (B is TvgListBoxSelection) then
    Result := TvgListBoxSelection(B)
  else
    Result := nil;
end;

function TvgListBox.GetVScrollBar: TvgScrollBar;
var
  B: TvgObject;
begin
  B := FindResource('vscrollbar');
  if (B <> nil) and (B is TvgScrollBar) then
  begin
    Result := TvgScrollBar(B);
  end
  else
    Result := nil;
end;

function TvgListBox.GetListRect: TvgRect;
var
  B: TvgObject;
begin
  B := FindResource('background');
  if (B <> nil) and (B is TvgVisualObject) then
    Result := TvgVisualObject(B).Margins.MarginRect(LocalRect)
  else
    Result := Margins.MarginRect(LocalRect);
end;

procedure TvgListBox.ScrollList;
var
  i, j, Idx: integer;
  R: TvgRect;
  CurY, RowHeight, ColWidth: single;
begin
  if FContent <> nil then
  begin
    Scene.BeginUpdate;
    FDisableAlign := true;
    R := GetListRect;
    if (VScrollBar <> nil) and (VScrollBar.Visible) then
      R.Right := VScrollBar.Position.X;
    { align }
    CurY := 0;
    Idx := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        for j := 0 to FColumns - 1 do
        begin
          if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;

          RowHeight := 0;
          if FItemWidth <> 0 then
            ColWidth := FItemWidth
          else
            ColWidth := (R.Right - R.Left) / FColumns;

          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if VScrollBar <> nil then
                SetBounds(Padding.Left + (j * ColWidth), CurY + Padding.Top - VScrollBar.Value,
                  ColWidth, Height)
              else
                SetBounds(Padding.Left + (j * ColWidth), CurY + Padding.Top,
                  ColWidth, Height);

              if Height + Padding.Top + Padding.Bottom > RowHeight then
                RowHeight := Height + Padding.Top + Padding.Bottom;

              Index := Idx;
              if (ItemIndex = Idx) and (Selection <> nil) then
              begin
                if HideSelectionUnfocused then
                  Selection.Visible := Self.IsFocused
                else
                  Selection.Visible := true;
                Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
              end;
              Inc(Idx);
            end;
          end;
          CurY := CurY + RowHeight;
        end;
    FDisableAlign := false;
    Scene.EndUpdateWOInvalidate;
    Repaint;
  end;
end;                                                       

procedure TvgListBox.Realign;
var
  i, j, Idx: integer;
  R: TvgRect;
  RowHeight, ColWidth, CurY: single;
begin
  if FUpdating > 0 then Exit;
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  R := GetListRect;
  { content }
  if FContent <> nil then
  begin
    FContent.SetBounds(R.Left, R.Top, vgRectWidth(R), vgRectHeight(R));
    { correct items size }
    if FItemWidth <> 0 then
    begin
      if (VScrollBar <> nil) and (VScrollBar.Visible) then
        FColumns := trunc((R.Right - R.Left - VScrollBar.Width) / FItemWidth)
      else
        FColumns := trunc((R.Right - R.Left) / FItemWidth);
      if FColumns < 1 then FColumns := 1;
      if FContent.ChildrenCount > 0 then
        for i := 0 to (FContent.ChildrenCount - 1) do
          with TvgListBoxItem(FContent.Children[i]) do
          begin
            if FItemHeight <> 0 then
              SetBounds(Position.X, Position.Y, FItemWidth, FItemHeight)
            else
              SetBounds(Position.X, Position.Y, FItemWidth, Height);
          end;
    end;
    if (FItemWidth = 0) and (FItemHeight <> 0) then
    begin
      if FContent.ChildrenCount > 0 then
        for i := 0 to (FContent.ChildrenCount - 1) do
          with TvgListBoxItem(FContent.Children[i]) do
          begin
            SetBounds(Position.X, Position.Y, Width, FItemHeight)
          end;
    end;
    { calc items size }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        RowHeight := 0;
        for j := 0 to FColumns - 1 do
        begin
          if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if Height + Padding.Top + Padding.Bottom > RowHeight then
                RowHeight := Height + Padding.Top + Padding.Bottom;
            end;
        end;
        // set correct height
        for j := 0 to FColumns - 1 do
        begin
          if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              Height := RowHeight - Padding.Top - Padding.Bottom;
            end;
        end;
        CurY := CurY + RowHeight;
      end;
    { scrollbar }
    if VScrollBar <> nil then
    begin
      VScrollBar.Visible := CurY > vgRectHeight(R);
      if VScrollBar.Visible then
      begin
        VScrollBar.Orientation := vgVertical;
        VScrollBar.Width := ScrollBarSize;
        VScrollBar.Max := CurY;
        VScrollBar.ViewportSize := vgRectHeight(R);
        if Count > 0 then
          VScrollBar.SmallChange := CurY / Count;
        VScrollBar.SetBounds(R.Right - VScrollBar.Width, R.Top, VScrollBar.Width, vgRectHeight(R));
        R.Right := VScrollBar.Position.X;
      end;
    end;
    { align }
    CurY := 0;
    Idx := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        RowHeight := 0;
        for j := 0 to FColumns - 1 do
        begin
          if (i * FColumns) + j > FContent.ChildrenCount - 1 then Continue;

          if FItemWidth <> 0 then
            ColWidth := FItemWidth
          else
            ColWidth := (R.Right - R.Left) / FColumns;

          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if (VScrollBar <> nil) and (VScrollBar.Visible) then
                SetBounds(Padding.Left + (j * ColWidth), CurY + Padding.Top - VScrollBar.Value,
                  ColWidth, Height)
              else
                SetBounds(Padding.Left + (j * ColWidth), CurY + Padding.Top,
                  ColWidth, Height);

              if Height + Padding.Top + Padding.Bottom > RowHeight then
                RowHeight := Height + Padding.Top + Padding.Bottom;

              Index := Idx;
              if (ItemIndex = Idx) and (Selection <> nil) then
              begin
                if HideSelectionUnfocused then
                  Selection.Visible := Self.IsFocused
                else
                  Selection.Visible := true;
                Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
              end;
              Inc(Idx);
            end;
        end;
        CurY := CurY + RowHeight;
      end;
    if ((ItemIndex < 0) or (Count = 0)) and (Selection <> nil) then
    begin
      Selection.Visible := false;
      Selection.Height := 1;
    end;
  end;
  FDisableAlign := false;
end;

function TvgListBox.GetCount: integer;
var
  i: integer;
begin
  Result := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgListBoxItem then
      begin
        Inc(Result);
      end;
end;

function TvgListBox.ItemByIndex(const Idx: integer): TvgListBoxItem;
var
  i: integer;
begin
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgListBoxItem then
        if TvgListBoxItem(FContent.Children[i]).Index = Idx then
        begin
          Result := TvgListBoxItem(FContent.Children[i]);
          Exit;
        end;
  Result := nil;
end;

function TvgListBox.ItemByPoint(const X, Y: single): TvgListBoxItem;
var
  i: integer;
  P: TvgPoint;
begin
  P := LocaltoAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgListBoxItem then
        if TvgListBoxItem(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          Result := TvgListBoxItem(FContent.Children[i]);
          Exit;
        end;
  Result := nil;
end;

procedure TvgListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
  if Count > 0 then
  begin
    case Key of
      VK_HOME: ItemIndex := 0;
      VK_END: ItemIndex := Count - FColumns;
      VK_UP: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - FColumns;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_DOWN:
        begin
          If ItemIndex < Count - 1 then ItemIndex := ItemIndex + FColumns;
          if ItemIndex > Count - 1 then ItemIndex := Count - 1;
        end;
      VK_LEFT: If ItemIndex > 0 then ItemIndex := ItemIndex - 1;
      VK_RIGHT: If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgListBox.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  Item: TvgListBoxItem;
begin
  inherited;
  if Button = mbLeft then
  begin                                                                                      
    Item := ItemByPoint(X, Y);
    if Item <> nil then
      ItemIndex := Item.Index;
    FMouseSelecting := true;
  end;
end;

procedure TvgListBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  Item: TvgListBoxItem;
begin
  inherited;
  if (ssLeft in Shift) and FMouseSelecting then
  begin
    Item := ItemByPoint(X, Y);
    if Item <> nil then
      ItemIndex := Item.Index;
  end;
end;

procedure TvgListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FMouseSelecting := false;
end;

procedure TvgListBox.VScrollChange(Sender: TObject);
begin
  ScrollList;
end;

procedure TvgListBox.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if not FDisableMouseWheel and (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := VScrollBar.Value - (WheelDelta / 5);
    Handled := true;
  end;
end;

function TvgListBox.GetSelected: TvgListBoxItem;
begin
  Result := ItemByIndex(FItemIndex);
end;

procedure TvgListBox.SetItemIndex(const Value: integer);
var
  Item: TvgListBoxItem;
begin
  if FItemIndex <> Value then
  begin
    Item := ItemByIndex(ItemIndex);
    if Item <> nil then Item.Repaint;

    FItemIndex := Value;
    if (ItemByIndex(FItemIndex) <> nil) and (FContent <> nil) and (VScrollBar <> nil) then
    begin
      Item := ItemByIndex(FItemIndex);
      if Item.Position.Y + Item.Padding.Top + Item.Padding.Bottom + Item.Height > FContent.Height then
        VScrollBar.Value := VScrollBar.Value + (Item.Position.Y + Item.Padding.Top + Item.Padding.Bottom + Item.Height - FContent.Height);
      if Item.Position.Y < 0 then
        VScrollBar.Value := VScrollBar.Value + Item.Position.Y;
    end;
    if (ItemIndex >= 0) and (Selection <> nil) then
    begin
      if HideSelectionUnfocused then
        Selection.Visible := IsFocused
      else
        Selection.Visible := true;
      Selection.Repaint;
      Item := ItemByIndex(ItemIndex);
      if Item <> nil then
        with Item do
        begin
          Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
          Repaint;
        end;
    end;
    if (ItemIndex < 0) and (Selection <> nil) then
    begin
      Selection.Visible := false;
      Selection.Height := 1;
    end;
    if Assigned(FOnChange) then
      FOnChange(ItemByIndex(FItemIndex));
  end;
end;

procedure TvgListBox.Clear;
var
  i: integer;
begin
  BeginUpdate;
  if FContent <> nil then
    if FContent.ChildrenCount > 0 then
      for i := FContent.ChildrenCount - 1 downto 0 do
        if FContent.Children[i] is TvgListboxItem then
        begin
          TvgObject(FContent.Children[i]).Free;
        end;
  EndUpdate;
end;

procedure TvgListBox.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TvgListBox.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating = 0 then
    Realign;
end;

procedure TvgListBox.EnterFocus;
begin
  inherited ;
  if HideSelectionUnfocused then
    if (ItemIndex >= 0) and (Selection <> nil) then
    begin
      Selection.Visible := true;
    end;
end;

procedure TvgListBox.KillFocus;
begin
  inherited;
  if HideSelectionUnfocused then
    if (Selection <> nil) then
    begin
      Selection.Visible := false;
    end;
end;

procedure TvgListBox.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and ((AObject is TvgListBoxItem) or (AObject is TvgListBoxSelection)) then
  begin
    FContent.AddObject(AObject);
    Realign;
  end
  else
    inherited;
end;

procedure TvgListBox.SetScrollBarSize(const Value: single);
begin
  FScrollBarSize := Value;
  if FScrollBarSize < 1 then
    FScrollBarSize := 1;
  if VScrollBar <> nil then
  begin
    VScrollBar.Width := FScrollBarSize;
    if not (csLoading in ComponentState) then
      Realign;
  end;
end;

procedure TvgListBox.SetColumns(const Value: integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    if FColumns < 1 then
      FColumns := 1;
    Realign;
  end;
end;

procedure TvgListBox.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgListBox.SetItemWidth(const Value: single);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Realign;
  end;
end;

{ TvgComboListBox }

constructor TvgComboListBox.Create(AOwner: TComponent);
begin
  inherited;
  HideSelectionUnfocused := false;
end;

destructor TvgComboListBox.Destroy;
begin
  inherited;
end;

procedure TvgComboListBox.ApplyStyle;
begin
  inherited;
end;

procedure TvgComboListBox.ApplyResource;
begin
  if FNeedResource then
  inherited;
end;

procedure TvgComboListBox.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  inherited ;
end;

function TvgComboListBox.GetParentComponent: TComponent;
begin
  Result := inherited GetParentComponent;
end;

procedure TvgComboListBox.KillFocus;
begin
  inherited;
end;

procedure TvgComboListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
end;

procedure TvgComboListBox.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
end;

procedure TvgComboListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if (Parent is TvgPopup) and (TvgPopup(Parent).IsOpen) and (FComboBox <> nil) then
  begin
    if vgPtInRect(vgPoint(X, Y), LocalRect) then
      if ItemByPoint(X, Y) <> nil then
        FComboBox.ItemIndex := ItemByPoint(X, Y).Index;
    TvgPopup(Parent).IsOpen := false;
  end;
end;

procedure TvgComboListBox.MouseWheel(Shift: TShiftState;
  WheelDelta: integer; var Handled: boolean);
begin
  inherited;
end;

{ TvgComboBox =================================================================}

constructor TvgComboBox.Create(AOwner: TComponent);
begin
  inherited;
  DropDownCount := 8;
  FPopup := TvgPopup.Create(Self);
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Parent;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FListBox := TvgComboListBox.Create(Self);
  FListBox.Parent := FPopup;
  FListBox.FComboBox := Self;
  FListBox.Stored := false;
  FListBox.Align := vaClient;
  Width := 100;
  Height := 23;
end;

destructor TvgComboBox.Destroy;
begin
  inherited;
end;

procedure TvgComboBox.ChangeParent;
begin
  inherited;
  FPopup.Parent := Parent;
end;

function TvgComboBox.ItemClass: string;
begin
  Result := 'TvgListBoxItem';
end;

procedure TvgComboBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('content');
  if (T <> nil) and (T is TvgContent) then
  begin
    TvgContent(T).OnPaint := DoContentPaint;
    if (FListBox <> nil) and (FListBox.ItemByIndex(ItemIndex) <> nil) then
    begin
      FListBox.ItemByIndex(ItemIndex).Width := TvgContent(T).Width;
    end;
  end;
end;

procedure TvgComboBox.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { content }
  if FPopup <> nil then
    FPopup.Width := Width;
  if FListBox <> nil then
    FListBox.Width := Width;
  FDisableAlign := false;
end;

procedure TvgComboBox.Paint;
begin
  inherited ;
end;

procedure TvgComboBox.PaintChildren;
begin
  inherited ;
end;

procedure TvgComboBox.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  SOpacity: single;
  Item: TvgListBoxItem;
  SaveSize: TvgPoint;
begin
  if FListBox <> nil then
  begin
    Item := FListBox.ItemByIndex(FListBox.ItemIndex);
    if Item <> nil then
    begin
      SOpacity := Item.FAbsoluteOpacity;
      SaveSize := vgPoint(Item.Width, Item.Height);
      Item.SetSizeWithoutChange(vgRectWidth(ARect), vgRectHeight(ARect));
      Item.FAbsoluteOpacity := Opacity;
      Item.RecalcOpacity;
      Item.FRecalcOpacity := false;
      Item.PaintTo(Canvas, ARect, TvgObject(Sender));
      Item.FAbsoluteOpacity := SOpacity;
      Item.RecalcOpacity;
      Item.SetSizeWithoutChange(SaveSize.X, SaveSize.Y);
    end;
  end;
end;

procedure TvgComboBox.DropDown;
var
  i: integer;
begin
  if not FPopup.IsOpen then
  begin
    FPopup.Placement := FPlacement;
    FPopup.Width := Width;
    if FListbox.ItemHeight > 0 then
      FPopup.Height := DropDownCount * FListbox.ItemHeight
    else
      FPopup.Height := DropDownCount * (Height - 4);
    FListBox.FNeedResource := true;
    FListBox.ApplyResource;
    FPopup.IsOpen := true;
    FListbox.SetFocus;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

procedure TvgComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    DropDown;
  end;
end;

procedure TvgComboBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
end;

procedure TvgComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgComboBox.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
{  if WheelDelta > 0 then
    ItemIndex := ItemIndex + 1
  else
    ItemIndex := ItemIndex - 1;
  Handled := true;             }
end;

procedure TvgComboBox.Clear;
begin
  if FListBox <> nil then
    FListBox.Clear;
end;

procedure TvgComboBox.AddObject(AObject: TvgObject);
begin
  if (FListBox <> nil) and ((AObject is TvgListBoxItem) or (AObject is TvgListBoxSelection)) then
  begin
    FListBox.AddObject(AObject);
    if (csDesigning in ComponentState) {or ((Scene <> nil) and (Scene.DesignTime)) }then
      TvgVisualObject(AObject).Locked := true
    else
      TvgVisualObject(AObject).Locked := false;
  end
  else
    inherited;
end;

procedure TvgComboBox.DoListBoxChange(Sender: TObject);
begin

end;

function TvgComboBox.GetItemIndex: integer;
begin
  if FListBox <> nil then
    Result := FListBox.ItemIndex
  else
    Result := -1;
end;

procedure TvgComboBox.SetItemIndex(const Value: integer);
begin
  if FListBox <> nil then
  begin
    FListBox.ItemIndex := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgComboBox.DesignClick;
begin
  inherited ;
  FPopup.DesignHide := not FPopup.DesignHide;
  FPopup.Width := Width;
  if FListbox.ItemHeight > 0 then
    FPopup.Height := DropDownCount * FListbox.ItemHeight
  else
    FPopup.Height := DropDownCount * (Height - 4);
  FPopup.Position.X := Position.X;
  FPopup.Position.Y := Position.Y + Height;
end;

function TvgComboBox.GetScrollBarSize: single;
begin
  if FListBox <> nil then
    Result := FListBox.ScrollBarSize
  else
    Result := 16;
end;

procedure TvgComboBox.SetScrollBarSize(const Value: single);
begin
  if FListBox <> nil then
    FListBox.ScrollBarSize := Value;
end;

procedure TvgComboBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  j: integer;
begin
  inherited;
  if (FListBox <> nil) and (FListBox.FContent <> nil) then
    if (FListBox.FContent.ChildrenCount > 0) then
    begin
      for j := 0 to FListBox.FContent.ChildrenCount - 1 do
        if FListBox.FContent.Children[j].Stored then
          Proc(TComponent(FListBox.FContent.Children[j]));
    end;
end;

{ TvgStringListBox }

procedure TvgStringListBox.ApplyStyle;
begin
  inherited;
  RebuildList;
end;

constructor TvgStringListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TvgWideStringList.Create;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FItemHeight := 19;
  FNeedResource := true;
  FResource := 'listboxstyle';
end;

destructor TvgStringListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TvgStringListBox.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
end;

procedure TvgStringListBox.EndUpdate;
begin
  inherited;
  if FUpdating = 0 then
    RebuildList;
end;

procedure TvgStringListBox.RebuildList;
var
  i: integer;
  Item: TvgListBoxItem;
  Text: TvgTextControl;
begin
  if FUpdating > 0 then Exit;
  if (csLoading in ComponentState) then Exit;
  if csDestroying in ComponentState then Exit;
  if Items = nil then Exit;

  Inc(FUpdating);
  FDisableAlign := true;
  if Scene <> nil then
    Scene.BeginUpdate;
  Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := TvgListBoxItem.Create(Self);
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.Locked := true;
    Text := TvgTextControl.Create(Item);
    Text.Parent := Item;
    Text.Text := FItems[i];
    Text.Align := vaContents;
    Text.Resource := 'listboxtextstyle';
    Text.TextAlign := TextAlign;
    Text.HitTest := false;
    Text.Locked := true;
    Text.Stored := false;
  end;
  FDisableAlign := false;
  Dec(FUpdating);
  Realign;
  if Scene <> nil then
    Scene.EndUpdateWOInvalidate;
end;

procedure TvgStringListBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgStringListBox.SetTextAlign(const Value: TvgTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    if not (csLoading in ComponentState) then
      RebuildList;
  end;
end;

{ TvgStringComboBox }

constructor TvgStringComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TvgWideStringList.Create;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FItemHeight := 19;
  FNeedResource := true;
  FResource := 'comboboxstyle';
end;

destructor TvgStringComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TvgStringComboBox.Clear;
begin
  inherited;
end;

procedure TvgStringComboBox.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
end;

procedure TvgStringComboBox.RebuildList;
var
  SaveI, i: integer;
  Item: TvgListBoxItem;
  Text: TvgTextControl;
begin
  if (csLoading in ComponentState) then Exit;
  if csDestroying in ComponentState then Exit;
  if Items = nil then Exit;
  if Scene <> nil then
    Scene.BeginUpdate;
  SaveI := FListbox.ItemIndex;
  FListbox.FItemIndex := -1;
  Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := TvgListBoxItem.Create(Self);
    Item.Parent := Self;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.Locked := true;
    Text := TvgTextControl.Create(Item);
    Text.Parent := Item;
    Text.Text := FItems[i];
    Text.Resource := 'comboboxtextstyle';
    Text.Align := vaContents;
    Text.TextAlign := TextAlign;
    Text.HitTest := false;
    Text.Locked := true;
    Text.Stored := false;
  end;
  Realign;
  FListbox.FItemIndex := SaveI;
  if FListbox.FItemIndex >= FListbox.Count then
    FListbox.FItemIndex := FListbox.Count - 1;
  if Scene <> nil then
    Scene.EndUpdateWOInvalidate; 
end;

procedure TvgStringComboBox.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RebuildList;
  end;
end;

procedure TvgStringComboBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgStringComboBox.SetTextAlign(const Value: TvgTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    if not (csLoading in ComponentState) then
      RebuildList;
  end;
end;

procedure TvgStringComboBox.ApplyStyle;
begin
  inherited;
  RebuildList;
end;

{ TvgHorzListBox }

constructor TvgHorzListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'listboxstyle';
end;

destructor TvgHorzListBox.Destroy;
begin
  inherited;
end;

procedure TvgHorzListBox.ScrollList;
var
  i, j, Idx: integer;
  R: TvgRect;
  ColWidth, RowHeight, CurY: single;
begin
  FDisableAlign := true;
  { content }
  if FContent <> nil then
  begin
    R := GetListRect;
    { scrollbar }
    if (VScrollBar <> nil) and (VScrollBar.Visible) then
      R.Bottom := VScrollBar.Position.Y;

    { align }
    CurY := 0;
    Idx := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        ColWidth := 0;
        if FItemHeight <> 0 then
          RowHeight := FItemHeight
        else
          RowHeight := (R.Bottom - R.Top) / FColumns;
        for j := 0 to FColumns - 1 do
        begin
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if VScrollBar <> nil then
                SetBounds(CurY + Padding.Left - VScrollBar.Value, Padding.Top + (j * RowHeight),
                  Width, RowHeight)
              else
                SetBounds(CurY + Padding.Left, Padding.Top + (j * RowHeight),
                  Width, RowHeight);
              if ColWidth < Width + Padding.Left + Padding.Right then
                ColWidth := Width + Padding.Left + Padding.Right;
              Index := Idx;
              if (ItemIndex = Idx) and (Selection <> nil) then
              begin
                if HideSelectionUnfocused then
                  Selection.Visible := IsFocused
                else
                  Selection.Visible := true;
                Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
              end;
              Inc(Idx);
            end;
        end;
        CurY := CurY + ColWidth;
      end;
    if ((ItemIndex < 0) or (Count = 0)) and (Selection <> nil) then
    begin
      Selection.Visible := false;
      Selection.Height := 1;
    end;
  end;
  FDisableAlign := false;
end;

procedure TvgHorzListBox.Realign;
var
  i, j, Idx: integer;
  R: TvgRect;
  ColWidth, RowHeight, CurY: single;
  SaveContent: TvgContent;
begin
  if FUpdating > 0 then Exit;
  SaveContent := FContent;
  FContent := nil;
  inherited Realign;
  FContent := SaveContent;

  if FDisableAlign then Exit;
  FDisableAlign := true;
  R := GetListRect;
  { content }
  if FContent <> nil then
  begin
    FContent.SetBounds(R.Left, R.Top, vgRectWidth(R), vgRectHeight(R));
    { correct items size }
    if FItemHeight <> 0 then
    begin
      if (VScrollBar <> nil) and (VScrollBar.Visible) then
        FColumns := trunc((R.Bottom - R.Top - Padding.Top - Padding.Bottom - VScrollBar.Height) / FItemHeight)
      else
        FColumns := trunc((R.Bottom - R.Top - Padding.Top - Padding.Bottom) / FItemHeight);
      if FColumns < 1 then FColumns := 1;
      if FContent.ChildrenCount > 0 then
        for i := 0 to (FContent.ChildrenCount - 1) do
          with TvgListBoxItem(FContent.Children[i]) do
          begin
            if FItemWidth <> 0 then
              SetBounds(Position.X, Position.Y, FItemWidth, FItemHeight)
            else
              SetBounds(Position.X, Position.Y, Width, FItemHeight);
          end;
    end;
    if (FItemHeight = 0) and (FItemWidth <> 0) then
    begin
      if FContent.ChildrenCount > 0 then
        for i := 0 to (FContent.ChildrenCount - 1) do
          with TvgListBoxItem(FContent.Children[i]) do
          begin
            SetBounds(Position.X, Position.Y, FItemWidth, Height)
          end;
    end;
    { calc items size }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        ColWidth := 0;
        if FItemHeight <> 0 then
          RowHeight := FItemHeight
        else
          RowHeight := (R.Bottom - R.Top) / FColumns;
        for j := 0 to FColumns - 1 do
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if ColWidth < Width + Padding.Left + Padding.Right then
                ColWidth := Width + Padding.Left + Padding.Right;
            end;
        // calc width 
        for j := 0 to FColumns - 1 do
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              Width := ColWidth - (Padding.Left + Padding.Right);
            end;

        CurY := CurY + ColWidth;
      end;
    { scrollbar }
    if VScrollBar <> nil then
    begin
      VScrollBar.Visible := CurY > vgRectWidth(R);
      if VScrollBar.Visible then
      begin
        VScrollBar.Orientation := vgHorizontal;
        VScrollBar.Height := ScrollBarSize;
        VScrollBar.Max := CurY;
        VScrollBar.ViewportSize := vgRectWidth(R);
        if Count > 0 then
          VScrollBar.SmallChange := CurY / Count;
        VScrollBar.SetBounds(R.Left, R.Bottom - VScrollBar.Height, vgRectWidth(R), VScrollBar.Height);
        R.Bottom := VScrollBar.Position.Y;
      end;
    end;
    { selection }
    if FItemIndex > Count - 1 then
      FItemIndex := Count - 1;
    { align }
    CurY := 0;
    Idx := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to (FContent.ChildrenCount - 1) div FColumns do
      begin
        ColWidth := 0;
        if FItemHeight <> 0 then
          RowHeight := FItemHeight
        else
          RowHeight := (R.Bottom - R.Top) / FColumns;
        for j := 0 to FColumns - 1 do
          if FContent.Children[(i * FColumns) + j] is TvgListBoxItem then
            with TvgListBoxItem(FContent.Children[(i * FColumns) + j]) do
            begin
              if VScrollBar <> nil then
                SetBounds(CurY + Padding.Left - VScrollBar.Value, Padding.Top + (j * RowHeight),
                  Width, RowHeight)
              else
                SetBounds(CurY + Padding.Left, Padding.Top + (j * RowHeight),
                  Width, RowHeight);
              if ColWidth < Width + Padding.Left + Padding.Right then
                ColWidth := Width + Padding.Left + Padding.Right;
              Index := Idx;
              if (ItemIndex = Idx) and (Selection <> nil) then
              begin
                if HideSelectionUnfocused then
                  Selection.Visible := IsFocused
                else
                  Selection.Visible := true;
                Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
              end;
              Inc(Idx);
            end;
        CurY := CurY + ColWidth;
      end;
    if ((ItemIndex < 0) or (Count = 0)) and (Selection <> nil) then
    begin
      Selection.Visible := false;
      Selection.Height := 1;
    end;
  end;
  FDisableAlign := false;
end;

procedure TvgHorzListBox.SetItemIndex(const Value: integer);
var
  Item: TvgListBoxItem;
begin
  if FItemIndex <> Value then
  begin
    Item := ItemByIndex(ItemIndex);
    if Item <> nil then Item.Repaint;

    FItemIndex := Value;
    if (ItemByIndex(FItemIndex) <> nil) and (FContent <> nil) and (VScrollBar <> nil) then
    begin
      Item := ItemByIndex(FItemIndex);
      if Item.Position.X + Item.Padding.Left + Item.Padding.Right + Item.Width > FContent.Width then
        VScrollBar.Value := VScrollBar.Value + (Item.Position.X + Item.Padding.Left + Item.Padding.Right + Item.Width - FContent.Width);
      if Item.Position.X < 0 then
        VScrollBar.Value := VScrollBar.Value + Item.Position.X;
    end;
    if (ItemIndex >= 0) and (Selection <> nil) then
    begin
      Selection.Visible := true;
      Selection.Repaint;
      Item := ItemByIndex(ItemIndex);
      if Item <> nil then
        with Item do
        begin
          Selection.SetBounds(FContent.Position.X + Position.X, FContent.Position.Y + Position.Y, Width, Height);
          Repaint;
        end;
    end;
    if (ItemIndex < 0) and (Selection <> nil) then
    begin
      Selection.Visible := false;
      Selection.Width := 1;
    end;
    if Assigned(FOnChange) then
      FOnChange(ItemByIndex(FItemIndex));
  end;
end;

procedure TvgHorzListBox.SetScrollBarSize(const Value: single);
begin
  FScrollBarSize := Value;
  if FScrollBarSize < 1 then
    FScrollBarSize := 1;
  if VScrollBar <> nil then
  begin
    VScrollBar.Height := FScrollBarSize;
    if not (csLoading in ComponentState) then
      Realign;
  end;
end;

procedure TvgHorzListBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if Count > 0 then
  begin
    case Key of
      VK_HOME: ItemIndex := 0;
      VK_END: ItemIndex := Count - FColumns;
      VK_LEFT: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - FColumns;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_RIGHT:
        begin
          If ItemIndex < Count - 1 then ItemIndex := ItemIndex + FColumns;
          if ItemIndex > Count - 1 then ItemIndex := Count - 1;
        end;
      VK_UP: If ItemIndex > 0 then ItemIndex := ItemIndex - 1;
      VK_DOWN: If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
    else
      inherited ;
    end;
    Key := 0;
  end;
  inherited;
end;

{ TvgHudStringListBox }

constructor TvgHudStringListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudlistboxstyle';
end;

{ TvgHudStringComboBox }

constructor TvgHudStringComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudcomboboxstyle';
  FListBox.Resource := 'hudcombolistboxstyle';
end;

{ TvgHudListBox }

constructor TvgHudListBox.Create(AOwner: TComponent);
begin
  inherited;
end;

{ TvgHudHorzListBox }

constructor TvgHudHorzListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
  FResource := 'hudlistboxstyle';
end;

{ TvgHudComboBox }

constructor TvgHudComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FListBox.Resource := 'hudcombolistboxstyle';
end;

initialization
  RegisterVGObjects('Controls', [TvgListBox, TvgListBoxSelection, TvgComboBox, TvgStringListBox, TvgStringComboBox, TvgHorzListBox]);
  RegisterVGObjects('Items', [TvgListBoxItem]);
  RegisterVGObjects('HUD', [TvgHudListBox, TvgHudHorzListBox, TvgHudComboBox, TvgHudStringListBox, TvgHudStringComboBox]);
end.


