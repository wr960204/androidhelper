unit vg_treeview;

{$I vg_define.inc}

interface

uses Classes, SysUtils, Forms, Controls, vg_utils,
  vg_scene, vg_objects, vg_controls;

type

  TvgTreeView = class;

  TvgTreeViewItem = class(TvgExpander)
  private
  protected
    procedure ApplyStyle; override;
    procedure SetIsExpanded(const Value: boolean); override;
    function TreeView: TvgTreeView;
    function TreeItem: TvgTreeViewItem;
    procedure DesignInsert; override;
    procedure DesignSelect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    function ItemByPoint(const X, Y: single): TvgTreeViewItem;
    function ItemByIndex(const Idx: integer): TvgTreeViewItem;
    { design }
    function ItemClass: string; override;
  published
    property Resource;
  end;

  TvgTreeViewSelection = class(TvgControl)
  private
  protected
    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Resource;
  end;

  TvgTreeView = class(TvgControl)
  private
    FMouseSelecting: boolean;
    FOnChange: TNotifyEvent;
    FSelected: TvgTreeViewItem;
    FUpdating: boolean;
    FItemHeight: single;
    FCountExpanded: integer;
    FHideSelectionUnfocused: boolean;
    function GetCount: integer;
    function GetVScrollBarWidth: single;
    procedure SetVScrollBarWidth(const Value: single);
    procedure SetItemHeight(const Value: single);
  protected
    FContent: TvgContent;
    FVScrollBar: TvgScrollBar;
    FSelection: TvgTreeViewSelection;
    procedure ApplyStyle; override;
    procedure VScrollChange(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetListRect: TvgRect;
    procedure SetSelected(const Value: TvgTreeViewItem); virtual;
    procedure Scroll;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemClass: string; override;
    procedure Realign; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ExpandAll;
    procedure CollapseAll;
    function ItemByPoint(const X, Y: single): TvgTreeViewItem;
    function ItemByIndex(const Idx: integer): TvgTreeViewItem;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    property Count: integer read GetCount;
    property CountExpanded: integer read FCountExpanded;
    property Selected: TvgTreeViewItem read FSelected write SetSelected;
    property Selection: TvgTreeViewSelection read FSelection write FSelection;
    property VScrollBar: TvgScrollBar read FVScrollBar write FVScrollBar;
  published
    property Resource;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property HideSelectionUnfocused: boolean read FHideSelectionUnfocused write FHideSelectionUnfocused default true;
    property VScrollBarWidth: single read GetVScrollBarWidth write SetVScrollBarWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation {===============================================================}

uses Clipbrd, vg_ani;

{ TvgTreeViewItem }

constructor TvgTreeViewItem.Create(AOwner: TComponent);
begin
  inherited;
  FButton.Width := 17;
  FButton.Height := 17;
  FButton.Resource := 'treeviewexpanderbuttonstyle';
  FContent.Margins.left := 12;
  FContent.Padding.top := 17;
  Height := 17;
  HitTest := false;
  ClipChildren := true;
  TextAlign := vgTextAlignNear;
end;

procedure TvgTreeViewItem.ApplyStyle;
begin
  inherited;
end;

destructor TvgTreeViewItem.Destroy;
begin
  inherited;
end;

procedure TvgTreeViewItem.Realign;
var
  i: integer;
  R: TvgRect;
  CurY: single;
  T: TvgObject;
  ItemHeight: single;
  SaveContent: TvgContent;
begin
  if (TreeView <> nil) and (TreeView.FUpdating) then Exit;

  SaveContent := FContent;
  FContent := nil;
  try
    inherited ;
  finally
    FContent := SaveContent;
  end;

  if FDisableAlign then Exit;
  FDisableAlign := true;
  { calc count }
  if TreeView <> nil then
    TreeView.FCountExpanded := TreeView.FCountExpanded + 1;
  { calc height }
  if TreeView <> nil then
    ItemHeight := TreeView.ItemHeight
  else
    ItemHeight := 19;
  { text }
  T := FindResource('text');
  if (T <> nil) and (T.IsVisual) then
    TvgVisualObject(T).Height := ItemHeight;
  { content }
  if (FContent <> nil) and IsExpanded then
  begin
    { content }
    FContent.SetBounds(0, FContent.Padding.Top, Width, 1000);
    R := FContent.BoundsRect;
    { calc items size }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to FContent.ChildrenCount - 1 do
        if FContent.Children[i] is TvgTreeViewItem then
          with TvgTreeViewItem(FContent.Children[i]) do
          begin
            Realign;
            CurY := CurY + Height + Padding.Top + Padding.Bottom;
          end;
    if FButton <> nil then
      FButton.Visible := CurY > 0;
    { align }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to FContent.ChildrenCount - 1 do
        if FContent.Children[i] is TvgTreeViewItem then
          with TvgTreeViewItem(FContent.Children[i]) do
          begin
            SetBounds(Padding.Left + FContent.Margins.Left, CurY + Padding.Top, R.Right - R.Left - Padding.Left - Padding.Right - FContent.Margins.Left,
              Height);
            CurY := CurY + Height + Padding.Top + Padding.Bottom;
          end;
    if not IsExpanded or (CurY = 0) then
    begin
      Height := ItemHeight;
      if FContent <> nil then
        FContent.Padding.top := ItemHeight;
      if FButton <> nil then
      begin
        FButton.Width := ItemHeight;
        FButton.Height := ItemHeight;
      end;
    end
    else
      Height := CurY + FContent.Position.Y;
  end;
  FDisableAlign := false;
end;

function TvgTreeViewItem.ItemByIndex(const Idx: integer): TvgTreeViewItem;
var
  c, i: integer;
begin
  c := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
      begin
        if c = Idx then
        begin
          Result := TvgTreeViewItem(FContent.Children[i]);
          Exit;
        end;
        Inc(c);
      end;
  Result := nil;
end;

function TvgTreeViewItem.ItemByPoint(const X, Y: single): TvgTreeViewItem;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocaltoAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
        if TvgTreeViewItem(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          P1 := TvgTreeViewItem(FContent.Children[i]).AbsoluteToLocal(P);
          Result := TvgTreeViewItem(FContent.Children[i]).ItemByPoint(P1.X, P1.Y);
          if Result = nil then
            Result := TvgTreeViewItem(FContent.Children[i]);
          Exit;
        end;
  Result := nil;
end;

function TvgTreeViewItem.TreeView: TvgTreeView;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgTreeView then
    begin
      Result := TvgTreeView(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

function TvgTreeViewItem.TreeItem: TvgTreeViewItem;
var
  P: TvgObject;
begin
  P := Parent;
  while (P <> nil) do
  begin
    if P is TvgTreeViewItem then
    begin
      Result := TvgTreeViewItem(P);
      Exit;
    end;
    P := P.Parent;
  end;
  Result := nil;
end;

procedure TvgTreeViewItem.SetIsExpanded(const Value: boolean);
begin
  if IsExpanded <> Value then
  begin
    if Value then
    begin
      FIsExpanded := true;
      Realign;
      FIsExpanded := false;
    end;
    inherited;
    if TreeView <> nil then
      TreeView.Realign;
  end;
end;

procedure TvgTreeViewItem.DesignInsert;
begin
  inherited;
  if (TreeItem <> nil) and not TreeItem.IsExpanded then
    TreeItem.DesignClick;
  if TreeItem <> nil then
    TreeItem.Realign;
  if TreeView <> nil then
    TreeView.Realign;
end;

procedure TvgTreeViewItem.DesignSelect;
begin
  inherited;
  if TreeView <> nil then
    TreeView.Selected := Self;
end;

function TvgTreeViewItem.ItemClass: string;
begin
  Result := ClassName;
end;

{ TvgTreeViewSelection }

constructor TvgTreeViewSelection.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := false;
end;

procedure TvgTreeViewSelection.Paint;
begin
  inherited;
end;

procedure TvgTreeViewSelection.PaintChildren;
begin
  if csDesigning in ComponentState then Exit;
  inherited;
end;

{ TvgTreeView ==================================================================}

constructor TvgTreeView.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := true;
  AutoCapture := true;
  HideSelectionUnfocused := true;
  Width := 100;
  Height := 100;
  FItemHeight := 19;
  FVScrollBar := TvgScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.OnChange := VScrollChange;
  FVScrollBar.Orientation := vgVertical;
  FVScrollBar.Locked := true;
  FVScrollBar.Stored := false;
  FVScrollBar.Width := 18;
  FContent := TvgContent.Create(Self);
  FContent.Parent := Self;
  FContent.ClipChildren := true;
  FContent.HitTest := false;
  FContent.Stored := false;
  FContent.Locked := true;
  FSelection := TvgTreeViewSelection.Create(Self);
  FSelection.Locked := true;
  FSelection.Parent := Self;
  FSelection.Stored := false;
end;

destructor TvgTreeView.Destroy;
begin
  inherited;
end;

procedure TvgTreeView.ApplyStyle;
begin
  inherited;
  Realign;
end;

function TvgTreeView.GetListRect: TvgRect;
var
  B: TvgObject;
begin
  B := FindResource('background');
  if (B <> nil) and (B is TvgVisualObject) then
    Result := TvgVisualObject(B).Margins.MarginRect(LocalRect)
  else
    Result := Margins.MarginRect(LocalRect);
end;

procedure TvgTreeView.Scroll;
var
  i: integer;
  R: TvgRect;
  P: TvgPoint;
  CurY: single;
  C: integer;
begin
  if FUpdating then Exit;
  FDisableAlign := true;
  R := GetListRect;
  { content }
  if FContent <> nil then
  begin
    Scene.BeginUpdate;
    { scrollbar }
    if (FVScrollBar <> nil) and (FVScrollBar.Visible) then
      R.Right := FVScrollBar.Position.X;
    { align }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to FContent.ChildrenCount - 1 do
        if FContent.Children[i] is TvgTreeViewItem then
          with TvgTreeViewItem(FContent.Children[i]) do
          begin
            Position.X := Padding.Left;
            if FVScrollBar.Visible then
              SetBounds(Padding.Left, CurY + Padding.Top - FVScrollBar.Value,
                R.Right - R.Left - Padding.Left - Padding.Right, Height)
            else
              SetBounds(Padding.Left, CurY + Padding.Top,
                R.Right - R.Left - Padding.Left - Padding.Right, Height);
            CurY := CurY + Height + Padding.Top + Padding.Bottom;
          end;
    if (Selected <> nil) and (FSelection <> nil) then
    begin
      P := Selected.LocalToAbsolute(vgPoint(0, 0));
      P := FContent.AbsolutetoLocal(P);
      FSelection.SetBounds(P.X, P.Y, Selected.Width, ItemHeight);
      if HideSelectionUnfocused then
        Selection.Visible := IsFocused
      else
        Selection.Visible := true;
    end;
    if (Selected = nil) and (FSelection <> nil) then
    begin
      FSelection.Visible := false;
      FSelection.Height := 1;
    end;
    Scene.EndUpdateWOInvalidate;
    Repaint;
  end;
  FDisableAlign := false;
end;

procedure TvgTreeView.Realign;
var
  i: integer;
  R: TvgRect;
  P: TvgPoint;
  CurY: single;
  C: integer;
begin
  if FUpdating then Exit;
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  R := GetListRect;
  { content }
  FCountExpanded := 0;
  if FContent <> nil then
  begin
    FContent.Position.X := R.Left;
    FContent.Position.Y := R.Top;
    FContent.Width := vgRectWidth(R);
    FContent.Height := vgRectHeight(R);
    { calc items size }
    CurY := 0;
    C := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to FContent.ChildrenCount - 1 do
        if FContent.Children[i] is TvgTreeViewItem then
          with TvgTreeViewItem(FContent.Children[i]) do
          begin
            Realign;
            CurY := CurY + Height + Padding.Top + Padding.Bottom;
            C := C + 1;
          end;
    { scrollbar }
    if FVScrollBar <> nil then
    begin
      FVScrollBar.Visible := CurY > vgRectHeight(R);
      if FVScrollBar.Visible then
      begin
        FVScrollBar.Max := CurY;
        FVScrollBar.ViewportSize := vgRectHeight(R);
        if C > 0 then
          FVScrollBar.SmallChange := CurY / C;
        FVScrollBar.SetBounds(R.Right - FVScrollBar.Width, R.Top, FVScrollBar.Width, vgRectHeight(R));
        R.Right := FVScrollBar.Position.X;
      end
      else
        FVScrollBar.Value := 0;
    end;
    { align }
    CurY := 0;
    if FContent.ChildrenCount > 0 then
      for i := 0 to FContent.ChildrenCount - 1 do
        if FContent.Children[i] is TvgTreeViewItem then
          with TvgTreeViewItem(FContent.Children[i]) do
          begin
            Position.X := Padding.Left;
            if FVScrollBar.Visible then
              SetBounds(Padding.Left, CurY + Padding.Top - FVScrollBar.Value,
                R.Right - R.Left - Padding.Left - Padding.Right, Height)
            else
              SetBounds(Padding.Left, CurY + Padding.Top,
                R.Right - R.Left - Padding.Left - Padding.Right, Height);
            CurY := CurY + Height + Padding.Top + Padding.Bottom;
          end;
    if (Selected <> nil) and (FSelection <> nil) then
    begin
      P := Selected.LocalToAbsolute(vgPoint(0, 0));
      P := FContent.AbsolutetoLocal(P);
      FSelection.SetBounds(P.X, P.Y, Selected.Width, ItemHeight);
      if HideSelectionUnfocused then
        Selection.Visible := IsFocused
      else
        Selection.Visible := true;
    end;
    if (Selected = nil) and (FSelection <> nil) then
    begin
      FSelection.Visible := false;
      FSelection.Height := 1;
    end;
  end;
  FDisableAlign := false;
end;

function TvgTreeView.GetCount: integer;
var
  i: integer;
begin
  Result := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
      begin
        Inc(Result);
      end;
end;

function TvgTreeView.ItemByIndex(const Idx: integer): TvgTreeViewItem;
var
  c, i: integer;
begin
  c := 0;
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
      begin
        if c = Idx then
        begin
          Result := TvgTreeViewItem(FContent.Children[i]);
          Exit;
        end;
        Inc(c);
      end;
  Result := nil;
end;

function TvgTreeView.ItemByPoint(const X, Y: single): TvgTreeViewItem;
var
  i: integer;
  P, P1: TvgPoint;
begin
  P := LocalToAbsolute(vgPoint(X, Y));
  if (FContent <> nil) and (FContent.ChildrenCount > 0) then
    for i := 0 to FContent.ChildrenCount - 1 do
      if FContent.Children[i] is TvgTreeViewItem then
        if TvgTreeViewItem(FContent.Children[i]).pointInObject(P.X, P.Y) then
        begin
          P1 := TvgTreeViewItem(FContent.Children[i]).AbsoluteToLocal(P);
          Result := TvgTreeViewItem(FContent.Children[i]).ItemByPoint(P1.X, P1.Y);
          if Result = nil then
            Result := TvgTreeViewItem(FContent.Children[i]);
          Exit;
        end;
  Result := nil;
end;

procedure TvgTreeView.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgTreeView.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    Selected := ItemByPoint(X, Y);
    FMouseSelecting := true;
  end;
end;

procedure TvgTreeView.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if (ssLeft in Shift) and FMouseSelecting then
  begin
    Selected := ItemByPoint(X, Y);
  end;
end;

procedure TvgTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FMouseSelecting := false;
end;

procedure TvgTreeView.VScrollChange(Sender: TObject);
begin
  Scroll;
end;

procedure TvgTreeView.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if (FVScrollBar <> nil) and (FVScrollBar.Visible) then
  begin
    FVScrollBar.Value := FVScrollBar.Value - (WheelDelta / 5);
    Handled := true;
  end;
end;

procedure TvgTreeView.Clear;
var
  i: integer;
begin
  if FContent <> nil then
    if FContent.ChildrenCount > 0 then
      for i := FContent.ChildrenCount - 1 downto 0 do
        if FContent.Children[i] is TvgTreeViewItem then
        begin
          TvgObject(FContent.Children[i]).Free;
        end;
end;

procedure TvgTreeView.EnterFocus;
begin
  inherited;
  if HideSelectionUnfocused then
    if (Selection <> nil) then
    begin
      Selection.Visible := true;
    end;
end;

procedure TvgTreeView.KillFocus;
begin
  inherited;
  if HideSelectionUnfocused then
    if (Selection <> nil) then
    begin
      Selection.Visible := false;
    end;
end;

procedure TvgTreeView.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and ((AObject is TvgTreeViewItem) or (AObject is TvgTreeViewSelection)) then
  begin
    FContent.AddObject(AObject);
    if not FUpdating then
      Realign;
  end
  else
    inherited;
end;

procedure TvgTreeView.SetSelected(const Value: TvgTreeViewItem);
var
  i: TvgObject;
  P: TvgPoint;
begin
  if FSelected <> Value then
  begin
    if FSelected <> nil then
      FSelected.Repaint;
    FSelected := Value;
    if (FSelected <> nil) and (FContent <> nil) then
    begin
      i := FSelected.Parent;
      while ((i <> nil) and not (i is TvgTreeView)) do
      begin
        if (i is TvgTreeViewItem) then
          TvgTreeViewItem(i).IsExpanded := true;
        i := i.Parent;
      end;
      if FVScrollBar <> nil then
      begin
        P := AbsoluteToLocal(FSelected.LocalToAbsolute(vgPoint(0, 0)));
        if P.Y < 0 then
          FVScrollBar.Value := FVScrollBar.Value + P.Y;
        if P.Y + FSelected.Padding.Top + FSelected.Padding.Bottom + FSelected.Height > FContent.Height then
          FVScrollBar.Value := FVScrollBar.Value + (P.Y + FSelected.Padding.Top + FSelected.Padding.Bottom + FSelected.Height - FContent.Height);
      end;
      if (Selected <> nil) and (FSelection <> nil) then
      begin
        FSelection.Repaint;
        P := Selected.LocalToAbsolute(vgPoint(0, 0));
        P := FContent.AbsolutetoLocal(P);
        FSelection.SetBounds(P.X, P.Y, Selected.Width, ItemHeight);
        if HideSelectionUnfocused then
          Selection.Visible := IsFocused
        else
          Selection.Visible := true;
      end;
    end;
    if FSelected <> nil then
      FSelected.Repaint;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSelected) then
    FSelected := nil;
end;

function TvgTreeView.ItemClass: string;
begin
  Result := 'TvgTreeViewItem';
end;

procedure TvgTreeView.BeginUpdate;
begin
  FUpdating := true;
end;

procedure TvgTreeView.EndUpdate;
begin
  FUpdating := false;
  Realign;
end;

function TvgTreeView.GetVScrollBarWidth: single;
begin
  if FVScrollBar <> nil then
    Result := FVScrollBar.Width
  else
    Result := 16;
end;

procedure TvgTreeView.SetVScrollBarWidth(const Value: single);
begin
  if FVScrollBar <> nil then
  begin
    FVScrollBar.Width := Value;
    if not (csLoading in ComponentState) then
      Realign;
  end;
end;

procedure TvgTreeView.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgTreeView.CollapseAll;
var
  i: integer;
  item: TvgTreeViewItem;
begin
  BeginUpdate;
  for i := 0 to Count - 1 do
  begin
    item := ItemByIndex(i);
    if item <> nil then
      item.IsExpanded := false;
  end;
  EndUpdate;
end;

procedure TvgTreeView.ExpandAll;
var
  i: integer;
  item: TvgTreeViewItem;
begin
  BeginUpdate;
  for i := 0 to Count - 1 do
  begin
    item := ItemByIndex(i);
    if item <> nil then
      item.IsExpanded := true;
  end;
  EndUpdate;
end;

initialization
  RegisterVGObjects('Controls', [TvgTreeView]);
  RegisterVGObjects('Items', [TvgTreeViewItem]);
end.


