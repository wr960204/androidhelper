unit vg_layouts;

{$I vg_define.inc}
{$H+}

interface

uses Classes, SysUtils, vg_utils,
  vg_scene, vg_controls;

type

  TvgLayout = class(TvgVisualObject)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property HitTest default false;
  end;

  TvgScaledLayout = class(TvgVisualObject)
  private
    FOriginalWidth: single;
    FOriginalHeight: single;
    procedure SetOriginalWidth(const Value: single);
    procedure SetOriginalHeight(const Value: single);
  protected
    function GetChildrenMatrix: TvgMatrix; override;
    procedure SetHeight(const Value: single); override;
    procedure SetWidth(const Value: single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property OriginalWidth: single read FOriginalWidth write SetOriginalWidth;
    property OriginalHeight: single read FOriginalHeight write SetOriginalHeight;
  end;

  TvgScrollLayout = class(TvgContent)
  private
  protected
    function GetClipRect: TvgRect; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgScrollBox = class(TvgControl)
  private
    FHScrollBar: TvgScrollBar;
    FVScrollBar: TvgScrollBar;
    FLayout: TvgScrollLayout;
    FAutoHide: boolean;
    FScrollResource: string;
    FDisableMouseWheel: boolean;
    procedure SetScrollResource(const Value: string);
  protected
    procedure HScrollChange(Sender: TObject);
    procedure VScrollChange(Sender: TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure Realign; override;
    procedure Centre;
    procedure ScrollTo(const Dx, Dy: single);
    procedure InViewRect(const Rect: TvgRect);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property AutoHide: boolean read FAutoHide write FAutoHide default true;
    property HScrollBar: TvgScrollBar read FHScrollBar write FHScrollBar;
    property VScrollBar: TvgScrollBar read FVScrollBar write FVScrollBar;
    property DisableMouseWheel: boolean read FDisableMouseWheel write FDisableMouseWheel;
    property ScrollResource: string read FScrollResource write SetScrollResource;
  end;

  TvgGrid = class(TvgVisualObject)
  private
    FItemWidth: single;
    FItemHeight: single;                             
    FOrientation: TvgOrientation;
    procedure SetItemHeight(const Value: single);
    procedure SetItemWidth(const Value: single);
    procedure SetOrientation(const Value: TvgOrientation);
  protected
    procedure Realign; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property ItemWidth: single read FItemWidth write SetItemWidth;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
  end;

implementation {===============================================================}

type
  TvgHackScrollBar = class(TvgScrollBar);

{ TvgLayout ===================================================================}

constructor TvgLayout.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := false;
end;

destructor TvgLayout.Destroy;
begin
  inherited;
end;

procedure TvgLayout.Paint;
var
  R: TvgRect;
begin
  if Assigned(Scene) and Scene.GetDesignTime and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := vgDashSolid;
  end;
end;

{ TvgScrollLayout }

constructor TvgScrollLayout.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := true;
end;

function TvgScrollLayout.GetClipRect: TvgRect;
begin
  if (Parent <> nil) and (Parent is TvgScrollBox) then
  begin
    Result := vgRect(0, 0, TvgScrollBox(Parent).Width, TvgScrollBox(Parent).Height);
    if TvgScrollBox(Parent).VScrollBar.Enabled then
      Result.Right := Result.Right - TvgScrollBox(Parent).VScrollBar.Width;
    if TvgScrollBox(Parent).HScrollBar.Enabled then
      Result.Bottom := Result.Bottom - TvgScrollBox(Parent).HScrollBar.Height;
    vgOffsetRect(Result, -Position.X, -Position.Y);
  end
  else
    Result := inherited GetClipRect;
end;

{ TvgScrollBox }

constructor TvgScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FDisableEffect := true;
  FAutoHide := true;
  FLayout := TvgScrollLayout.Create(Self);
  FLayout.Parent := Self;
  FLayout.ClipChildren := true;
  FLayout.Stored := false;
  FLayout.Locked := true;
  FHScrollBar := TvgScrollBar.Create(Self);
  FHScrollBar.Parent := Self;
  FHScrollBar.OnChange := HScrollChange;
  FHScrollBar.Orientation := vgHorizontal;
  FHScrollBar.Locked := true;
  FHScrollBar.Stored := false;
  FHScrollBar.Height := 18;
  FVScrollBar := TvgScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.OnChange := VScrollChange;
  FVScrollBar.Orientation := vgVertical;
  FVScrollBar.Locked := true;
  FVScrollBar.Stored := false;
  FVScrollBar.Width := 18;
  ClipChildren := true;
end;

destructor TvgScrollBox.Destroy;
begin
  FLayout := nil;
  FHScrollBar := nil;
  FVScrollBar := nil;
  inherited;
end;

procedure TvgScrollBox.Realign;
var
  i: integer;
  R, LocalR: TvgRect;
begin
  if csDestroying in ComponentState then Exit;
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  try
    if (FHScrollBar <> nil) and (FVScrollBar <> nil) and (FLayout <> nil) then
    begin
      R := LocalRect;
      if FLayout.FChildren <> nil then
        for i := 0 to FLayout.FChildren.Count - 1 do
          if TvgObject(FLayout.FChildren[i]).isVisual then
            if (TvgVisualObject(FLayout.FChildren[i]).Visible) then
            begin
              if (TvgObject(FLayout.FChildren[i]) = FHScrollBar) or (TvgObject(FLayout.FChildren[i]) = FVScrollBar) then Continue;
              LocalR := TvgVisualObject(FLayout.FChildren[i]).LocalRect;
              vgOffsetRect(LocalR, TvgVisualObject(FLayout.FChildren[i]).Position.X, TvgVisualObject(FLayout.FChildren[i]).Position.Y);
              R := vgUnionRect(R, LocalR);
            end;

      FHScrollBar.Enabled := vgRectWidth(R) > Width;
      FVScrollBar.Enabled := vgRectHeight(R) > Height;

      FHScrollBar.Position.X := 0;
      FHScrollBar.Position.Y := Height - FHScrollBar.Height;
      FHScrollBar.Width := Width - FVScrollBar.Width;
      if not FVScrollBar.Enabled and FAutoHide then
        FHScrollBar.Width := Width;
      FHScrollBar.Min := R.Left;
      FHScrollBar.Max := R.Right;
      FHScrollBar.ViewportSize := Width;
      if FVScrollBar.Enabled and (vgRectWidth(R) > Width) then
      begin
        FHScrollBar.ViewportSize := FHScrollBar.ViewportSize - FVScrollBar.Width;
      end;
      FHScrollBar.SmallChange := 5;
      if FAutoHide then
        FHScrollBar.Visible := FHScrollBar.Enabled;
      FHScrollBar.BringToFront;

      FVScrollBar.Position.X := Width - FVScrollBar.Width;
      FVScrollBar.Position.Y := 0;
      FVScrollBar.Height := Height - FHScrollBar.Height;
      if not FHScrollBar.Enabled and FAutoHide then
        FVScrollBar.Height := Height;
      FVScrollBar.Max := vgRectHeight(R);
      FVScrollBar.ViewportSize := Height;
      if FHScrollBar.Enabled and (vgRectHeight(R) > Height) then
      begin
        FVScrollBar.ViewportSize := FVScrollBar.ViewportSize - FHScrollBar.Height;
      end;
      FVScrollBar.SmallChange := 5;
      if FAutoHide then
        FVScrollBar.Visible := FVScrollBar.Enabled;
      FVScrollBar.BringToFront;

      if FHScrollBar.Visible then
        FLayout.Position.X := -FHScrollBar.Value
      else
        FLayout.Position.X := 0;
      if FVScrollBar.Visible then
        FLayout.Position.Y := -FVScrollBar.Value
      else
        FLayout.Position.Y := 0;
      FLayout.Width := vgRectWidth(R);
      FLayout.Height := vgRectHeight(R);
    end;
  finally
    FDisableAlign := false;
  end;
end;

procedure TvgScrollBox.HScrollChange(Sender: TObject);
begin
  Realign;
end;

procedure TvgScrollBox.VScrollChange(Sender: TObject);
begin
  Realign;
end;

procedure TvgScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if not Handled and not (FDisableMouseWheel) and (FVScrollBar <> nil) and (FVScrollBar.Visible) then
  begin
    FVScrollBar.Value := FVScrollBar.Value - (WheelDelta / 5);
    Handled := true;
  end;
end;

procedure TvgScrollBox.SetScrollResource(const Value: string);
begin
  if FScrollResource <> Value then
  begin
    FScrollResource := Value;
    if (FVScrollBar <> nil) and (FVScrollBar.Visible) then
      FVScrollBar.Resource := FScrollResource;
    if (FHScrollBar <> nil) and (FHScrollBar.Visible) then
      FHScrollBar.Resource := FScrollResource;
  end;
end;

procedure TvgScrollBox.AddObject(AObject: TvgObject);
begin
  if (FLayout <> nil) and (AObject <> FVScrollBar) and (AObject <> FHScrollBar) and (AObject <> FLayout) then
  begin
    FLayout.AddObject(AObject);
    Realign;
  end
  else
    inherited;
end;

procedure TvgScrollBox.Loaded;
begin
  inherited;
end;

procedure TvgScrollBox.Centre;
begin
  if (FVScrollBar <> nil) and (FVScrollBar.Visible) then
  begin
    FVScrollBar.Value := (FVScrollBar.Max - FVScrollBar.ViewportSize) / 2;
  end;
  if (FHScrollBar <> nil) and (FHScrollBar.Visible) then
  begin
    FHScrollBar.Value := (FHScrollBar.Max - FHScrollBar.ViewportSize) / 2;
  end;
end;

procedure TvgScrollBox.ScrollTo(const Dx, Dy: single);
begin
  if (FVScrollBar <> nil) and (FVScrollBar.Visible) then
    FVScrollBar.Value := FVScrollBar.Value - Dy;
  if (FHScrollBar <> nil) and (FHScrollBar.Visible) then
    FHScrollBar.Value := FHScrollBar.Value - Dx;
end;

procedure TvgScrollBox.InViewRect(const Rect: TvgRect);
begin

end;

procedure TvgScrollBox.BeginUpdate;
begin
  FDisableAlign := true;
end;

procedure TvgScrollBox.EndUpdate;
begin
  FDisableAlign := false;
  Realign;
end;

{ TvgGrid }

constructor TvgGrid.Create(AOwner: TComponent);
begin
  inherited;
  ItemHeight := 64;
  ItemWidth := 64;
end;

procedure TvgGrid.Realign;
var
  i: integer;
  CurPos: TvgPoint;
begin
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { content }
  CurPos := vgPoint(Margins.Left, Margins.Top);
  for i := 0 to ChildrenCount - 1 do
    if Children[i].IsVisual then
      with Children[i].Visual do
      begin
        SetBounds(CurPos.X + Padding.Left, CurPos.Y + Padding.Top, FItemWidth - Padding.Left - Padding.right,
          FItemHeight - Padding.top - Padding.bottom);
        if Orientation = vgHorizontal then
        begin
          CurPos.X := CurPos.X + FItemWidth;
          if CurPos.X + FItemWidth > Self.Width - Self.Margins.Left - Self.Margins.Right then
          begin
            CurPos.X := Self.Margins.Left;
            CurPos.Y := CurPos.Y + FItemHeight;
          end;
        end
        else
        begin
          CurPos.Y := CurPos.Y + FItemHeight;
          if CurPos.Y + FItemHeight > Self.Height - Self.Margins.Top - Self.Margins.Bottom then
          begin
            CurPos.Y := Self.Margins.Top;
            CurPos.X := CurPos.X + FItemWidth;
          end;
        end;
      end;
  FDisableAlign := false;
end;

procedure TvgGrid.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Realign;
  end;
end;

procedure TvgGrid.SetItemWidth(const Value: single);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    Realign;
  end;
end;

procedure TvgGrid.SetOrientation(const Value: TvgOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

{ TvgScaledLayout }

constructor TvgScaledLayout.Create(AOwner: TComponent);
begin
  inherited;
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

destructor TvgScaledLayout.Destroy;
begin
  inherited;
end;

function TvgScaledLayout.GetChildrenMatrix: TvgMatrix;
begin
  Result := IdentityMatrix;
  Result.m11 := Width / FOriginalWidth;
  Result.m22 := Height / FOriginalHeight;
end;

procedure TvgScaledLayout.Paint;
var
  R: TvgRect;
begin
  if Assigned(Scene) and Scene.GetDesignTime and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := vgDashSolid;
  end;
  inherited ;
end;

procedure TvgScaledLayout.SetOriginalHeight(const Value: single);
begin
  if FOriginalHeight <> Value then
  begin
    FOriginalHeight := Value;
    if FOriginalHeight < 1 then
      FOriginalHeight := 1;
    RecalcAbsolute;
  end;
end;

procedure TvgScaledLayout.SetOriginalWidth(const Value: single);
begin
  if FOriginalWidth <> Value then
  begin
    FOriginalWidth := Value;
    if FOriginalWidth < 1 then
      FOriginalWidth := 1;
    RecalcAbsolute;
  end;
end;

procedure TvgScaledLayout.SetHeight(const Value: single);
begin
  inherited;
  RecalcAbsolute;
end;

procedure TvgScaledLayout.SetWidth(const Value: single);
begin
  inherited;
  RecalcAbsolute;
end;

initialization
  RegisterVGObjects('Layout', [TvgLayout, TvgScaledLayout, TvgScrollBox, TvgGrid]);
end.


