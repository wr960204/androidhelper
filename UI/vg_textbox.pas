unit vg_textbox;

{$I vg_define.inc}
{$I AndroidLanguage.inc}

interface

uses
  {$IFDEF FPC}
  LCLType, LMessages, LResources,
  {$ENDIF}
  {$IFDEF WIN32} windows, {$ENDIF}
  Menus, Classes, SysUtils, Controls, Forms, vg_utils, vg_classes, vg_scene, vg_objects, vg_controls,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF}, StringConsts;

type

  TvgTextBox = class(TvgTextControl)
  private
    FOnChange: TNotifyEvent;
    FReadOnly: boolean;
    FSelStart: integer;
    FSelLength: integer;
    FCaretPosition: integer;
    FMaxLength: integer;
    FFirstVisibleChar: integer;
    FLMouseSelecting: boolean;
    FNeedChange: boolean;
    FDisableCaret: boolean;
    FPassword: boolean;
    FPopupMenu: TPopupMenu;
    FOnTyping: TNotifyEvent;
    procedure InsertText(const AText: WideString);
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: WideString;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelRect: TvgRect;
    procedure SetCaretPosition(const Value: integer);
    function GetCoordinatePosition(x: single): integer;
    procedure SetMaxLength(const Value: Integer);
    function GetNextWordBeging(StartPosition: integer): integer;
    function GetPrivWordBeging(StartPosition: integer): integer;
    procedure UpdateFirstVisibleChar;
    procedure UpdateCaretePosition;
    procedure SetPassword(const Value: boolean);
    procedure CreatePopupMenu;
    procedure DoCopy(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure UpdatePopupMenuItems;
    procedure DoSelectAll(Sender: TObject);
  protected
    procedure ApplyStyle; override;
    procedure Change; virtual;
    function GetPasswordCharWidth: single;
    function TextWidth(const Str: WideString): single;
    procedure SetText(const Value: WideString); override;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ContextMenu(const ScreenPosition: TvgPoint); override;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    function GetCharX(a: integer): single;
    function ContentRect: TvgRect;
    property CaretPosition: integer read FCaretPosition write SetCaretPosition;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
  published
    property Cursor default crIBeam;
    property Font;
    property FontFill;
    property Password: boolean read FPassword write SetPassword;
    property Text;
    property TextAlign default vgTextAlignNear;
    property Resource;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTyping: TNotifyEvent read FOnTyping write FOnTyping;
  end;

  TvgValueType = (
    vgValueInteger,
    vgValueFloat
  );

  TvgNumberBox = class(TvgTextBox)
  private
    FValue: single;
    FMin: single;
    FMax: single;
    FPressed: boolean;
    FPressedPos: TvgPoint;
    FPressedVert: boolean;
    FPressedInc: single;
    FValueType: TvgValueType;
    FHorzIncrement: single;
    FVertIncrement: single;
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const AValue: single);
    procedure SetValueType(const Value: TvgValueType);
  protected
    procedure Change; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure PaintChildren; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Value: single read FValue write SetValue;
    property ValueType: TvgValueType read FValueType write SetValueType;
    property HorzIncrement: single read FHorzIncrement write FHorzIncrement;
    property VertIncrement: single read FVertIncrement write FVertIncrement;
  end;

  TvgHudTextBox = class(TvgTextBox)
  private
  protected
  public
  published
  end;

  TvgHudNumberBox = class(TvgNumberBox)
  private
  protected
  public
  published
  end;

implementation {===============================================================}

uses Clipbrd, vg_ani;

{ TvgTextBox ==================================================================}

constructor TvgTextBox.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := true;
  Cursor := crIBeam;
  TextAlign := vgTextAlignNear;
  AutoCapture := true;
  Width := 100;
  Height := 21;
  FCaretPosition := 0;
  FSelStart := 0;
  FSelLength := 0;
  FFirstVisibleChar := 1;
  CreatePopupMenu;
end;

destructor TvgTextBox.Destroy;
begin
  FPopupMenu.Free;
  inherited;
end;

procedure TvgTextBox.CreatePopupMenu;
var
  TmpItem: TMenuItem;
begin
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.AutoHotkeys := maManual;
  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_CUT;
    OnClick := DoCut;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_COPY;
    OnClick := DoCopy;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_PASTE;
    OnClick := DoPaste;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_DELETE;
    OnClick := DoDelete;
  end;
  FPopupMenu.Items.Add(TmpItem);

//  FPopupMenu.Items.NewBottomLine;

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_SELALL;
    OnClick := DoSelectAll;
  end;
  FPopupMenu.Items.Add(TmpItem);
end;

procedure TvgTextBox.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TvgTextBox.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TvgTextBox.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TvgTextBox.DoDelete(Sender: TObject);
begin
  ClearSelection;
end;

procedure TvgTextBox.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TvgTextBox.UpdatePopupMenuItems;
var
  SelTextEmpty: boolean;
begin
  SelTextEmpty := SelText <> EmptyStr;
  FPopupMenu.Items.Find(STR_MN_CUT).Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_COPY).Enabled := SelTextEmpty;
  FPopupMenu.Items.Find(STR_MN_PASTE).Enabled := (Clipbrd.Clipboard.AsText <> EmptyStr) and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_DELETE).Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_SELALL).Enabled := SelText <> Text;
end;

procedure TvgTextBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource(VG_CONTENT);
  if (T <> nil) and (T is TvgContent) then
  begin
    TvgContent(T).OnPaint := DoContentPaint;
  end
end;

function TvgTextBox.ContentRect: TvgRect;
var
  T: TvgObject;
begin
  T := FindResource(VG_CONTENT);
  if (T <> nil) and (T.IsVisual) then
  begin
    Result := TvgVisualObject(T).BoundsRect;
  end
  else
  begin
    Result := LocalRect;
  end;
end;

procedure TvgTextBox.Paint;
begin
  inherited;
end;

procedure TvgTextBox.DoContentPaint(Sender: TObject; const Canvas: TvgCanvas;
  const ARect: TvgRect);
var
  i: integer;
  R: TvgRect;
begin
  { draw text }
  Canvas.Font.Assign(Font);
  Canvas.Fill.Assign(FontFill);
  if FPassword then
  begin
    R := ARect;
    R.Right := R.Left + TextWidth(VG_CHAR_A) - 1;
    R.Top := (vgRectHeight(ARect) - vgRectWidth(R)) / 2;
    R.Bottom := R.Top + vgRectWidth(R);
    for i := 1 to Length(Text) do
    begin
      Canvas.FillEllipse(R, AbsoluteOpacity);
      vgOffsetRect(R, vgRectWidth(R) + 1, 0);
    end;
  end
  else
  begin
    Canvas.FillText(ARect, ARect, Copy(Text, FFirstVisibleChar, Length(Text) - FFirstVisibleChar + 1),
      false, AbsoluteOpacity, TextAlign, vgTextAlignCenter);
  end;
  { carret }
  if not IsFocused then Exit;
  { selection }
  if SelLength > 0 then
  begin
    Canvas.Fill.Color := VG_COLOR_TEXTBOX_FILL;
    Canvas.FillRect(GetSelRect, 2, 2, AllCorners, AbsoluteOpacity);
  end;
  { carret }
  if FDisableCaret then Exit;
  if SelLength > 0 then Exit;
  Canvas.Fill.Assign(FontFill);
  Canvas.Fill.Style := vgBrushSolid;
  R := ARect;
  R.Left := GetCharX(FCaretPosition);
  R.Right := R.Left + 2;
  vgInflateRect(R, 0, -1);
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
end;

procedure TvgTextBox.InsertText(const AText: WideString);
var
  TmpS: WideString;
begin
  if ReadOnly then Exit;

  TmpS := Text;
//  FActionStack.FragmentDeleted(SelStart + 1, Copy(TmpS, SelStart+1, SelLength));
  Delete(TmpS, SelStart + 1, SelLength);
//  FActionStack.FragmentInserted(SelStart + 1, Length(AText), SelLength <> 0);
  Insert(AText, TmpS, SelStart + 1);
  if (MaxLength <= 0) or (Length(TmpS) <= MaxLength) then
  begin
    Text := TmpS;
    CaretPosition := SelStart + Length(AText);
  end;
  SelLength := 0;
end;

procedure TvgTextBox.UpdateFirstVisibleChar;
var
  LEditRect: TvgRect;
begin
  if FFirstVisibleChar >= (FCaretPosition + 1) then
  begin
    FFirstVisibleChar := FCaretPosition;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin
    LEditRect := ContentRect;

    begin
      while (TextWidth(Copy(Text, FFirstVisibleChar, FCaretPosition - FFirstVisibleChar + 1)) > LEditRect.Right - LEditRect.Left) and (FFirstVisibleChar < Length(Text)) do
      begin
        if TextWidth(Copy(Text, FFirstVisibleChar + 500, (FCaretPosition - FFirstVisibleChar + 500) + 1)) > LEditRect.Right - LEditRect.Left then
          Inc(FFirstVisibleChar, 500)
        else
          if TextWidth(Copy(Text, FFirstVisibleChar + 100, (FCaretPosition - FFirstVisibleChar + 100) + 1)) > LEditRect.Right - LEditRect.Left then
            Inc(FFirstVisibleChar, 100)
          else
            if TextWidth(Copy(Text, FFirstVisibleChar + 50, (FCaretPosition - FFirstVisibleChar + 100) + 1)) > LEditRect.Right - LEditRect.Left then
              Inc(FFirstVisibleChar, 50)
            else
              if TextWidth(Copy(Text, FFirstVisibleChar + 10, (FCaretPosition - FFirstVisibleChar + 10) + 1)) > LEditRect.Right - LEditRect.Left then
                Inc(FFirstVisibleChar, 10)
              else
                Inc(FFirstVisibleChar);
      end;
    end;
  end;
  Repaint;
end;

procedure TvgTextBox.UpdateCaretePosition;
begin
  SetCaretPosition(CaretPosition);
end;

function TvgTextBox.GetPasswordCharWidth: single;
begin
  Result := TextWidth(VG_CHAR_A);
end;

function TvgTextBox.TextWidth(const Str: WideString): single;
var
  R: TvgRect;
begin
  R := ContentRect;
  R.Right := 10000;
  if FPassword then
    Canvas.MeasureText(R, R, VG_CHAR_A, false, TextAlign, vgTextAlignCenter)
  else
    Canvas.MeasureText(R, R, Str, false, TextAlign, vgTextAlignCenter);
  Result := vgRectWidth(R);
end;

function TvgTextBox.GetCoordinatePosition(x: single): integer;
var
  CurX: double;
  TmpX,
  WholeTextWidth,
  EditRectWidth : single;
  T: TvgObject;
  Str, StrA: WideString;
begin
  Result := FFirstVisibleChar - 1;
  if Length(Text) = 0 then
    Exit;
  T := FindResource(VG_CONTENT);
  if (T <> nil) and (T.IsVisual) then
    x := x - TvgVisualObject(T).Position.X;

  if FPassword then
    WholeTextWidth := Length(Text) * GetPasswordCharWidth
  else
    WholeTextWidth := TextWidth(Copy(Text, 1, Length(Text)));

  EditRectWidth := ContentRect.Right - ContentRect.Left;
  TmpX := x;
  if WholeTextWidth < EditRectWidth then
    case TextAlign of
      vgTextAlignFar: TmpX := x - (EditRectWidth-WholeTextWidth);
      vgTextAlignCenter: TmpX := x - ((EditRectWidth-WholeTextWidth) / 2);
    end;

  if FPassword then
  begin
    Result := Result + Trunc((TmpX - ContentRect.Left) / GetPasswordCharWidth);
    if Result < 0 then
      Result := 0
    else
      if Result > Length(Text) then
        Result := Length(Text);
  end
  else
  begin
    StrA := System.Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 1);
    Str := System.Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 2);
    while (TextWidth(StrA) < TmpX) and (Result < Length(Text)) do
    begin
      if (TmpX > TextWidth(StrA) + ((TextWidth(Str) - TextWidth(StrA)) / 2)) and (TmpX < TextWidth(Str)) then
      begin
        Result := Result + 1;
        Break;
      end;
      if TmpX < TextWidth(Str) then Break;
      Result := Result + 1;
      StrA := Str;
      Str := Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 2);
    end;
  end;
end;

function TvgTextBox.GetCharX(a: integer): single;
var
  WholeTextWidth: single;
  EditRectWidth: single;
  R: TvgRect;
begin
  Result := ContentRect.Left;

  if FPassword then
    WholeTextWidth := Length(Text) * GetPasswordCharWidth
  else
    R := ContentRect;
  Canvas.MeasureText(R, R, Text, false, TextAlign, vgTextAlignCenter);
  WholeTextWidth := vgRectWidth(R);

  if a > 0 then
  begin
    if FPassword then
    begin
      if a <= Length(Text) then
        Result := Result + (a - FFirstVisibleChar + 1) * GetPasswordCharWidth
      else
        Result := Result + (Length(Text) - FFirstVisibleChar + 1) * GetPasswordCharWidth;
    end
    else
    begin
      if a <= Length(Text) then
      begin
        R := ContentRect;
        Canvas.MeasureText(R, R, Copy(Text, FFirstVisibleChar, a - FFirstVisibleChar + 1), false, TextAlign, vgTextAlignCenter);
        Result := Result + vgRectWidth(R);
      end
      else
      begin
{        R := ContentRect;
        Canvas.MeasureText(R, R, Copy(Text, FFirstVisibleChar, Length(Text) - FFirstVisibleChar + 1), false, TextAlign, vgTextAlignCenter);
        Result := Result + vgRectWidth(R);}
      end;
    end;
  end;

  EditRectWidth := ContentRect.Right - ContentRect.Left;
  if WholeTextWidth < EditRectWidth then
    case TextAlign of
      vgTextAlignFar : Result := Result + (EditRectWidth - WholeTextWidth);
      vgTextAlignCenter: Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
    end;
end;

function TvgTextBox.GetNextWordBeging(StartPosition: integer): integer;
var
  SpaceFound,
    WordFound: boolean;
begin
  Result := StartPosition;
  SpaceFound := false;
  WordFound := false;
  while (Result + 2 <= Length(Text)) and
    ((not ((Text[Result + 1] <> Space) and SpaceFound))
    or not WordFound) do
  begin
    if Text[Result + 1] = Space then
      SpaceFound := true;
    if Text[Result + 1] <> Space then begin
      WordFound := true;
      SpaceFound := false;
    end;

    Result := Result + 1;
  end;
  if not SpaceFound then
    Result := Result + 1;
end;

function TvgTextBox.GetPrivWordBeging(StartPosition: integer): integer;
var
  WordFound: boolean;
begin
  Result := StartPosition;
  WordFound := false;
  while (Result > 0) and
    ((Text[Result] <> Space) or not WordFound) do
  begin
    if Text[Result] <> Space then
      WordFound := true;
    Result := Result - 1;
  end;
end;

function TvgTextBox.GetSelStart: integer;
begin
  if FSelLength > 0 then
    Result := FSelStart
  else
    if FSelLength < 0 then
      Result := FSelStart + FSelLength
    else
      Result := CaretPosition;
end;

function TvgTextBox.GetSelRect: TvgRect;
begin
  Result := ContentRect;
  Result.Left := GetCharX(SelStart);
  Result.Right := GetCharX(SelStart + SelLength) + 1;
end;

function TvgTextBox.GetSelLength: integer;
begin
  Result := Abs(FSelLength);
end;

function TvgTextBox.GetSelText: WideString;
begin
  Result := Copy(Text, SelStart + 1, SelLength);
end;

procedure TvgTextBox.SetSelLength(const Value: integer);
begin
  if FSelLength <> Value then
  begin
    FSelLength := Value;
    Repaint;
  end;
end;

procedure TvgTextBox.SetSelStart(const Value: integer);
begin
  if FSelStart <> Value then
  begin
    SelLength := 0;
    FSelStart := Value;
    CaretPosition := FSelStart;
    Repaint;
  end;
end;

procedure TvgTextBox.SetCaretPosition(const Value: integer);
begin
  if Value < 0 then
    FCaretPosition := 0
  else
    if Value > Length(Text) then
      FCaretPosition := Length(Text)
    else
      FCaretPosition := Value;

  UpdateFirstVisibleChar;

  if SelLength <= 0 then
    FSelStart := Value;

  Repaint;
{  if Focused then
    SetCaretPos(GetCharX(FCaretPosition), ContentRect.Top);}
end;

procedure TvgTextBox.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
  end;
end;

procedure TvgTextBox.CopyToClipboard;
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Cardinal;
  S: WideString;
begin
  {$IFNDEF FPC}
//  if PasswordKind = pkNone then
    if Length(SelText) > 0 then
    begin
      S := SelText;
        begin
          Size := Length(S);
          Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * Size + 2);
          try
            DataPtr := GlobalLock(Data);
            try
              Move(PWideChar(S)^, DataPtr^, 2 * Size + 2);
              Clipbrd.Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
            finally
              GlobalUnlock(Data);
            end;
          except
            GlobalFree(Data);
            raise;
          end;
        end;
    end;
  {$ELSE}
  if SelText <> EmptyStr then
    Clipbrd.Clipboard.AsText := UTF8Encode(SelText);
  {$ENDIF}
end;

procedure TvgTextBox.PasteFromClipboard;
var
  Data: THandle;
  Insertion: WideString;
begin
  if ReadOnly then Exit;
  {$IFNDEF FPC}
  if Clipbrd.Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    Data := Clipbrd.Clipboard.GetAsHandle(CF_UNICODETEXT);
    try
      if Data <> 0 then
        Insertion := PWideChar(GlobalLock(Data));
    finally
      if Data <> 0 then GlobalUnlock(Data);
    end;
  end
  else
    Insertion := Clipbrd.Clipboard.AsText;

  InsertText(Insertion);
  {$ELSE}
  InsertText(UTF8Decode(Clipbrd.Clipboard.AsText));
  {$ENDIF}
end;

procedure TvgTextBox.ClearSelection;
var
  TmpS: WideString;
begin
  if ReadOnly then Exit;

  TmpS := Text;
//  FActionStack.FragmentDeleted(SelStart+1, Copy(TmpS,SelStart+1,SelLength));
  Delete(TmpS, SelStart + 1, SelLength);
  CaretPosition := SelStart;
  Text := TmpS;
  SelLength := 0;
end;

procedure TvgTextBox.CutToClipboard;
begin
//  if PasswordKind = pkNone then
    CopyToClipboard;
  ClearSelection;
end;

procedure TvgTextBox.SelectAll;
begin
  SetCaretPosition(Length(Text));
  SelStart := 0;
  SelLength := Length(Text);
  Repaint;
end;

procedure TvgTextBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  S: wideString;
  TmpS: WideString;
  OldCaretPosition: integer;
begin
  inherited ;
  OldCaretPosition := CaretPosition;
  case Key of
    VK_RETURN: Change;
    VK_END: CaretPosition := Length(Text);
    VK_HOME: CaretPosition := 0;
    VK_LEFT:
      if ssCtrl in Shift then
        CaretPosition := GetPrivWordBeging(CaretPosition)
      else
        CaretPosition := CaretPosition - 1;
    VK_RIGHT:
      if ssCtrl in Shift then
        CaretPosition := GetNextWordBeging(CaretPosition)
      else
        CaretPosition := CaretPosition + 1;
    VK_DELETE, 8: {Delete or BackSpace key was pressed}
      if not ReadOnly then
      begin
        if SelLength <> 0 then
        begin
          if Shift = [ssShift] then
            CutToClipboard
          else
            ClearSelection;
        end
        else
        begin
          TmpS := Text;
          if TmpS <> EmptyStr then
            if Key = VK_DELETE then
            begin
              Delete(TmpS, CaretPosition + 1, 1);
            end
            else
            begin {BackSpace key was pressed}
              Delete(TmpS, CaretPosition, 1);
              CaretPosition := CaretPosition - 1;
            end;
          Text := TmpS;
          if Assigned(FOnTyping) then FOnTyping(Self);
        end;
      end;
    VK_INSERT:
      if Shift = [ssCtrl] then
      begin
        CopyToClipboard;
      end
      else
        if Shift = [ssShift] then
        begin
          PasteFromClipboard;
          if Assigned(FOnTyping) then FOnTyping(Self);
        end;
  end;

  case KeyChar of
    VG_CHAR_C,VG_CHAR_C_U:
      if Shift = [ssCtrl] then
      begin
        CopyToClipboard;
        KeyChar := CH_ZERO;
      end;
    VG_CHAR_V, VG_CHAR_V_U:
      if Shift = [ssCtrl] then
      begin
        PasteFromClipboard;
        if Assigned(FOnTyping) then FOnTyping(Self);
        KeyChar := CH_ZERO;
      end;
    VG_CHAR_X, VG_CHAR_X_U:
      if Shift = [ssCtrl] then
      begin
        CutToClipboard;
        KeyChar := CH_ZERO;
      end;
    VG_CHAR_Z, VG_CHAR_Z_U:
      if Shift = [ssCtrl] then
      begin
        {UnDo};
        KeyChar := CH_ZERO;
      end;
  end;

  if Key in [VK_END, VK_HOME, VK_LEFT, VK_RIGHT] then
  begin
    if ssShift in Shift then
    begin
      if SelLength = 0 then
        FSelStart := OldCaretPosition;
      FSelStart := CaretPosition;
      FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
    end
    else
      FSelLength := 0;
    Repaint;
  end;

  if (Ord(KeyChar) >= 32) and not ReadOnly then
  begin
    S := KeyChar;
    InsertText(S);
    if Assigned(FOnTyping) then FOnTyping(Self);
  end;

  UpdateCaretePosition;
end;

procedure TvgTextBox.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgTextBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if Button = mbLeft then
    FLMouseSelecting := true;

  if Button = mbLeft then
  begin
    CaretPosition := GetCoordinatePosition(x);
    SelLength := 0;
  end;
end;

procedure TvgTextBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  OldCaretPosition: integer;
  TmpNewPosition : integer;
begin
  inherited;
  if FLMouseSelecting then
  begin
    TmpNewPosition := GetCoordinatePosition(x);
    OldCaretPosition := CaretPosition;
    if (x > ContentRect.Right) then
      CaretPosition := TmpNewPosition +1
    else
      CaretPosition := TmpNewPosition;
    if SelLength = 0 then
      FSelStart := OldCaretPosition;
    FSelStart := CaretPosition;
    FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
  end;
end;

procedure TvgTextBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FLMouseSelecting := false;
end;

procedure TvgTextBox.Change;
begin
  if FNeedChange and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TvgTextBox.ContextMenu(const ScreenPosition: TvgPoint);
begin
  inherited;
  if csDesigning in ComponentState then Exit;

  UpdatePopupMenuItems;
  FPopupMenu.PopupComponent := Self;
  FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
end;

procedure TvgTextBox.KillFocus;
begin
  inherited ;
  Change;
end;

procedure TvgTextBox.EnterFocus;
begin
  inherited;
  SelectAll;
  FNeedChange := false;
end;

procedure TvgTextBox.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    inherited;
    FNeedChange := true;
  end;
end;

procedure TvgTextBox.SetPassword(const Value: boolean);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    Repaint;
  end;
end;

{ TvgNumberBox }

constructor TvgNumberBox.Create(AOwner: TComponent);
begin
  inherited;
  Max := 10;
  VertIncrement := 5;
  HorzIncrement := 1;
  Text := VG_CHAR_0;
  Value := 0;
  AutoCapture := true;
end;

destructor TvgNumberBox.Destroy;
begin
  inherited;
end;

function TvgNumberBox.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgNumberBox.SetData(const Value: Variant);
begin
  Self.Value := Value;
end;

procedure TvgNumberBox.Change;
begin
  try
    FValue := StrToFloat(Text);
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  except
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  end;
  Repaint;
  inherited;
end;

procedure TvgNumberBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressed := true;
    FPressedPos := vgPoint(X, Y);
    FPressedVert := false;
    FPressedInc := 0;
  end;
end;

procedure TvgNumberBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if FPressed then
  begin
    if Abs(X - FPressedPos.X) >= Abs(Y - FPressedPos.Y) then
    begin
      { horz }
      if X > FPressedPos.X then
        Value := Value + HorzIncrement
      else
        Value := Value - HorzIncrement;
      FPressedInc := X - FPressedPos.X;
      FPressedVert := false;
    end
    else
    begin
      { vert }
      if Y < FPressedPos.Y then
        Value := Value + VertIncrement
      else
        Value := Value - VertIncrement;
      FPressedInc := X - FPressedPos.X;
      FPressedVert := true;
    end;
    FNeedChange := true;
    FPressedPos := vgPoint(X, Y);
  end;
end;

procedure TvgNumberBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressed then
  begin
    FPressed := false;
    Change;
    Repaint;
  end;
end;

procedure TvgNumberBox.Paint;
begin
  inherited ;
end;

procedure TvgNumberBox.PaintChildren;
var
  R: TvgRect;
begin
  if FPressed then
    FDisableCaret := true;
  inherited;
  if FPressed then
  begin
    Canvas.SetMatrix(AbsoluteMatrix);
    Canvas.Fill.Style := vgBrushSolid;
    Canvas.Fill.SolidColor := $AA505050;
    R := LocalRect;
    if FPressedVert then
    begin
      vgInflateRect(R, -1, -1);
      R.Left := R.Right - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity);
      vgInflateRect(R, -1, -1);
    end
    else
    begin
      vgInflateRect(R, -1, -1);
      R.Top := R.Bottom - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity);
      vgInflateRect(R, -1, -1);
    end;
  end;
  if FPressed then
    FDisableCaret := false;
end;

procedure TvgNumberBox.SetMax(const Value: single);
begin
  FMax := Value;
end;

procedure TvgNumberBox.SetMin(const Value: single);
begin
  FMin := Value;
end;

procedure TvgNumberBox.SetValue(const AValue: single);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
    SelLength := 0;
    Repaint;
  end;
end;

procedure TvgNumberBox.SetValueType(const Value: TvgValueType);
begin
  FValueType := Value;
end;

initialization
  RegisterVGObjects(VG_I_CONTROLS, [TvgTextBox, TvgNumberBox]);
  RegisterVGObjects(VG_I_HUD, [TvgHudTextBox, TvgHudNumberBox]);
end.


