unit vg_memo;

interface

{$I AndroidLanguage.inc}

uses                      
  {$IFDEF FPC}
  LCLProc, LCLType,
  {$ENDIF}
  {$IFDEF WIN32}Windows, {$ENDIF}Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  menus, Contnrs, StdCtrls, vg_scene, vg_controls, vg_utils, vg_classes,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF}, StringConsts;

type
  TPopupMenuClass = class of TPopupMenu;

  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);
  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  TInsertOption = (ioSelected, ioMoveCaret, ioCanUndo, ioUnDoPairedWithPriv);
  TInsertOptions = set of TInsertOption;
  TDeleteOption = (doMoveCaret, doCanUndo);
  TDeleteOptions = set of TDeleteOption;

  TActionType = (atDelete, atInsert);

  TLinesBegs = array of integer;
  PLinesBegs = ^TLinesBegs;

  TCaretPosition = record
    Line, Pos: integer;
  end;

  PEdtAction = ^TEdtAction;

  TEdtAction = record
    ActionType : TActionType;

    PairedWithPriv : boolean;
    StartPosition : integer;
    DeletedFragment : WideString; {For atDelete}
    Length : integer; {For atInsert}
  end;

  TvgMemo = class;

  TEdtActionStack = class (TStack)
  private
    FOwner : TvgMemo;
  public
    constructor Create(AOwner : TvgMemo);
    destructor Destroy; override;

    procedure FragmentInserted(StartPos, FragmentLength : integer; IsPairedWithPriv : boolean);
    procedure FragmentDeleted(StartPos : integer; Fragment : WideString);
    procedure CaretMovedBy(Shift : integer);

    function RollBackAction : boolean;
  end;

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth);

  TSelArea = array of TvgRect;

  TvgMemo = class(TvgTextControl)
  private
    FUpdating : boolean;
    FInternalMustUpdateLines : boolean;

    FLMouseSelecting: boolean;
    FOldMPt : TvgPoint;

    FCaretPosition: TCaretPosition;
    FFirstVisibleChar: integer;

    FPopupMenu: TPopupMenu;

    FAutoSelect: boolean;
    FCharCase: TEditCharCase;
    FHideSelection: Boolean;
    FMaxLength: Integer;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FPasswordChar: char;
    FTextAlignment: TAlignment;

    FActionStack : TEdtActionStack;
    FScrollBars: TScrollStyle;

    FLines: TvgWideStrings;
    FWordWrap: boolean;

    FLinesBegs : array of integer;

    FSelStart: TCaretPosition;
    FSelEnd: TCaretPosition;
    FSelected : boolean;

    FOldSelStartPos, FOldSelEndPos, FOldCaretPos : integer;

    FIntCharsWidths : array [System.WideChar] of single;
    FCaretShown : boolean;

    function GetSelBeg : TCaretPosition;
    function GetSelEnd : TCaretPosition;
    function CharsWidths(Char: System.WideChar): single;

    procedure StorePositions;
    procedure RestorePositions;

    procedure SelectAtPos(APos : TCaretPosition);

    procedure VScrlBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure VScrlBarChange(Sender: TObject);

    procedure HScrlBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure HScrlBarChange(Sender: TObject);

    procedure SetText(const Value: WideString);
    procedure SetFont(Value: TFont);

    procedure SetCaretPosition(const Value: TCaretPosition);
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelStart: integer;
    function GetSelLength: integer;

    procedure UpdateHScrlBarByCaretPos;
    procedure UpdateVScrlBarByCaretPos;

    function GetSelText: WideString;

    procedure SetAutoSelect(const Value: boolean);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);

    procedure SelectAtMousePoint;

    procedure Change; dynamic;
    procedure SetPasswordChar(const Value: char);

    function GetNextWordBeging(StartPosition: TCaretPosition): TCaretPosition;
    function GetPrivWordBeging(StartPosition: TCaretPosition): TCaretPosition;

    function GetPositionShift(APos : TCaretPosition; Delta: integer {char count}):TCaretPosition;
    procedure MoveCareteBy(Delta : integer);
    procedure MoveCaretLeft;
    procedure MoveCaretRight;

    procedure MoveCaretVertical(LineDelta : integer);
    procedure MoveCaretDown;
    procedure MoveCaretUp;
    procedure MoveCaretPageUp;
    procedure MoveCaretPageDown;

    procedure UpdateCaretPosition(UpdateScrllBars : boolean);
    procedure UpdateCarete;
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetScrollBars(const Value: TScrollStyle);

    procedure SetLines(const Value: TvgWideStrings);
    procedure GetLineBounds(LineIndex : integer; var LineBeg, LineLength : integer);

    function GetLineCount : integer;
    function GetLine(Index : integer) : WideString; //Returns Line without special symbols at the end.
    function GetLineInternal(Index : integer) : WideString; //Returns Line with special symbols at the end.
    procedure InsertLine(Index: Integer; const S: WideString);
    procedure DeleteLine(Index: Integer);
    procedure ClearLines;

    procedure SetWordWrap(const Value: boolean);

    function GetPageSize : single;

    function GetRealWordWrap : boolean;
    function GetTextWidth(LineNum: Integer): single;
    function GetWidestLine : integer;
    function FillLocalLinesBegs(PText: PWideString; ABegChar, AEndChar: integer;
      TmpLinesBegs: PLinesBegs) : integer;

    procedure UpdateRngLinesBegs(PText : PWideString;AUpdBegLine, AUpdEndLine,
      AUpdBegChar, AUpdEndChar, ACharDelta, AOldWideslLineWidth : integer);
    function GetShowSelection : boolean;
    function GetLineRealEnd(AStartPos: TCaretPosition;
      PText: PWideString): TCaretPosition;
    procedure UpdateCharsWidths;
    function GetHScrollBar: TvgScrollBar;
    function GetVScrollBar: TvgScrollBar;
    function VScrollBarValue: single;
    function HScrollBarValue: single;
    procedure EnterFocus; override;
    procedure KillFocus; override;
  protected
    function GetPasswordCharWidth: single; virtual;
    function GetLineHeight : single;
    function GetPointPosition(Pt : TvgPoint): TCaretPosition;

    function GetSelArea: TSelArea; virtual;

    procedure Paint; override;
    procedure PaintText; virtual;
    procedure PaintSelectedText; virtual;
    procedure DrawPasswordChar(SymbolRect: TvgRect; Selected: boolean); virtual;

    function GetPopupMenuClass: TPopupMenuClass; virtual;
    procedure CreatePopupMenu; virtual;
    procedure UpdatePopupMenuItems; virtual;

    procedure ApplyStyle; override;
    function ContentRect: TvgRect;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);

    function ValidText(NewText: WideString): boolean; virtual;

    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; //override;

    procedure UpdateScrlBarsParams;
    procedure UpdateScrlBars;

    procedure ContextMenu(const ScreenPosition: TvgPoint); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y, dx, dy: single); override;

    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure SelectWord;
    procedure FontChanged(Sender: TObject); override;

    procedure DoUndo(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoCopy(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);
  public
    FWidesLineIndex : integer;
    FTextWidth : array of integer;

    procedure UpdateLines;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowCaret; virtual;
    procedure HideCaret; virtual;

    procedure Realign; override;

    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure CutToClipboard;
    procedure ClearSelection;
    procedure SelectAll;

    procedure GoToTextEnd;
    procedure GoToTextBegin;
    procedure GotoLineEnd;
    procedure GoToLineBegin;

    function GetPositionPoint(ACaretPos : TCaretPosition): TvgPoint;

    procedure UnDo;

    procedure InsertAfter(Position: TCaretPosition; S: WideString; Options : TInsertOptions);
    procedure DeleteFrom(Position: TCaretPosition; ALength : integer; Options : TDeleteOptions);

    procedure SetUpdateState(Updating : boolean);

    function TextPosToPos(APos : integer) : TCaretPosition;
    function PosToTextPos(APostion : TCaretPosition) : integer;

    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText;
    property CaretPosition: TCaretPosition read FCaretPosition write SetCaretPosition;
    property TextWidth[LineNum: Integer]: single read GetTextWidth;
    property VScrollBar: TvgScrollBar read GetVScrollBar;
    property HScrollBar: TvgScrollBar read GetHScrollBar;
  published
    property Cursor default crIBeam;
    property TextAlignment : TAlignment read FTextAlignment write SetTextAlignment default taLeftJustify;
    property AutoSelect: boolean read FAutoSelect write SetAutoSelect default true;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property Enabled;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property Lines : TvgWideStrings read FLines write SetLines stored false;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    property Text write SetText;
    property Font;
    property FontFill;
    property Resource;
  end;

  TvgHudMemo = class(TvgMemo)
  private
  protected
  public
  published
  end;

function ComposeCaretPos(ALine, APos : integer) : TCaretPosition;

implementation

uses
  {$IFDEF KS_COMPILER6_UP}
  Types,
  {$ENDIF}
  ExtCtrls, Clipbrd;

type

  TvgMemoLines = class(TvgWideStrings)
  private
    FMemo: TvgMemo;
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

{ TvgMemoLines }

procedure TvgMemoLines.Clear;
begin
  FMemo.ClearLines;
end;

procedure TvgMemoLines.Delete(Index: Integer);
begin
  FMemo.DeleteLine(Index);
end;

procedure TvgMemoLines.Insert(Index: Integer; const S: WideString);
begin
  FMemo.InsertLine(Index, S);
end;

function TvgMemoLines.Get(Index: Integer): WideString;
begin
  Result := FMemo.GetLine(Index);
end;

function TvgMemoLines.GetCount: Integer;
begin
  Result := FMemo.GetLineCount;
end;

procedure TvgMemoLines.SetUpdateState(Updating: Boolean);
begin
  inherited;
//  FMemo.SetUpdateState(UpdateCount > 0);
end;

function ComposeCaretPos(ALine, APos : integer) : TCaretPosition;
begin
  with Result do
  begin
    Line := ALine;
    Pos := APos;
  end;
end;

{ TvgMemo }

constructor TvgMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TvgMemoLines.Create;
  (FLines as TvgMemoLines).FMemo := Self;
  CanFocused := true;
  Cursor := crIBeam;
  FInternalMustUpdateLines := true;

  CreatePopupMenu;

  FCaretShown := false;

  FActionStack := TEdtActionStack.Create(Self);

  FTextAlignment := taLeftJustify;
  FAutoSelect := true;
  FCharCase := ecNormal;
  FHideSelection := true;
  FMaxLength := 0;
  FReadOnly := false;
  FPasswordChar := CH_ZERO;
  FScrollBars := ssNone;

  FLMouseSelecting := false;
  FOldMPt := vgPoint(0,0);

  FUpdating := false;


  with FCaretPosition do begin
    Line := 0;
    Pos := 0;
  end;

  FSelStart := ComposeCaretPos(0,0);
  FSelEnd := ComposeCaretPos(0,0);
  FSelected := false;

  FOldSelStartPos := -1;
  FOldSelEndPos := -1;
  FOldCaretPos := -1;

  AutoCapture := true;

  FWidesLineIndex := 0;
  SetLength(FTextWidth,0);

  Width := 100;
{  UpdateCharsWidths;
  UpdateLines;
  UpdateScrlBarsParams;}
end;

procedure TvgMemo.EnterFocus;
begin
  inherited;
  UpdateCarete;
  with FCaretPosition do
  begin
    Line := 0;
    Pos := 0;
  end;

  if AutoSelect then
    SelectAll;
end;

procedure TvgMemo.Killfocus;
begin
  inherited;
  Repaint;
end;

function TvgMemo.GetPositionPoint(ACaretPos : TCaretPosition): TvgPoint;
var
  WholeTextWidth : single;
  EdiTvgRectWidth : single;
  LineText : WideString;
begin
  Result.X := ContentRect.Left;
  Result.Y := ContentRect.Top + GetLineHeight * (ACaretPos.Line);
  Result.Y := ContentRect.Top + GetLineHeight * (ACaretPos.Line - VScrollBarValue);
  WholeTextWidth := 0;
  if Canvas = nil then Exit;

  if (ACaretPos.Line < Lines.Count) and (Lines.Count > 0) then
  begin
    LineText := Lines[ACaretPos.Line];

    Canvas.Font.Assign(Font);
    if PasswordChar <> CH_ZERO then
      WholeTextWidth := Length(LineText) * GetPasswordCharWidth
    else
      WholeTextWidth := Canvas.TextWidth(LineText);

    if ACaretPos.Pos > 0 then
    begin
      if PasswordChar <> CH_ZERO then
      begin
        if ACaretPos.Pos <= Length(LineText) then
          Result.X := Result.X + (ACaretPos.Pos) * GetPasswordCharWidth
        else
          Result.X := Result.X + (Length(LineText)) * GetPasswordCharWidth;
      end
      else
      begin
        if ACaretPos.Pos <= Length(LineText) then
          Result.X := Result.X + Canvas.TextWidth(Copy(LineText, 1, ACaretPos.Pos))
        else
          Result.X := Result.X + Canvas.TextWidth(LineText);
      end;
    end;
  end;
  EdiTvgRectWidth := ContentRect.Right - ContentRect.Left;
  if WholeTextWidth < EdiTvgRectWidth then
    case TextAlignment of
      taRightJustify : Result.X := Result.X + (EdiTvgRectWidth-WholeTextWidth);
      taCenter : Result.X := Result.X + ((EdiTvgRectWidth-WholeTextWidth) / 2);
    end;

  Result.X := Result.X - HScrollBarValue;
end;

function TvgMemo.GetPointPosition(Pt : TvgPoint): TCaretPosition;
var
  CurX: double;
  TmpX,
  WholeTextWidth,
  EdiTvgRectWidth : single;
  LineText : WideString;
  LLine : integer;
  LPos : integer;
  TmpPt : TvgPoint;
  LEdiTvgRect : TvgRect;
begin
  with Result do
  begin
    Line := 0;
    Pos := 0;
  end;

  if Lines.Count <= 0 then
    Exit;

  LEdiTvgRect := ContentRect;

  with LEdiTvgRect, Pt do begin
    if x < Left then
      TmpPt.x := Left
    else
      if x > Right then
        TmpPt.x := Right
      else
        TmpPt.x := x;

    if y < Top then
      TmpPt.y := Top
    else
      if y > Bottom then
        TmpPt.y := Bottom
      else
        TmpPt.y := y;
  end;

  LLine := trunc((TmpPt.Y-ContentRect.Top) / GetLineHeight + VScrollBarValue);

  LPos := 0;

  if LLine > Lines.Count-1 then
    LLine := Lines.Count-1;

  LineText := Lines[LLine];

  if Length(LineText) > 0 then begin
    Canvas.Font.Assign(Font);
    if PasswordChar <> CH_ZERO then
      WholeTextWidth := Length(LineText)*GetPasswordCharWidth
    else
      WholeTextWidth := Canvas.TextWidth(LineText);

    EdiTvgRectWidth := ContentRect.Right - ContentRect.Left;
    TmpX := TmpPt.x;
    if WholeTextWidth < EdiTvgRectWidth then
      case TextAlignment of
        taRightJustify : TmpX := TmpPt.x - (EdiTvgRectWidth-WholeTextWidth);
        taCenter : TmpX := TmpPt.x - ((EdiTvgRectWidth-WholeTextWidth) / 2);
      end;

    TmpX := TmpX + HScrollBarValue;

    if PasswordChar <> CH_ZERO then
    begin
      LPos := LPos + Trunc((TmpX - ContentRect.Left) / GetPasswordCharWidth);
      if LPos < 0 then
        LPos := 0
      else
        if LPos > Length(LineText) then
          LPos := Length(LineText);
    end
    else
    begin
      Canvas.Font.Assign(Font);
      CurX := ContentRect.Left + Canvas.TextWidth(LineText[1]) / 2;
      while (CurX < TmpX) and (LPos + 1 <= Length(LineText)) and (CurX < ContentRect.Right + HScrollBarValue) do
      begin
        CurX := CurX + Canvas.TextWidth(LineText[LPos + 1]) / 2;
        if LPos + 1 + 1 <= Length(LineText) then
          CurX := CurX + Canvas.TextWidth(LineText[LPos + 1 + 1]) / 2;
        Inc(LPos);
      end;
    end;
  end;
  with Result do begin
    Line := LLine;
    Pos := LPos;
  end;
end;

procedure TvgMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  TmpS: WideString;
  OldCaretPosition: TCaretPosition;
  WasSelection : boolean;
  LTmpOptions : TInsertOptions;
begin
  inherited KeyDown(Key, KeyChar, Shift);
  OldCaretPosition := CaretPosition;
  if (Key = VK_RETURN) then
  begin
    WasSelection := SelLength > 0;
    if WasSelection then
      DeleteFrom(GetSelBeg,SelLength,[doMoveCaret, doCanUndo]{true,true, false});
    if WasSelection then
      LTmpOptions := [ioUnDoPairedWithPriv]
    else
      LTmpOptions := [];
    TmpS := CHAR_CRLF;
    InsertAfter(CaretPosition, TmpS, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    SelLength := 0;
    Key := 0;
  end;
  case Key of
    VK_END: if ssCtrl in Shift then
              GoToTextEnd
            else
              GoToLineEnd;
    VK_HOME: if ssCtrl in Shift then
               GoToTextBegin
             else
               GoToLineBegin;
    VK_LEFT:
      if ssCtrl in Shift then
        CaretPosition := GetPrivWordBeging(CaretPosition)
      else
        MoveCaretLeft;
    VK_RIGHT:
      if ssCtrl in Shift then
        CaretPosition := GetNextWordBeging(CaretPosition)
      else
        MoveCaretRight;
    VK_UP:
      MoveCaretUp;
    VK_DOWN:
      MoveCaretDown;
    VK_PRIOR:
      MoveCaretPageUp;
    VK_NEXT:
      MoveCaretPageDown;
    VK_DELETE, 8: {Delete or BackSpace key was pressed}
      if not ReadOnly then
      begin
        if SelLength <> 0 then
        begin
          if ssShift in Shift then
            CutToClipboard
          else
            ClearSelection;
        end
        else
        begin
          TmpS := Text;
          if Key = VK_DELETE then
            DeleteFrom(CaretPosition,1, [doMoveCaret, doCanUndo])
          else {BackSpace key was pressed}
            DeleteFrom(GetPositionShift(CaretPosition,-1),1, [doMoveCaret, doCanUndo]);
        end;
      end;
    VK_INSERT:
      if ssCtrl in Shift then
        CopyToClipboard
      else
        if ssShift in Shift then
          PasteFromClipboard;
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

  if ((Ord(Keychar) >= 32) or (Keychar = CHAR_13)) and not ReadOnly then
  begin
    WasSelection := SelLength > 0;
    if WasSelection then
      DeleteFrom(GetSelBeg,SelLength,[doMoveCaret, doCanUndo]{true,true, false});
    if WasSelection then
      LTmpOptions := [ioUnDoPairedWithPriv]
    else
      LTmpOptions := [];
    if Keychar <> CHAR_13 then
    begin
      InsertAfter(CaretPosition, KeyChar, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    end
    else
    begin
      TmpS := CHAR_CRLF;
      InsertAfter(CaretPosition, TmpS, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    end;
    SelLength := 0;
    Keychar := CH_ZERO;
  end;
  
  if Key in [VK_END, VK_HOME, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT] then
  begin
    if ssShift in Shift then
    begin
      if not FSelected then
        SelectAtPos(OldCaretPosition);
      SelectAtPos(CaretPosition);
      Repaint;
    end else
      if FSelected then begin
        FSelected := false;
        Repaint;
      end;
  end;
  UpdateCaretPosition(true);
end;

procedure TvgMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single);
begin
  inherited;
  if (Button = mbLeft) and  (ssDouble in Shift) then
  begin
    if vgPtInRect(vgPoint(x,y), ContentRect) then
    begin
      FLMouseSelecting := false;
      SelectWord;
    end;
  end;
  if (Button = mbLeft) and  vgPtInRect(vgPoint(x,y), ContentRect) then
  begin
    FLMouseSelecting := true;
    CaretPosition := GetPointPosition(vgPoint(x,y));
    FSelected := false;
    SelectAtPos(CaretPosition);
    Repaint;
  end;
end;

function TvgMemo.ContentRect: TvgRect;
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
  with Result do
  begin
    if (VScrollBar <> nil) and (VScrollBar.Visible or (csDesigning in ComponentState)) then
      Right := Right - VScrollBar.Width - 2;
    if (HScrollBar <> nil) and (HScrollBar.Visible or (csDesigning in ComponentState)) then
      Bottom := Bottom - HScrollBar.Height - 2;
  end;
end;

procedure TvgMemo.DoContentPaint(Sender: TObject;
  const Canvas: TvgCanvas; const ARect: TvgRect);
var
  TmpRect,
  LEdiTvgRect : TvgRect;
  LSelArea : TSelArea;
  CurSelRect : integer;
  SaveIndex: integer;
begin
  with Canvas do
  begin
    SaveIndex := Canvas.SaveCanvas;
    Canvas.IntersectClipRect(ContentRect);
    // text
    Font.Assign(Font);
    Fill.Assign(FontFill);
    PaintText;
    if IsFocused then
    begin
      // selection
      LSelArea := GetSelArea;
      if GetShowSelection then
      begin
        for CurSelRect := Low(LSelArea) to High(LSelArea) do
        begin
          Fill.Style := vgBrushSolid;
          Fill.SolidColor := $802A8ADF;
          vgIntersectRect(TmpRect, LSelArea[CurSelRect], ContentRect);
          FillRect(TmpRect, 2, 2, AllCorners, 1);
        end;
      end;
      // caret
      Fill.Assign(FontFill);
      with GetPositionPoint(FCaretPosition) do
        FillRect(vgRect(X, Y, X + 2, Y + Font.Size + 2), 0, 0, AllCorners, 1);
    end;
    Canvas.RestoreCanvas(SaveIndex);
  end;
end;

function TvgMemo.GetVScrollBar: TvgScrollBar;
var
  B: TvgObject;
begin
  B := FindResource(VG_VSBAR);
  if (B <> nil) and (B is TvgScrollBar) then
  begin
    Result := TvgScrollBar(B);
    Result.Orientation := vgVertical;
    Result.OnChange := VScrlBarChange;
  end
  else
    Result := nil;
end;

function TvgMemo.GetHScrollBar: TvgScrollBar;
var
  B: TvgObject;
begin
  B := FindResource(VG_HSBAR);
  if (B <> nil) and (B is TvgScrollBar) then
  begin
    Result := TvgScrollBar(B);
    Result.Orientation := vgHorizontal;
    Result.OnChange := HScrlBarChange;
  end
  else
    Result := nil;
end;

procedure TvgMemo.ApplyStyle;
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

procedure TvgMemo.Paint;
begin
end;

procedure TvgMemo.PaintText;
var
  TmpRect: TvgRect;
  CurChar, CurLine, LEndLine: integer;
  TmpPt : TvgPoint;
  LPageSize : single;
  LLeftTopCharPt : TvgPoint;
  LSelBegLine, LSelEndLine : integer;
begin
  TmpRect := ContentRect;
  Canvas.Font.Assign(Font);
  LPageSize := GetPageSize;

  LLeftTopCharPt.X := TmpRect.Left;
  LLeftTopCharPt.Y := TmpRect.Top;

  LSelBegLine := -1;
  LSelEndLine := -1;
  if FSelected then begin
    LSelBegLine := GetSelBeg.Line;
    LSelEndLine := GetSelEnd.Line;
  end;

  CurLine := Trunc(VScrollBarValue);
  if VScrollBarValue + LPageSize - 1 < Lines.Count-1 then
    LEndLine := Trunc(VScrollBarValue + LPageSize-1)
  else
    LEndLine := Lines.Count-1;
  while CurLine <= LEndLine do
  begin
//    if ((CurLine <= LSelBegLine) or (CurLine >= LSelEndLine)) or not GetShowSelection then
    begin
      if PasswordChar <> CH_ZERO then
        for CurChar := 0 to Length(Lines[CurLine]) - 1 do begin
          TmpPt := GetPositionPoint(ComposeCaretPos(CurLine, CurChar));
          DrawPasswordChar(vgRect(TmpPt.X,
                                TmpPt.Y,
                                TmpPt.X + GetPasswordCharWidth,
                                TmpPt.Y + GetLineHeight),
                           false);
        end
      else begin
        TmpPt := GetPositionPoint(ComposeCaretPos(CurLine, 0));
        Canvas.FillText(vgRect(TmpPt.X, TmpPt.Y, 1000, 1000), LocalRect, Lines[CurLine], false, 1, vgTextAlignNear, vgTextAlignNear);
      end;
    end;
    Inc(CurLine);
  end;
end;

procedure TvgMemo.PaintSelectedText;
var
  CurLine, CurPos : integer;
  EndPos, BegPos: TCaretPosition;
  CurLineEndPos, CurLineBegPos : integer;
  TmpPt : TvgPoint;
  SelBegLineVisible, SelEndLineVisible : boolean;
begin
  if not FSelected then
    Exit;

  if (Lines.Count <= 0) or (SelLength <= 0) then
    Exit;

  SelBegLineVisible := true;
  SelEndLineVisible := true;

  BegPos := GetSelBeg;
  if BegPos.Line < VScrollBarValue then
  begin
    BegPos.Line := Round(VScrollBarValue);
    SelBegLineVisible := false;
  end;

  EndPos := GetSelEnd;
  if EndPos.Line > VScrollBarValue + GetPageSize-1 then begin
    EndPos.Line := Round(VScrollBarValue + GetPageSize-1);
    SelEndLineVisible := false;
  end;

  if EndPos.Line < BegPos.Line then
    EndPos.Line := BegPos.Line;

  for CurLine := BegPos.Line to EndPos.Line do begin
    if (CurLine = BegPos.Line) and SelBegLineVisible then
      CurLineBegPos := BegPos.Pos
    else
      CurLineBegPos :=0;

    if (CurLine = EndPos.Line) and SelEndLineVisible then
      CurLineEndPos := EndPos.Pos
    else
      CurLineEndPos := Length(GetLine(CurLine));

    if PasswordChar <> CH_ZERO then
      for CurPos := CurLineBegPos to CurLineEndPos do begin
        TmpPt:= GetPositionPoint(ComposeCaretPos(CurLine,CurPos));
        DrawPasswordChar(vgRect(TmpPt.x,
                              TmpPt.y,
                              TmpPt.x+GetPasswordCharWidth,
                              TmpPt.y+GetLineHeight),
                         true);
      end
    else begin
      TmpPt := GetPositionPoint(ComposeCaretPos(CurLine,CurLineBegPos));
      Canvas.Fill.Style := vgBrushSolid;
      Canvas.Fill.solidColor := $FF0000FF;
      Canvas.FillText(vgRect(TmpPt.X, TmpPt.Y, 1000, 1000), LocalRect, Copy(Lines[CurLine],CurLineBegPos+1,CurLineEndPos-CurLineBegPos{+1}),
        false, 1, vgTextAlignNear, vgTextAlignNear);
    end;
  end;
end;

procedure TvgMemo.UpdateHScrlBarByCaretPos;
var
  LEdiTvgRect: TvgRect;
  LCaretLinePos : integer;
  LCaretLine : integer;
  CurCaretX : integer;
begin
  if Lines.Count <= 0 then
    Exit;
  if Canvas = nil then Exit;

  LEdiTvgRect := ContentRect;
  CurCaretX := Round(GetPositionPoint(CaretPosition).X);

  if not ((CurCaretX < LEdiTvgRect.Left) or
          (CurCaretX > LEdiTvgRect.Right)) then
    Exit;

  LCaretLinePos := CaretPosition.Pos;
  LCaretLine := CaretPosition.Line;

  if FFirstVisibleChar >= (LCaretLinePos + 1) then
  begin
    FFirstVisibleChar := LCaretLinePos;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin
    if PasswordChar <> CH_ZERO then
      while ((LCaretLinePos - FFirstVisibleChar + 1) * GetPasswordCharWidth >
        LEdiTvgRect.Right - LEdiTvgRect.Left)
        and (FFirstVisibleChar < Length(Lines[LCaretLine])) do
        Inc(FFirstVisibleChar)
    else
    begin
      Canvas.Font.Assign(Font);
      while (Canvas.TextWidth(Copy(Lines[LCaretLine], FFirstVisibleChar, LCaretLinePos - FFirstVisibleChar + 1)) > LEdiTvgRect.Right - LEdiTvgRect.Left)
        and (FFirstVisibleChar < Length(Lines[LCaretLine])) do
        Inc(FFirstVisibleChar);
    end;
  end;
  Repaint;
  Canvas.Font.Assign(Font);
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
    HScrollBar.Value := Canvas.TextWidth(Copy(Lines[LCaretLine],1,FFirstVisibleChar-1));
end;

procedure TvgMemo.MouseMove(Shift: TShiftState; x, y, dx, dy: single);
var
  LEdiTvgRect : TvgRect;
begin
  inherited;
  FOldMPt := vgPoint(x,y);

  if FLMouseSelecting then
  begin
    LEdiTvgRect := ContentRect;
    SelectAtMousePoint;
  end;
end;

procedure TvgMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
begin
  inherited;
  FLMouseSelecting := false;
  if SelLength = 0 then
    FSelected := false;
end;

procedure TvgMemo.CopyToClipboard;
begin
  if PasswordChar = CH_ZERO then
    if SelText <> EmptyStr then
      ClipBoard.AsText := {$IFDEF FPC}UTF8Encode{$ENDIF}(SelText);
end;

procedure TvgMemo.PasteFromClipboard;
var
  WasSelection : boolean;
  Data: THandle;
  Insertion: WideString;
begin
  if ReadOnly then Exit;
  try
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

    WasSelection := SelLength >0;
    if WasSelection then
    begin
      DeleteFrom(GetSelBeg,SelLength, [doMoveCaret, doCanUndo]);
      InsertAfter(GetSelBeg, Insertion, [ioSelected, ioMoveCaret, ioCanUndo, ioUndoPairedWithPriv]);
    end
    else
      InsertAfter(CaretPosition, Insertion, [ioSelected, ioMoveCaret, ioCanUndo{, ioUndoPairedWithPriv}]);
    {$ELSE}
  //    SetUpdateState(true);
    WasSelection := SelLength >0;
    if WasSelection then
    begin
      DeleteFrom(GetSelBeg,SelLength, [doMoveCaret, doCanUndo]);
      InsertAfter(GetSelBeg, {$IFDEF FPC}UTF8Decode{$ENDIF}(ClipBoard.AsText), [ioSelected, ioMoveCaret, ioCanUndo, ioUndoPairedWithPriv]);
    end
    else
      InsertAfter(CaretPosition, {$IFDEF FPC}UTF8Decode{$ENDIF}(ClipBoard.AsText), [ioSelected, ioMoveCaret, ioCanUndo{, ioUndoPairedWithPriv}]);
    {$ENDIF}
  finally
//    SetUpdateState(false);
  end;
end;

function TvgMemo.GetPopupMenuClass: TPopupMenuClass;
begin
  Result := TPopupMenu;
end;

procedure TvgMemo.CreatePopupMenu;
var
  TmpItem: TMenuItem;
begin
  FPopupMenu := GetPopupMenuClass.Create(Self);
  FPopupMenu.AutoHotkeys := maManual;
  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_UNDO;
    OnClick := DoUndo;
  end;
  FPopupMenu.Items.Add(TmpItem);

//  FPopupMenu.Items.NewBottomLine;

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

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := STR_MN_SELALL;
    OnClick := DoSelectAll;
  end;
  FPopupMenu.Items.Add(TmpItem);
end;

procedure TvgMemo.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TvgMemo.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TvgMemo.DoDelete(Sender: TObject);
begin
  ClearSelection;
end;

procedure TvgMemo.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

destructor TvgMemo.Destroy;
begin
  FActionStack.Free;
  FPopupMenu.Free;
  FLines.Free;
  inherited;
end;

procedure TvgMemo.UpdatePopupMenuItems;
var
  SelTextEmpty: boolean;
begin
  SelTextEmpty := SelText <> EmptyStr;
  FPopupMenu.Items.Find(STR_MN_UNDO).Enabled := FActionStack.AtLeast(1) and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_CUT).Enabled := SelTextEmpty and (not (PasswordChar <> CH_ZERO)) and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_COPY).Enabled := SelTextEmpty and not (PasswordChar <> CH_ZERO);
  FPopupMenu.Items.Find(STR_MN_PASTE).Enabled := (ClipBoard.AsText <> EmptyStr) and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_DELETE).Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find(STR_MN_SELALL).Enabled := SelText <> Text;
end;

function TvgMemo.GetNextWordBeging(StartPosition: TCaretPosition): TCaretPosition;
var
  SpaceFound,
  WordFound: boolean;
  LLineText : WideString;
  CurPos : integer;
  CurLine : integer;
begin
  CurPos := StartPosition.Pos;
  CurLine := StartPosition.Line;

  if StartPosition.Pos < Length(GetLine(StartPosition.Line)) then begin
    LLineText := GetLine(StartPosition.Line);

    SpaceFound := false;
    WordFound := false;
    while (CurPos + 2 <= Length(LLineText)) and
      ((not ((LLineText[CurPos + 1] <> SPC) and SpaceFound))
      or not WordFound) do
    begin
      if LLineText[CurPos + 1] = SPC then
        SpaceFound := true;
      if LLineText[CurPos + 1] <> SPC then begin
        WordFound := true;
        SpaceFound := false;
      end;

      CurPos := CurPos + 1;
    end;
    if not SpaceFound then
      CurPos := CurPos + 1;
  end else
    if StartPosition.Line < Lines.Count-1 then begin
      CurLine := CurLine+1;
      CurPos := 0;
    end;

  with Result do begin
    Line := CurLine;
    Pos := CurPos;
  end
end;

function TvgMemo.GetPrivWordBeging(StartPosition: TCaretPosition): TCaretPosition;
var
  WordFound: boolean;
  LLineText : WideString;
  CurPos : integer;
  CurLine : integer;
begin
  Result := StartPosition;

  CurPos := StartPosition.Pos;
  CurLine := StartPosition.Line;

  if StartPosition.Pos > 0 then begin
    LLineText := GetLine(StartPosition.Line);

    WordFound := false;
    while (CurPos > 0) and
      ((LLineText[CurPos] <> SPC) or not WordFound) do
    begin
      if LLineText[CurPos] <> SPC then
        WordFound := true;
      CurPos := CurPos - 1;
    end;
  end else
    if (StartPosition.Line-1 >= 0) and (StartPosition.Line-1<=Lines.Count-1) then begin
      CurLine := CurLine-1;
      CurPos := Length(GetLine(CurLine));
    end;

  with Result do begin
    Line := CurLine;
    Pos := CurPos;
  end
end;


procedure TvgMemo.ClearSelection;
begin
  if not ReadOnly then
    DeleteFrom(GetSelBeg, SelLength, [doMoveCaret, doCanUndo]);
end;

procedure TvgMemo.CutToClipboard;
begin
  if PasswordChar = CH_ZERO then
    CopyToClipboard;
  ClearSelection;
end;

procedure TvgMemo.SelectAll;
begin
  FSelStart := ComposeCaretPos(Lines.Count-1, Length(GetLineInternal(Lines.Count-1)));
  FSelEnd := ComposeCaretPos(0,0);
  FSelected := true;
  GoToTextEnd;
  Repaint;
end;

procedure TvgMemo.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TvgMemo.DrawPasswordChar(SymbolRect: TvgRect; Selected: boolean);
var
  LRect : TvgRect;
begin
  vgIntersectRect(LRect, SymbolRect, ContentRect);
  Canvas.Font.Assign(Font);

end;

function TvgMemo.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := Round(GetLineHeight + ContentRect.Top*2);
end;

procedure TvgMemo.SelectWord;
begin
  FSelStart := GetPrivWordBeging(CaretPosition);
  FSelEnd := GetNextWordBeging(CaretPosition);
  FSelected := true;
  Repaint;
end;

procedure TvgMemo.UpdateCarete;
begin
  if IsFocused then
  begin
    ShowCaret;
  end;
end;

procedure TvgMemo.HideCaret;
begin
  FCaretShown := false;
end;

procedure TvgMemo.ShowCaret;
begin
  if IsFocused then
  begin
    FCaretShown := true;
  end;
end;

function TvgMemo.GetPasswordCharWidth: single;
begin
  Canvas.Font.Assign(Font);
  Result := Canvas.TextWidth(PasswordChar);
end;

procedure TvgMemo.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TvgMemo.ContextMenu(const ScreenPosition: TvgPoint);
begin
  inherited;
  if csDesigning in ComponentState then Exit;

  UpdatePopupMenuItems;
  FPopupMenu.PopupComponent := Self;
  FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
end;

procedure TvgMemo.FontChanged(Sender: TObject);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Fillchar(FIntCharsWidths, SizeOf(FIntCharsWidths), 0);
    UpdateCharsWidths;
    UpdateLines;
    UpdateCarete;
    UpdateCaretPosition(false);
    UpdateScrlBarsParams;
  end;
end;

procedure TvgMemo.SetFont(Value: TFont);
begin
{  inherited Font := Value;
  Perform(CM_FONTCHANGED,0,0);}
end;

procedure TvgMemo.SetText(const Value: WideString);
var
  TmpS: WideString;
  LOldText: WideString;
begin
  if ReadOnly and not (([csDesigning, csLoading] * ComponentState) <> []) then
    Exit;

  if not ValidText(Value) then
    Exit;

  TmpS := Value;
  LOldText := Text;

  if (Value <> EmptyStr) and (CharCase <> ecNormal) then
    //
  else
    inherited Text := TmpS;

  if FInternalMustUpdateLines then
  begin
    UpdateLines;
    Repaint;
    UpdateScrlBarsParams;
  end;

  if Text <> LOldText then
    Change;
end;

procedure TvgMemo.SetCaretPosition(const Value: TCaretPosition);
begin
  if Value.Line > Lines.Count-1 then
    FCaretPosition.Line := Lines.Count-1
  else
    FCaretPosition.Line := Value.Line;

  if FCaretPosition.Line < 0 then
    FCaretPosition.Line := 0;

  if Value.Pos < 0 then
    FCaretPosition.Pos := 0
  else
    if Value.Pos > Length(Lines[FCaretPosition.Line]) then
      FCaretPosition.Pos := Length(Lines[FCaretPosition.Line])
    else
      FCaretPosition.Pos := Value.Pos;

  UpdateCaretPosition(true);
end;

procedure TvgMemo.SetPasswordChar(const Value: char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    Repaint;
    UpdateCaretPosition(false);
  end;
end;

procedure TvgMemo.SetSelLength(const Value: integer);
begin
end;

procedure TvgMemo.SetSelStart(const Value: integer);
begin

end;

procedure TvgMemo.SetAutoSelect(const Value: boolean);
begin
  if FAutoSelect <> Value then
    FAutoSelect := Value;
end;

function TvgMemo.GetSelStart: integer;
begin
  if FSelected then
    Result := PosToTextPos(GetSelBeg)
  else
    Result := PosToTextPos(CaretPosition);
end;

function TvgMemo.GetSelArea: TSelArea;
var
  BegLine, EndLine, CurLine : integer;
  LPageSize : single;
  SelBegLineVisible, SelEndLineVisible : boolean;
begin
  if not FSelected then begin
    SetLength(Result,0);
    Exit;
  end;

  SelBegLineVisible := true;
  SelEndLineVisible := true;

  BegLine := GetSelBeg.Line;

  if BegLine < VScrollBarValue then
  begin
    BegLine := Round(VScrollBarValue);
    SelBegLineVisible := false;
  end;

  EndLine := GetSelEnd.Line;
  LPageSize := GetPageSize;

  if EndLine > VScrollBarValue + LPageSize-1 then
  begin
    EndLine := Round(VScrollBarValue + LPageSize-1);
    SelEndLineVisible := false;
  end;

  if EndLine < BegLine then
    EndLine := BegLine;

  SetLength(Result,EndLine-BegLine+1);

  CurLine := BegLine;
  while (CurLine <= EndLine) and (CurLine < Lines.Count) do begin
    with Result[CurLine-BegLine] do begin
      Left := GetPositionPoint(ComposeCaretPos(CurLine,0)).x;
      Right := GetPositionPoint(ComposeCaretPos(CurLine,Length(Lines[CurLine]))).x;
      Top := GetPositionPoint(ComposeCaretPos(CurLine,0)).y;
      Bottom := GetPositionPoint(ComposeCaretPos(CurLine,0)).y+GetLineHeight;
    end;
    Inc(CurLine);
  end;

  if EndLine-BegLine >= 0 then begin
    if SelBegLineVisible then
      Result[0].Left := GetPositionPoint(ComposeCaretPos(BegLine,GetSelBeg.Pos)).x;
    if SelEndLineVisible then
      Result[EndLine-BegLine].Right := GetPositionPoint(ComposeCaretPos(EndLine,GetSelEnd.Pos)).x;
  end;
end;

function TvgMemo.GetSelLength: integer;
begin
  if FSelected then
    Result := PosToTextPos(GetSelEnd)-PosToTextPos(GetSelBeg)
  else
    Result := 0;
end;

function TvgMemo.GetSelText: WideString;
var
  LSelStart,
  LSelLength : Integer;
begin
  if FSelected then begin
    LSelStart := SelStart;
    LSelLength := SelLength;
    Result := Copy(Text, LSelStart + 1, LSelLength);
  end else
    Result := EmptyStr;
end;

procedure TvgMemo.SetCharCase(const Value: TEditCharCase);
var
  TmpS: WideString;
begin
  if FCharCase <> Value then
  begin
    FCharCase := Value;
    if Text <> EmptyStr then
    begin
      TmpS := Text;
    end;
  end;
end;

procedure TvgMemo.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    Repaint;
  end;
end;

procedure TvgMemo.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
  end;
end;

function TvgMemo.ValidText(NewText: WideString): boolean;
begin
  Result := true;
end;

procedure TvgMemo.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then begin
    FTextAlignment := Value;
    Repaint;
  end;
end;

procedure TvgMemo.UpdateCaretPosition(UpdateScrllBars : boolean);
var
  TmpPt : TvgPoint;
  TmpRect : TvgRect;
begin
  if UpdateScrllBars then
  begin
    UpdateVScrlBarByCaretPos;
    UpdateHScrlBarByCaretPos;
  end;
  TmpRect := ContentRect;
  TmpPt := GetPositionPoint(CaretPosition);
  if IsFocused then
    Repaint;
  with TmpPt do
    if (X >= TmpRect.Left) and (X <= TmpRect.Right) and
       (Y >= TmpRect.Top) and (Y+GetLineHeight <= TmpRect.Bottom) then
      ShowCaret
    else
      HideCaret;
end;

function TvgMemo.GetLineRealEnd(AStartPos : TCaretPosition; PText : PWideString) : TCaretPosition;
begin
  Result.Line := AStartPos.Line;
  while (Result.Line+1 <= Lines.Count-1) and
    (GetLineInternal(Result.Line) = GetLine(Result.Line)) do
    Result.Line := Result.Line + 1;

  if (Result.Line <= Lines.Count-1) and (Lines.Count > 0) then begin
    Result.Pos := Length(GetLine(Result.Line)) + FLinesBegs[Result.Line]-1
  end else
    Result.Pos := 0;
end;

function TvgMemo.FillLocalLinesBegs(PText : PWideString; ABegChar, AEndChar : integer; TmpLinesBegs : PLinesBegs) : integer;
var
  LCurChar : integer;
  //TmpS : WideString;
  LTmpWidth : single;
  LEdiTvgRectWidth : single;
  LLocalWidesLineWidth : single;
  LWidth : single;
  CurLineIsEmpty : boolean;
begin
  Result := 0;
  SetLength(TmpLinesBegs^, 0);

  if PText^ = EmptyStr then
    Exit;

  LCurChar := ABegChar;
  
  LTmpWidth := 0;
  CurLineIsEmpty := true;

  with ContentRect do
    LEdiTvgRectWidth := Right-Left;

  Result := -1;
  LLocalWidesLineWidth := -1;

  Canvas.Font.Assign(Font);
  while LCurChar <= AEndChar do
  begin
    if (PText^[LCurChar] = CHAR_13) then
    begin
      if  LCurChar+1 <= Length(PText^) then
        if PText^[LCurChar+1] = CHAR_10 then
          Inc(LCurChar);

      SetLength(TmpLinesBegs^,Length(TmpLinesBegs^)+1);
      TmpLinesBegs^[Length(TmpLinesBegs^)-1] := LCurChar+1;

      LWidth := {Canvas.TextWidth(TmpS)} LTmpWidth;
      if LWidth > LLocalWidesLineWidth  then
      begin
        LLocalWidesLineWidth := LWidth;
        Result := Length(TmpLinesBegs^)-1;
      end;

      LTmpWidth := 0;
      CurLineIsEmpty := true;
    end
    else
    begin
      if GetRealWordWrap and
         (LTmpWidth+CharsWidths(PText^[LCurChar]){(Canvas.TextWidth(TmpS+PText^[LCurChar])}>=LEdiTvgRectWidth) and
         not CurLineIsEmpty{(Length(TmpS)>0)} then begin
        SetLength(TmpLinesBegs^,Length(TmpLinesBegs^)+1);
        TmpLinesBegs^[Length(TmpLinesBegs^)-1] := LCurChar;
        LTmpWidth := 0;
        CurLineIsEmpty := true;
      end;
      LTmpWidth := LTmpWidth + CharsWidths(PText^[LCurChar]);{ TmpS := TmpS + PText^[LCurChar];}
      CurLineIsEmpty := false;
    end;
    Inc(LCurChar);
  end;

  LWidth := LTmpWidth{Canvas.TextWidth(TmpS)};
  if LWidth > LLocalWidesLineWidth then
    Result := Length(TmpLinesBegs^)-1;

  if Length(TmpLinesBegs^) = 0 then
    Result := 0;
end;

procedure TvgMemo.UpdateRngLinesBegs(PText : PWideString;AUpdBegLine, AUpdEndLine,
  AUpdBegChar, AUpdEndChar, ACharDelta, AOldWideslLineWidth : integer);
var
  LUpdEndChar,
  LNewWidesLineIndex,
  LLineDelta, i : integer;
  LTmpLinesBegs : TLinesBegs;
begin
  if (Length(FLinesBegs) = 0) and (PText^ <> EmptyStr) then
  begin
    SetLength(FLinesBegs, 1);
    FLinesBegs[0] := 1;
  end;

  LUpdEndChar := AUpdEndChar + ACharDelta;
  LNewWidesLineIndex := FillLocalLinesBegs(PText ,AUpdBegChar, LUpdEndChar, @LTmpLinesBegs)
  +AUpdBegLine;

  LLineDelta := Length(LTmpLinesBegs) - (AUpdEndLine-AUpdBegLine);

  if LLineDelta > 0 then
  begin
    SetLength(FLinesBegs, Length(FLinesBegs) + LLineDelta);
    for i := Length(FLinesBegs)-1 downto AUpdEndLine+1+LLineDelta do
      FLinesBegs[i] := FLinesBegs[i-LLineDelta] + ACharDelta;
  end
  else
  begin
    for i := AUpdBegLine+1 to Length(FLinesBegs)-1+LLineDelta do
      FLinesBegs[i] := FLinesBegs[i-LLineDelta] + ACharDelta;
    SetLength(FLinesBegs, Length(FLinesBegs) + LLineDelta);
  end;

  for i := 0 to Length(LTmpLinesBegs) - 1 do
    if AUpdBegLine+i+1 <= Length(FLinesBegs)-1 then
      FLinesBegs[AUpdBegLine+i+1] := LTmpLinesBegs[i];

  if FWidesLineIndex > Length(FLinesBegs)-1 then
    FWidesLineIndex := Round(GetWidestLine)
  else
  if TextWidth[LNewWidesLineIndex] >= AOldWideslLineWidth then
    FWidesLineIndex := LNewWidesLineIndex
  else
    if not ((FWidesLineIndex < AUpdBegLine) or (FWidesLineIndex > AUpdEndLine)) then
      FWidesLineIndex := GetWidestLine;

  Repaint;
  UpdateScrlBarsParams;
end;

procedure TvgMemo.InsertAfter(Position: TCaretPosition; S: WideString;
  Options : TInsertOptions);
var
  LText : WideString;
  Insertion : WideString;
  LUpdBegLine, LUpdBegChar, LUpdEndLine, LUpdEndChar : integer;

  LInsertionLength : integer;
  LOldWideslLineWidth : single;
begin
  LText := Text;
  Insertion := S;
  if MaxLength > 0 then
    Insertion := Copy(Insertion, 1, MaxLength - Length(LText));

  if ioCanUndo in Options then
    FActionStack.FragmentInserted(PosToTextPos(Position), Length(S), ioUnDoPairedWithPriv in Options);

  LUpdBegLine := Position.Line;
  if Position.Line <= Length(FLinesBegs)-1 then
    LUpdBegChar := FLinesBegs[Position.Line]
  else
    LUpdBegChar := 1;

  with GetLineRealEnd(Position, @LText) do
  begin
    LUpdEndLine := Line;
    LUpdEndChar := Pos;
  end;

  LInsertionLength := Length(Insertion);
  LOldWideslLineWidth := TextWidth[FWidesLineIndex];

  Insert(Insertion, LText, PosToTextPos(Position)+1);
  try
    FInternalMustUpdateLines := false;
    Text := LText;
  finally
    FInternalMustUpdateLines := true;
  end;

  UpdateRngLinesBegs(@LText, LUpdBegLine, LUpdEndLine,
    LUpdBegChar, LUpdEndChar, LInsertionLength, Round(LOldWideslLineWidth));

  if ioSelected in Options then
  begin
    FSelStart := Position;
    FSelEnd := GetPositionShift(Position,Length(Insertion));
    FSelected := true;
    CaretPosition := FSelEnd;
  end
  else
  begin
    if not (csLoading in ComponentState) then
      MoveCareteBy(Length(Insertion));
  end;
end;

procedure TvgMemo.DeleteFrom(Position : TCaretPosition; ALength: integer;
  Options : TDeleteOptions);
var
  LUpdBegLine, LUpdEndLine,
  LUpdBegChar, LUpdEndChar : integer;
  LText : WideString;
  LTmpPos, LTmpLength : integer;
  LOldWideslLineWidth : integer;
begin
  LText := Text;

  LTmpLength := ALength;
  LTmpPos :=  PosToTextPos(Position)+1;

  if (LTmpPos+ALength-1+1 <= System.Length(LText)) and
     (LTmpPos+ALength-1 >= 1) and
     (LText[LTmpPos+ALength-1]=CHAR_13) and
     (LText[LTmpPos+ALength-1+1]=CHAR_10) then
    LTmpLength := LTmpLength + 1;

  if (LTmpPos-1 >= 0) and
     (LTmpPos <= System.Length(LText)) and
     (LText[LTmpPos]=CHAR_10) and
     (LText[LTmpPos-1]=CHAR_13) then begin
    LTmpLength := LTmpLength + 1;
    LTmpPos := LTmpPos-1;
  end;

  if (doCanUndo in Options) and (LTmpLength > 0) then
    FActionStack.FragmentDeleted(LTmpPos, Copy(LText,LTmpPos,LTmpLength));

  LUpdBegLine := Position.Line;
  if Position.Line <= Length(FLinesBegs)-1 then
    LUpdBegChar := FLinesBegs[Position.Line]
  else
    LUpdBegChar := 1;

  with GetLineRealEnd(GetPositionShift(Position, LTmpLength-1),@LText) do begin
    LUpdEndLine := Line;
    LUpdEndChar := Pos;
  end;

  LOldWideslLineWidth := Round(TextWidth[FWidesLineIndex]);

  Delete(LText,LTmpPos,LTmpLength);

  try
    FInternalMustUpdateLines := false;
    Text := LText;
  finally
    FInternalMustUpdateLines := true;
  end;

  UpdateRngLinesBegs(@LText, LUpdBegLine, LUpdEndLine,
    LUpdBegChar, LUpdEndChar, -LTmpLength, LOldWideslLineWidth);

  if (doMoveCaret in Options) or (SelLength <> 0) then begin
    FSelected := false;
    CaretPosition := Position;
  end;
end;

procedure TvgMemo.DoUndo(Sender: TObject);
begin
  UnDo;
end;

procedure TvgMemo.UnDo;
begin
  FActionStack.RollBackAction;
end;

procedure TvgMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if VScrollBar <> nil then
      VScrollBar.Visible := ScrollBars in [ssBoth, ssVertical];
    if HScrollBar <> nil then
      HScrollBar.Visible := ScrollBars in [ssBoth, ssHorizontal];
    UpdateScrlBars;
    UpdateCaretPosition(false);
  end;
end;

procedure TvgMemo.UpdateScrlBarsParams;
var
  LPageSize : single;
  LMaxLineWidth : single;
begin
  if not Assigned(Parent) then Exit;
  if (VScrollBar = nil) or (HScrollBar = nil) then Exit;

  {Updating Vertical scrollbar params}
  LPageSize := GetPageSize;
  if Lines.Count > 0 then
  begin
    Canvas.Font.Assign(Font);
    VScrollBar.ViewportSize := LPageSize;
  end
  else
    VScrollBar.ViewportSize := 0;
  VScrollBar.Max := Lines.Count;

  if VScrollBarValue + LPageSize > Lines.Count then
    VScrollBar.Value := Lines.Count - LPageSize;

  with VScrollBar do
    Visible := (LPageSize < Max - Min + 1) and (Max - Min <> 0);

  {Updating Horizontal scrollbar params}
  if not GetRealWordWrap then
  begin
    Canvas.Font.Assign(Font);
    LMaxLineWidth := Canvas.TextWidth(Lines[FWidesLineIndex]);
    with ContentRect do
      LPageSize := Right-Left;

    with HScrollBar do
    begin
      Min := 0;
      Max := LMaxLineWidth + 5;
      ViewportSize := LPageSize;
      Visible := (LPageSize < Max - Min + 1) and (Max - Min <> 0);
    end;
  end
  else
    HScrollBar.Visible := false;

  UpdateScrlBars;
end;

procedure TvgMemo.Realign;
begin
  inherited Realign;
  UpdateScrlBars;
  if GetRealWordWrap then
  begin
    StorePositions;
    UpdateLines;
    RestorePositions;
  end;
  UpdateScrlBarsParams;
  UpdateHScrlBarByCaretPos;
end;

procedure TvgMemo.SetLines(const Value: TvgWideStrings);
begin
  FLines.Assign(Value);
end;

function TvgMemo.TextPosToPos(APos : integer) : TCaretPosition;
var
  CurRangeBeg, CurRangeEnd : integer;
  TmpI : integer;
begin
  with Result do begin
    Line := 0;
    Pos := 0;
  end;

  if Lines.Count <= 0 then
    Exit;

  CurRangeBeg := 0;
  CurRangeEnd := Length(FLinesBegs)-1;
  repeat
    if ((CurRangeBeg < Length(FLinesBegs)-1) and
        (APos+1>=FLinesBegs[CurRangeBeg]) and
        (APos+1<FLinesBegs[CurRangeBeg+1]))
    or ((CurRangeBeg = Length(FLinesBegs)-1) and
        (APos+1>=FLinesBegs[CurRangeBeg]))
    then
      CurRangeEnd := CurRangeBeg
    else
    begin
      if APos+1 < FLinesBegs[CurRangeBeg] then begin
        TmpI := CurRangeEnd - CurRangeBeg+1;
        CurRangeEnd := CurRangeBeg;
        CurRangeBeg := CurRangeBeg - TmpI div 2;
      end else
        if APos+1 >= FLinesBegs[CurRangeEnd] then begin
          TmpI := CurRangeEnd - CurRangeBeg+1;
          CurRangeBeg := CurRangeEnd;
          CurRangeEnd := CurRangeEnd + TmpI div 2;
        end else
          CurRangeEnd := (CurRangeBeg + CurRangeEnd) div 2;

      if CurRangeBeg < 0 then
        CurRangeBeg := 0;

      if CurRangeEnd < 0 then
        CurRangeEnd := 0;

      if CurRangeEnd > Length(FLinesBegs)-1 then
        CurRangeEnd := Length(FLinesBegs)-1;

      if CurRangeBeg > Length(FLinesBegs)-1 then
        CurRangeBeg := Length(FLinesBegs)-1;
    end;

  until CurRangeBeg = CurRangeEnd;
  Result.Line := CurRangeBeg;

  if Result.Line <= Length(FLinesBegs)-1 then
    Result.Pos := APos-FLinesBegs[Result.Line]+1;
end;

procedure TvgMemo.UpdateLines;
var
  LCurChar : integer;
  //TmpS : WideString;
  LTmpWidth : single;
  LEdiTvgRectWidth : single;
  LText : WideString;
  LLongestLineWidth : single;
  CurLineIsEmpty : boolean;
begin
  FWidesLineIndex := 0;
  SetLength(FLinesBegs,0);
  if Text = EmptyStr then
    Exit;

  LCurChar := 1;

  LTmpWidth := 0;
  CurLineIsEmpty := true;
  SetLength(FLinesBegs,0);
  LText := Text;
  SetLength(FLinesBegs,1);
  FLinesBegs[0] := 1;
  with ContentRect do
    LEdiTvgRectWidth := Right-Left;
  LLongestLineWidth := 0;

  if Canvas = nil then Exit;
  Canvas.Font.Assign(Font);
  UpdateCharsWidths;
  while LCurChar <= Length(LText) do
  begin
    if (LText[LCurChar] = CHAR_13) then
    begin

      if  LCurChar+1 <= Length(LText) then
        if LText[LCurChar+1] = CHAR_10 then
          Inc(LCurChar);

      if LLongestLineWidth < LTmpWidth{Canvas.TextWidth(TmpS)} then begin
        LLongestLineWidth := LTmpWidth{Canvas.TextWidth(TmpS)};
        FWidesLineIndex := Length(FLinesBegs)-1;
      end;

      SetLength(FLinesBegs,Length(FLinesBegs)+1);
      FLinesBegs[Length(FLinesBegs)-1] := LCurChar+1;

      LTmpWidth := 0;
      CurLineIsEmpty := true;
    end
    else
    begin
      if GetRealWordWrap and
         (LTmpWidth+CharsWidths(LText[LCurChar]){(Canvas.TextWidth(TmpS+LText[LCurChar])}>=LEdiTvgRectWidth) and
         not CurLineIsEmpty{(Length(TmpS)>0)} then begin

        SetLength(FLinesBegs,Length(FLinesBegs)+1);
        FLinesBegs[Length(FLinesBegs)-1] := LCurChar;
        
        LTmpWidth := 0;
        CurLineIsEmpty := true;
      end;
      //TmpS := TmpS + LText[LCurChar];
      LTmpWidth := LTmpWidth + CharsWidths(LText[LCurChar]);
      CurLineIsEmpty := false;
    end;
    Inc(LCurChar);
  end;

  if LLongestLineWidth < LTmpWidth{Canvas.TextWidth(TmpS)} then
    FWidesLineIndex := Length(FLinesBegs)-1;
end;

procedure TvgMemo.MoveCaretLeft;
begin
  MoveCareteBy(-1);
end;

procedure TvgMemo.MoveCaretRight;
begin
  MoveCareteBy(1);
end;

procedure TvgMemo.MoveCareteBy(Delta : integer);
begin
  CaretPosition := GetPositionShift(CaretPosition, Delta);
end;

procedure TvgMemo.VScrlBarChange(Sender: TObject);
begin
  UpdateCaretPosition(false);
  Repaint;
end;

procedure TvgMemo.VScrlBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with Sender as TScrollBar do
    if (ScrollPos > Max - PageSize + 1) and (PageSize <> 0) then
      ScrollPos := Max - PageSize + 1;
end;

function TvgMemo.GetLineHeight: single;
begin
  if Assigned(Parent) and (Canvas <> nil) then
  begin
    Canvas.Font.Assign(Font);
    Result := Canvas.TextHeight(VG_PQ);
  end
  else
    Result := 1;
end;

procedure TvgMemo.HScrlBarChange(Sender: TObject);
begin
  Repaint;
  UpdateCaretPosition(false);
end;

procedure TvgMemo.HScrlBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with Sender as TScrollBar do
    if (ScrollPos > Max - PageSize + 1) and (PageSize <> 0) then
      ScrollPos := Max - PageSize + 1;
end;

procedure TvgMemo.UpdateVScrlBarByCaretPos;
var
  LCaretPosLine : integer;
  LPageSize : single;
begin
  LCaretPosLine := CaretPosition.Line;

  LPageSize := GetPageSize;

  if (VScrollBar <> nil) and (LCaretPosLine < VScrollBarValue) then
    VScrollBar.Value := LCaretPosLine;

  if (VScrollBar <> nil) and (LCaretPosLine > VScrollBarValue + LPageSize-1) then
    VScrollBar.Value := LCaretPosLine-LPageSize+1;
end;

procedure TvgMemo.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    UpdateLines;
    UpdateScrlBarsParams;
    Repaint;
    UpdateCaretPosition(false);
  end;
end;

procedure TvgMemo.GetLineBounds(LineIndex: integer; var LineBeg,
  LineLength: integer);
begin
  if Length(FLinesBegs) = 0 then
  begin
    LineBeg := 1;
    LineLength := 0;
    Exit;
  end;

  if (LineIndex <= Length(FLinesBegs)-1) and (LineIndex >= 0) then
  begin
    LineBeg := FLinesBegs[LineIndex];
    if (LineIndex+1 < Length(FLinesBegs))then
      LineLength := FLinesBegs[LineIndex+1]-LineBeg
    else
      LineLength := Length(Text)-LineBeg+1;
  end
  else
  begin
    LineBeg := 0;
    LineLength := 0;
  end;
end;

function TvgMemo.GetLineCount: integer;
begin
  if Text <> EmptyStr then
    Result := Length(FLinesBegs)
  else
    Result := 0;
end;

function TvgMemo.GetLine(Index: integer): WideString;
begin
  Result := GetLineInternal(Index);
  if Length(Result) > 0 then
  begin
    if Result[Length(Result)] = CHAR_10 then
      Delete(Result,Length(Result),1);
    if Result[Length(Result)] = CHAR_13 then
      Delete(Result,Length(Result),1);
  end;
end;

procedure TvgMemo.InsertLine(Index: Integer; const S: WideString);
begin
  if Index < GetLineCount then
    InsertAfter(ComposeCaretPos(Index, 0),S+CHAR_CRLF,[])
  else
    if (Index > 0) and (GetLineCount > 0) then
    begin
      InsertAfter(ComposeCaretPos(Index - 1, Length(GetLineInternal(Index - 1))),CHAR_CRLF+S,[])
    end
    else
      InsertAfter(ComposeCaretPos(Index, 0),S,[]);
end;

procedure TvgMemo.DeleteLine(Index: Integer);
begin
  if Index = GetLineCount - 1 then
    DeleteFrom(ComposeCaretPos(Index-1,Length(GetLineInternal(Index-1))){LLineBeg-1} ,Length(GetLineInternal(Index))+1, [])
  else
    DeleteFrom(ComposeCaretPos(Index,0){LLineBeg}, Length(GetLineInternal(Index))+1, []);
end;

procedure TvgMemo.ClearLines;
begin
  Text := EmptyStr;
end;

procedure TvgMemo.SelectAtMousePoint;
var
  TmpPt : TvgPoint;
  LEdiTvgRect : TvgRect;
begin
  LEdiTvgRect := ContentRect;
  TmpPt := FOldMPt;
  with TmpPt, LEdiTvgRect do begin
    if y < Top then
      y := Top
    else
      if y > Bottom then
        y := Bottom;

    if x < Left then
      x := Left
    else
      if x > Right then
        x := Right;
  end;

  CaretPosition :=  GetPointPosition(TmpPt);
  SelectAtPos(CaretPosition);
  Repaint;
end;

function TvgMemo.GetPageSize: single;
begin
  with ContentRect do
    Result := (Bottom-Top) / GetLineHeight;
end;

function TvgMemo.GetRealWordWrap: boolean;
begin
  Result := WordWrap or (FTextAlignment in [taRightJustify, taCenter]);
end;

procedure TvgMemo.UpdateScrlBars;
begin
  if (HScrollBar = nil) or (VScrollBar = nil) then Exit;

    with HScrollBar do
    begin
      Orientation := vgHorizontal;
      Width := Self.Width - 2 - 2;
      if VScrollBar.Visible or (csDesigning in ComponentState) then
        Width := Width -VScrollBar.Width;
  //    UpdateSytemSize;
      Height := 18;
      Position.Y := Self.Height - 2 - Height;
      Position.X := 2;
    end;
    with VScrollBar do
    begin
      Orientation := vgVertical;
      Position.Y := 2;
      Height := Self.Height - 2 - 2;
      if HScrollBar.Visible or (csDesigning in ComponentState) then
        Height := Height - HScrollBar.Height;
      Width := 18;
  //    UpdateSytemSize;
      Position.X := Self.Width - Width - 2;
    end;
end;

function TvgMemo.GetTextWidth(LineNum: Integer): single;
begin
  if (LineNum >= 0) and (LineNum <= Lines.Count-1) then
  begin
    Canvas.Font.Assign(Font);
    Result := Canvas.TextWidth(Lines[LineNum]);
  end
  else
    Result := 0;
end;

procedure TvgMemo.SetUpdateState(Updating: boolean);
begin
  {if FUpdating and not Updating then begin
    FUpdating := false;
    UpdateScrlBarsParams;
    UpdateCaretPosition(false);
    Repaint;
  end else
    FUpdating := Updating;}
end;

function TvgMemo.PosToTextPos(APostion: TCaretPosition): integer;
var
  LTmpLine : integer;
begin
  Result := 0;
  if Text = EmptyStr then
    Exit;

  with APostion do begin
    if Line <= Length(FLinesBegs)-1 then
      LTmpLine := Line
    else
      LTmpLine := Length(FLinesBegs)-1;

    Result := FLinesBegs[LTmpLine];

    if Pos <= Length(GetLineInternal(LTmpLine)) then
      Result := Result + Pos -1
    else
      Result := Result + Length(GetLineInternal(LTmpLine)) -1;

  end;
end;

function TvgMemo.GetLineInternal(Index: integer): WideString;
var
  LLineBeg, LLineLength : integer;
begin
  GetLineBounds(Index, LLineBeg, LLineLength);
  Result := Copy(Text, LLineBeg, LLineLength);
end;

procedure TvgMemo.GoToTextBegin;
begin
  with FCaretPosition do
  begin
    Line := 0;
    Pos := 0;
  end;
end;

procedure TvgMemo.GoToTextEnd;
begin
  with FCaretPosition do
  begin
    Line := Lines.Count - 1;
    if Line >= 0 then
      Pos := Length(Lines[Line])
    else
      Pos := 0;
  end;
end;

procedure TvgMemo.GoToLineEnd;
begin
  with FCaretPosition do
  begin
    if Line <= Lines.Count-1 then
      Pos := Length(GetLine(CaretPosition.Line));
  end;
end;

procedure TvgMemo.GoToLineBegin;
begin
  with FCaretPosition do
  begin
    Pos := 0;
  end;
end;

function TvgMemo.GetSelBeg: TCaretPosition;
begin
  if FSelStart.Line < FSelEnd.Line then
    Result := FSelStart
  else
    if FSelEnd.Line < FSelStart.Line then
      Result := FSelEnd
    else
      if FSelStart.Pos < FSelEnd.Pos then
        Result := FSelStart
      else
        Result := FSelEnd;
end;

function TvgMemo.GetSelEnd: TCaretPosition;
begin
  if FSelStart.Line > FSelEnd.Line then
    Result := FSelStart
  else
    if FSelEnd.Line > FSelStart.Line then
      Result := FSelEnd
    else
      if FSelStart.Pos > FSelEnd.Pos then
        Result := FSelStart
      else
        Result := FSelEnd;
end;

procedure TvgMemo.SelectAtPos(APos: TCaretPosition);
begin
  if not FSelected then begin
    FSelStart := APos;
    FSelEnd := APos;
    FSelected := true;
  end else begin
    FSelEnd := APos;
  end;
end;

function TvgMemo.GetPositionShift(APos: TCaretPosition;
  Delta: integer): TCaretPosition;
var
  LNewPos : TCaretPosition;
  LNewTextPos : integer;
  i : integer;
  CurLineText : WideString;
begin
  LNewPos := APos;
  with LNewPos do
    if Delta >= 14 then begin
      LNewTextPos := PosToTextPos(CaretPosition)+Delta;

      if Delta > 0 then begin
        if (LNewTextPos+1 <= Length(Text)) and
           (Text[LNewTextPos+1] = CHAR_10) then
          Inc(LNewTextPos);
      end else
        if Delta < 0 then begin
          if (LNewTextPos+1-1 >= Length(Text)) and
             (Text[LNewTextPos+1-1] = CHAR_10) then
          Dec(LNewTextPos);
        end;

      LNewPos := TextPosToPos(LNewTextPos);
    end else begin
      CurLineText := GetLineInternal(Line);
      if Delta > 0 then begin
        i := 1;
        while i <= Delta do begin
          Pos := Pos+1;
          if (Pos+1 <= Length(CurLineText)) and (CurLineText[Pos+1] = CHAR_10) then begin
            Inc(Pos); Inc(i);
          end;
          if Pos+1 > Length(CurLineText) then begin
            if Line+1 <= Lines.Count-1 then begin
              Line := Line+1;
              CurLineText := GetLineInternal(Line);
              Pos := 0;
            end else
              Pos := Length(CurLineText);
          end;
          Inc(i);
        end;
      end else begin {Delta < 0}
        i := 1;
        while i <= Abs(Delta) do begin
          if Pos-1 >= 0 then
            Pos := Pos-1
          else begin
            if Line -1 >= 0 then begin
              Line := Line-1;
              CurLineText := GetLineInternal(Line);
              if CurLineText[Length(CurLineText)] = CHAR_10 then
                Pos := Length(CurLineText)-2
              else
                Pos := Length(CurLineText)-1;
            end;
          end;
          Inc(i);
        end;
      end;
    end;
  Result := LNewPos;
end;

procedure TvgMemo.RestorePositions;
begin
  if FOldCaretPos >= 0 then
    CaretPosition := TextPosToPos(FOldCaretPos);
  if FSelected and (FOldSelStartPos >= 0) then begin
    FSelStart := TextPosToPos(FOldSelStartPos);
    FSelEnd := TextPosToPos(FOldSelEndPos);
    FOldSelStartPos := -1;
  end;
end;

procedure TvgMemo.StorePositions;
begin
  FOldCaretPos := PosToTextPos(CaretPosition);
  if FSelected then begin
    FOldSelStartPos := PosToTextPos(FSelStart);
    FOldSelEndPos := PosToTextPos(FSelEnd);
  end;
end;

procedure TvgMemo.MoveCaretVertical(LineDelta: integer);
var
  NewLine, NewY, OldX : integer;
begin
  with FCaretPosition do
  begin
    NewLine := Line+LineDelta;
    if NewLine < 0 then
      NewLine := 0
    else
      if NewLine > Lines.Count -1 then
        NewLine := Lines.Count -1;

    NewY := Round(GetPositionPoint(ComposeCaretPos(NewLine,Pos)).Y);
    OldX := Round(GetPositionPoint(CaretPosition).X);
    Line := NewLine;
    Pos := Round(GetPointPosition(vgPoint(OldX,NewY)).Pos);
  end;
end;

procedure TvgMemo.MoveCaretDown;
begin
  MoveCaretVertical(1);
end;

procedure TvgMemo.MoveCaretUp;
begin
  MoveCaretVertical(-1);
end;

procedure TvgMemo.MoveCaretPageDown;
begin
//  MoveCaretVertical(GetPageSize);
end;

procedure TvgMemo.MoveCaretPageUp;
begin
//  MoveCaretVertical(-GetPageSize);
end;

function TvgMemo.GetWidestLine: integer;
var
  i : integer;
  LWidth, LMaxWidth : single;
begin
  Result := -1;
  LMaxWidth := -1;
  for i := 0 to Lines.Count-1 do
  begin
    LWidth := TextWidth[i];
    if LWidth > LMaxWidth then
    begin
      Result := i;
      LMaxWidth := LWidth;
    end;
  end;
end;

function TvgMemo.GetShowSelection: boolean;
begin
  Result := IsFocused or not HideSelection;
end;

function TvgMemo.CharsWidths(Char: System.WideChar): single;
begin
  if FIntCharsWidths[Char] <> 0 then
    Result := FIntCharsWidths[Char]
  else
  begin
    Canvas.Font.Assign(Font);
    FIntCharsWidths[Char] := Canvas.TextWidth(Char);
    Result := FIntCharsWidths[Char]
  end;
end;

procedure TvgMemo.UpdateCharsWidths;
var
  CurCh : System.WideChar;
begin
  if FIntCharsWidths[System.Widechar(VG_CHAR_A_U)] <> 0 then Exit;
  Canvas.Font.Assign(Font);
  for CurCh := CHAR_32 to System.WideChar(255) do
    FIntCharsWidths[CurCh] := Canvas.TextWidth(CurCh);
end;

procedure TvgMemo.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := VScrollBar.Value - (WheelDelta / 30);
  end;
end;

function TvgMemo.VScrollBarValue: single;
begin
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
    Result := VScrollBar.Value
  else
    Result := 0;
end;

function TvgMemo.HScrollBarValue: single;
begin
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
    Result := HScrollBar.Value
  else
    Result := 0;
end;

{ TEdtActionStack }

constructor TEdtActionStack.Create(AOwner: TvgMemo);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TEdtActionStack.Destroy;
var
  TmpItem : PEdtAction;
begin
  while AtLeast(1) do begin
    TmpItem := Pop;
    Finalize(TmpItem^);
    FreeMem(TmpItem);
  end;
  inherited;
end;

procedure TEdtActionStack.FragmentDeleted(StartPos: integer;
  Fragment: WideString);
var
  TmpItem : PEdtAction;
begin
  if Fragment = EmptyStr then Exit;

  if (not AtLeast(1)) or
      not ((PEdtAction(Peek)^.ActionType=atDelete) and
           (PEdtAction(Peek)^.StartPosition-StartPos-Length(Fragment) <= 1) and
           (PEdtAction(Peek)^.StartPosition-StartPos >= 0)) then
  begin
    New(TmpItem);
    Initialize(TmpItem^);
    Push(TmpItem);

    with TmpItem^ do begin
      ActionType := atDelete;
      StartPosition := StartPos;
      DeletedFragment := Fragment;
      PairedWithPriv := false;
    end;
  end
  else
    case PEdtAction(Peek)^.ActionType of
      atDelete : begin
        if StartPos > 0 then begin
          if StartPos < PEdtAction(Peek)^.StartPosition then
            PEdtAction(Peek)^.DeletedFragment := Fragment+PEdtAction(Peek)^.DeletedFragment
          else
            PEdtAction(Peek)^.DeletedFragment := PEdtAction(Peek)^.DeletedFragment+Fragment;
          PEdtAction(Peek)^.StartPosition := StartPos;
        end;
      end;
    end;
end;

procedure TEdtActionStack.FragmentInserted(StartPos, FragmentLength: integer; IsPairedWithPriv : boolean);
var
  TmpItem : PEdtAction;
begin
  if FragmentLength = 0 then Exit;

  if (not AtLeast(1)) or
      not ((PEdtAction(Peek)^.ActionType=atInsert) and
           (PEdtAction(Peek)^.StartPosition+PEdtAction(Peek)^.Length =StartPos)) then
  begin
    New(TmpItem);
    Initialize(TmpItem^);
    Push(TmpItem);
    with TmpItem^ do begin
      ActionType := atInsert;
      StartPosition := StartPos;
      Length := FragmentLength;
      PairedWithPriv := IsPairedWithPriv;
    end;
  end else
    case PEdtAction(Peek)^.ActionType of
      atInsert : PEdtAction(Peek)^.Length := PEdtAction(Peek)^.Length+FragmentLength;
    end;
end;

procedure TEdtActionStack.CaretMovedBy(Shift: integer);
begin

end;

function TEdtActionStack.RollBackAction: boolean;
var
  TmpItem : PEdtAction;
  WasPaired : boolean;
  LTmpOptions : TInsertOptions;
begin
  Result := AtLeast(1);
  if not(Result and Assigned(FOwner)) then Exit;

  repeat
    TmpItem := Pop;

    with TmpItem^, FOwner do begin
      if DeletedFragment<>CHAR_CRLF then
        LTmpOptions := [ioSelected]
      else
        LTmpOptions := [];

      case ActionType of
        atDelete : InsertAfter(TextPosToPos(StartPosition-1),DeletedFragment,LTmpOptions+[ioMoveCaret]);
        atInsert : DeleteFrom(TextPosToPos(StartPosition),Length,[doMoveCaret]);
      end;
    end;

    WasPaired := TmpItem^.PairedWithPriv;
    Finalize(TmpItem^);
    Dispose(TmpItem);
  until (not AtLeast(1)) or (not WasPaired);
end;

initialization
  RegisterVGObjects(VG_I_CONTROLS, [TvgMemo]);
  RegisterVGObjects(VG_I_HUD, [TvgHudMemo]);
end.

