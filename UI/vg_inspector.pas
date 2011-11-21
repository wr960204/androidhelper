unit vg_inspector;

{$I vg_define.inc}

interface

uses Graphics, Classes, SysUtils, vg_utils, Controls, Dialogs,
  vg_scene, vg_objects, vg_controls, vg_treeview, vg_textbox, vg_listbox;

type

  TvgInspector = class(TvgTreeView)
  private
    FSelectedObject: TComponent;
    FEditBox: TvgTextBox;
    FComboBox: TvgPopupBox;
    FEditButton: TvgButton;
    FDivider: TvgShape;
    FDisabledProperties: TStrings;
    procedure RebuildList;
    procedure RebuildEditor;
    procedure SetSelectedObject(const Value: TComponent);
    procedure SetDisabledProperties(const Value: TStrings);
  protected
    function Editor: TvgControl;
    procedure AddObject(ItemRoot: TvgObject; Root: TObject);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetSelected(const Value: TvgTreeViewItem); override;
    procedure VScrollChange(Sender: TObject); override;
    procedure DoEditorChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property DisabledProperties: TStrings read FDisabledProperties write SetDisabledProperties;
    property SelectedObject: TComponent read FSelectedObject write SetSelectedObject;
  end;

implementation {===============================================================}

uses TypInfo, vg_dsgn, vg_dsgn_bmp, vg_dsgn_path;

{ TvgInspector }

constructor TvgInspector.Create(AOwner: TComponent);
begin
  inherited;
  FDisabledProperties := TStringList.Create;
  FDivider := TvgRectangle.Create(Self);
  FDivider.Stored := false;
  FDivider.ResourceName := 'inspectordivider';
  FDivider.Parent := Self;
  FDivider.SetBounds(120, 0, 2, Height);
  FEditButton := TvgButton.Create(Self);
  FEditButton.ResourceName := 'inspectorEditor';
  FEditButton.Resource := 'inspectorbuttonstyle';
  FEditButton.Visible := false;
  FEditButton.Stored := false;
  FEditButton.Parent := Self;
  FEditButton.OnClick := DoEditorChange;
  FEditButton.Visible := false;
  FEditBox := TvgTextBox.Create(Self);
  FEditBox.ResourceName := 'inspectorEditor';
  FEditBox.Padding.Rect := vgRect(0, 0, 0, 0);
  FEditBox.Resource := 'inspectoreditstyle';
  FEditBox.Stored := false;
  FEditBox.Parent := Self;
  FEditBox.OnChange := DoEditorChange;
  FEditBox.Visible := false;
  FComboBox := TvgPopupBox.Create(Self);
  FComboBox.ResourceName := 'inspectorEditor';
  FComboBox.Resource := 'inspectorpopupboxstyle';
  FComboBox.Stored := false;
  FComboBox.Parent := Self;
  FComboBox.TextAlign := vgTextAlignNear;
  FComboBox.OnChange := DoEditorChange;
  FComboBox.Visible := false;
  ClipChildren := true;
end;

destructor TvgInspector.Destroy;
begin
  FDisabledProperties.Free;
  inherited;
end;

procedure TvgInspector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TvgInspector.AddObject(ItemRoot: TvgObject; Root: TObject);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  i, j, PropCount: integer;
  Obj: TObject;
  Item: TvgTreeViewItem;
  Value: TvgLabel;
begin
  if Root = nil then Exit;
  {$IFDEF KS_COMPILER5}
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray], nil);
  GetMem(PropList, SizeOf(PPropInfo) * PropCount);
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray], PropList);
  {$ELSE}
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), PropList);
  {$ENDIF}
  if PropCount = 0 then Exit;

  try
    {$IFDEF KS_COMPILER6_UP}
    SortPropList(PropList, PropCount);
    {$ENDIF}
    for i := 0 to PropCount - 1 do
    begin
      with PropList[i]^ do
      begin
        if PropType^.Kind in [tkMethod] then Continue;
        if FDisabledProperties.IndexOf(Name) >= 0 then Continue;
        
        Item := TvgTreeViewItem.Create(Self);
        Item.Parent := ItemRoot;
        Item.Text := Name;
        Item.Locked := true;
        Item.Stored := false;
        Value := TvgLabel.Create(Self);
        Value.ResourceName := 'inspectorEditor';
        Value.Locked := true;
        Value.Stored := false;
        Value.Parent := Item;
        Value.SetBounds(FDivider.Position.X + FDivider.Width + 2, 0, Width, Item.Height + 2);
        Value.TextAlign := vgTextAlignNear;
        Value.WordWrap := false;
        Item.TagObject := Root;
        Item.TagString := Name;
        Item.Tag := Integer(Value);
        case PropType^.Kind of
          tkInteger:
            begin
              Value.Text := IntToStr(GetOrdProp(Root, Name));
            end;
          tkFloat:
            begin
              Value.Text := FloatToStr(GetFloatProp(Root, Name));
            end;
          tkClass:
            begin
              Obj := GetObjectProp(Root, Name);
              if Obj <> nil then
              begin
                Item.IsExpanded := false;
                TvgLabel(Value).Text := Obj.ClassName;
                if not (Obj is TvgObject) then
                  AddObject(Item, Obj);
              end;
            end;
           {$IFDEF KS_COMPILER11}tkUString, {$ENDIF}tkWString:
            begin
              {$IFDEF KS_COMPILER6_UP}
              Value.Text := GetWideStrProp(Root, Name);
              {$ELSE}
              Value.Text := GetStrProp(Root, Name);
              {$ENDIF}
            end;
          tkString, tkLString:
            begin
              Value.Text := GetStrProp(Root, Name);
            end;
          tkEnumeration:
            begin
              Value.Text := GetEnumProp(Root, Name);
            end;
          {$IFDEF FPC}
          tkBool:
            begin
              Value.Text := GetEnumProp(Root, Name);
            end;
          {$ENDIF}
          else
            Value.Text := GetStrProp(Root, Name);
        end;
      end;
    end;
  finally
    FreeMem(PropList, SizeOf(PPropInfo) * PropCount);
  end;
end;

procedure TvgInspector.RebuildList;
begin
  if Scene <> nil then Scene.BeginUpdate;
  BeginUpdate;
  FDisableAlign := true;
  FEditButton.TagObject := nil;
  FEditBox.TagObject := nil;
  FComboBox.TagObject := nil;
  Clear;
  if FSelectedObject <> nil then
    AddObject(Self, FSelectedObject);
  FDisableAlign := false;
  EndUpdate;
  if Scene <> nil then Scene.EndUpdateWOInvalidate;
  Repaint;
end;

procedure TvgInspector.RebuildEditor;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  Editor: TvgControl;
  i: integer;
begin
  if Scene <> nil then Scene.BeginUpdate;
  FDisableAlign := true;
  try
    FEditButton.Visible := false;
    FEditBox.Visible := false;
    FComboBox.Visible := false;
    if (Selected = nil) or (Selected.TagObject = nil) then Exit;

    PropInfo := GetPropInfo(Selected.TagObject, Selected.TagString);
    if PropInfo = nil then Exit;

    TypeData := GetTypeData(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF});
    if TypeData = nil then Exit;

    if FEditButton.TagObject <> nil then
      TvgVisualObject(FEditButton.TagObject).Visible := true;
    if FEditBox.TagObject <> nil then
      TvgVisualObject(FEditBox.TagObject).Visible := true;
    if FComboBox.TagObject <> nil then
      TvgVisualObject(FComboBox.TagObject).Visible := true;
    with PropInfo^ do
    begin
      case PropType^.Kind of
        tkMethod: Exit;
        tkClass:
          begin
            FEditButton.Text := 'Edit...';
            Editor := FEditButton;
          end;
        tkInteger:
          begin
            FEditBox.Text := IntToStr(GetOrdProp(Selected.TagObject, Selected.TagString));
            Editor := FEditBox;
          end;
        tkFloat:
          begin
            FEditBox.Text := FloatToStr(GetFloatProp(Selected.TagObject, Selected.TagString));
            Editor := FEditBox;
          end;
        tkWString:
          begin
            {$IFDEF KS_COMPILER6_UP}
            FEditBox.Text := GetWideStrProp(Selected.TagObject, Selected.TagString);
            {$ELSE}
            FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
            {$ENDIF}
            Editor := FEditBox;
          end;
        tkString, tkLString:
          begin
            FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
            FEditBox.SelectAll;
            Editor := FEditBox;
          end;
        tkEnumeration:
          begin
            FComboBox.Items.Clear;
            FComboBox.ItemIndex := -1;
            for i := TypeData.MinValue to TypeData.MaxValue do
              FComboBox.Items.Add(GetEnumName(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}, i));
            FComboBox.ItemIndex := GetOrdProp(Selected.TagObject, Selected.TagString);
            Editor := FComboBox;
          end;
        {$IFDEF FPC}
        tkBool:
          begin
            FComboBox.Items.Clear;
            for i := TypeData.MinValue to TypeData.MaxValue do
              FComboBox.Items.Add(GetEnumName(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}, i));
            FComboBox.ItemIndex := GetOrdProp(Selected.TagObject, Selected.TagString);
            Editor := FComboBox;
          end;
        {$ENDIF}
      else
        FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
        Editor := FEditBox;
      end;
    end;
    if Editor <> nil then
    begin
      Editor.Visible := true;
      Editor.Locked := true;
      Editor.Stored := false;
      Editor.TagObject := TObject(Selected.Tag);
      if Editor.TagObject <> nil then
        TvgVisualObject(Editor.TagObject).Visible := false;
      if VScrollBar.Visible then
        Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
          Width - FDivider.Position.X - FDivider.Width - VScrollBar.Width - FContent.Position.X, ItemHeight)
      else
        Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
          Width - FDivider.Position.X - FDivider.Width - FContent.Position.X, ItemHeight);
      if Selection <> nil then
        Selection.Width := FDivider.Position.X - Selection.Position.X - FContent.Position.X;
      Editor.SetFocus;
    end;
  finally
    FDisableAlign := false;
  end;
  if Scene <> nil then Scene.EndUpdateWOInvalidate;
  Repaint;
end;

procedure TvgInspector.DoEditorChange(Sender: TObject);
var
  Obj: TObject;
  F: TFontDialog;
  S: string;
  EditDlg: TvgPathDataDesigner;
begin
  if FDisableAlign then Exit;

  if (Selected <> nil) then
  begin
    if (Sender = FEditButton) and FEditButton.Visible then
    begin
      { Designer }
      Obj := GetObjectProp(Selected.TagObject, Selected.TagString);
      if Obj is TvgBrush then
      begin
        SelectInDesign(Obj, TPersistent(SelectedObject));
      end;
      if Obj is TvgBitmap then
      begin
        vgBitmapEditor := TvgBitmapEditor.Create(nil);
        vgBitmapEditor.AssignFromBitmap(TvgBitmap(Obj));
        if vgBitmapEditor.ShowModal = mrOk then
        begin
          vgBitmapEditor.AssignToBitmap(TvgBitmap(Obj));
        end;
        vgBitmapEditor.Free;
      end;
      if Obj is TvgPathData then
      begin
        EditDlg := TvgPathDataDesigner.Create(Self);
        EditDlg.PathData.Lines.Text := TvgPathData(Obj).Data;
        if EditDlg.ShowModal = mrOk then
        begin
          TvgPathData(Obj).Data := EditDlg.PathData.Lines.Text;
        end;
        EditDlg.Free;
      end;
      if Obj is TvgFont then
      begin
        F := TFontDialog.Create(nil);
        F.Font.Name := TvgFont(Obj).Family;
        F.Font.Height := -Abs(trunc(TvgFont(Obj).Size));
        if TvgFont(Obj).Style in [vgFontBold, vgFontBoldItalic] then
          F.Font.Style := F.Font.Style + [fsBold];
        if TvgFont(Obj).Style in [vgFontItalic, vgFontBoldItalic] then
          F.Font.Style := F.Font.Style + [fsItalic];
        if F.Execute then
        begin
          TvgFont(Obj).Family := F.Font.Name;
          TvgFont(Obj).Size := Abs(F.Font.Height);
          TvgFont(Obj).Style := vgFontRegular;
          if F.Font.Style = [fsBold] then
            TvgFont(Obj).Style := vgFontBold;
          if F.Font.Style = [fsItalic] then
            TvgFont(Obj).Style := vgFontItalic;
          if F.Font.Style = [fsBold, fsItalic] then
            TvgFont(Obj).Style := vgFontBoldItalic;
        end;
        F.Free;
      end;
    end;
    if (Sender = FEditBox) and FEditBox.Visible then
    begin
      SetPropValue(Selected.TagObject, Selected.TagString, TvgTextBox(Sender).Text);
      if TvgObject(Sender).TagObject <> nil then
        TvgLabel(TvgObject(Sender).TagObject).Text := TvgTextBox(Sender).Text;
    end;
    if (Sender = FComboBox) and FComboBox.Visible then
    begin
      SetPropValue(Selected.TagObject, Selected.TagString, TvgPopupBox(Sender).ItemIndex);
      if TvgLabel(TvgObject(Sender).TagObject).TagObject <> nil then
        TvgLabel(TvgObject(Sender).TagObject).Text := TvgPopupBox(Sender).Items[TvgPopupBox(Sender).ItemIndex];
    end; 
  end;
end;

procedure TvgInspector.SetSelectedObject(const Value: TComponent);
begin
  if FSelectedObject <> Value then
  begin
    FSelectedObject := Value;
    RebuildList;
    RebuildEditor;
  end;
end;

procedure TvgInspector.SetSelected(const Value: TvgTreeViewItem);
begin
  if Value <> Selected then
  begin
    inherited;
    RebuildEditor;
  end;
end;

procedure TvgInspector.Realign;
begin
  inherited;
  if (Selection <> nil) and (FDivider <> nil) then
    Selection.Width := FDivider.Position.X - Selection.Position.X;
  if FDivider <> nil then
    FDivider.SetBounds(120, 0, 2, Height);
  if (Selected <> nil) and (Editor <> nil) then
  begin
    if (VScrollBar <> nil) and VScrollBar.Visible then
      Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
        Width - FDivider.Position.X - FDivider.Width - VScrollBar.Width - FContent.Position.X, ItemHeight)
    else
      Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
        Width - FDivider.Position.X - FDivider.Width - FContent.Position.X, ItemHeight);
  end;
end;

procedure TvgInspector.SetDisabledProperties(const Value: TStrings);
begin
  FDisabledProperties.Assign(Value);
end;

function TvgInspector.Editor: TvgControl;
begin
  if FComboBox.Visible then
    Result := FComboBox
  else
  if FEditBox.Visible then
    Result := FEditBox
  else
    Result := FEditButton;
end;

procedure TvgInspector.VScrollChange(Sender: TObject);
begin
  inherited;
  if (Selected <> nil) and (Editor <> nil) then
  begin
    if (VScrollBar <> nil) and VScrollBar.Visible then
      Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
        Width - FDivider.Position.X - FDivider.Width - VScrollBar.Width - FContent.Position.X, ItemHeight)
    else
      Editor.SetBounds(FDivider.Position.X + FDivider.Width + FContent.Position.X, Selection.Position.Y + FContent.Position.Y,
        Width - FDivider.Position.X - FDivider.Width - FContent.Position.X, ItemHeight);
  end;
end;

initialization
  RegisterVGObjects('Design', [TvgInspector]);
end.


