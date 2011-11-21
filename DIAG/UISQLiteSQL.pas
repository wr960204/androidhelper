unit UISQLiteSQL;

{$I AndroidLanguage.inc} 

interface

uses
  Classes, SysUtils,Controls, Forms, vg_controls, vg_textbox, vg_scene,
  vg_objects, vg_memo, UIDialogBase, StringConsts,
  {$IFDEF CHS}UIChsConsts{$ELSE}UIEngConsts{$ENDIF};

type
  TAndroidUISQLiteSQL = class(TAndroidUIDialogBase)
  public
    // UI Elements
    mmSQL: TvgHudMemo;
    mmSQLResult: TvgHudMemo;
    btnSQLiteHelp: TvgHudButton;
    btnExecuteSQL: TvgHudButton;
    btnSQLOption: TvgHudButton;

    btnDump: TvgImage;
    btnTables: TvgImage;
    btnLoadScript: TvgImage;
  protected
    procedure InitUI; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  AndroidUISQLiteSQL: TAndroidUISQLiteSQL;

implementation

uses
  UI;

{ TAndroidUISQLiteSQL }

constructor TAndroidUISQLiteSQL.Create;
begin
  inherited;
  AndroidUISQLiteSQL := Self;
  SetSize(555, 360);
  SetScale(555, 360);
  FUI.KeyPreview := True;
  FUI.OnKeyDown := AndroidUI.Operation.UIKeyDownSQL;
end;

destructor TAndroidUISQLiteSQL.Destroy;
begin
  AndroidUISQLiteSQL := nil;
  inherited;
end;

procedure TAndroidUISQLiteSQL.InitUI;
var
  img: TResourceStream;
begin
  inherited;

  mmSQL:= TvgHudMemo.Create(Window);
  mmSQL.Parent := Window;
  mmSQL.Position.X := 24;
  mmSQL.Position.Y := 45;
  mmSQL.Width := 472;
  mmSQL.Height := 100;
  mmSQL.ScrollBars := ssVertical;
  mmSQL.Font.Style := vgFontBold;
  
  mmSQLResult:= TvgHudMemo.Create(Window);
  mmSQLResult.Parent := Window;
  mmSQLResult.Position.X := 24;
  mmSQLResult.Position.Y := 145;
  mmSQLResult.Width := 505;
  mmSQLResult.Height := 160;
  mmSQLResult.ScrollBars := ssVertical;
  mmSQLResult.Font.Style := vgFontBold;
  mmSQLResult.ReadOnly := True;

  btnTables:= TvgImage.Create(Window);
  btnTables.Parent := Window;
  btnTables.Position.X := 497;
  btnTables.Position.Y := 45;
  btnTables.Width := 32;
  btnTables.Height := 32;
  btnTables.ShowHint := True;
  btnTables.Hint := STR_HINT_TABLES;
  img := TResourceStream.Create(HInstance, IMG_U5, STR_PNG_EXT);
  btnTables.Bitmap.LoadFromStream(img);
  img.Free;
  btnTables.OnMouseEnter := AndroidUI.Operation.UISQLiteTablesEnter;
  btnTables.OnMouseLeave := AndroidUI.Operation.UISQLiteTablesLeave;
  btnTables.OnClick := AndroidUI.Operation.btnSQLiteTableClick;

  btnDump:= TvgImage.Create(Window);
  btnDump.Parent := Window;
  btnDump.Position.X := 497;
  btnDump.Position.Y := 78;
  btnDump.Width := 32;
  btnDump.Height := 32;
  btnDump.ShowHint := True;
  btnDump.Hint := STR_HINT_DUMP;
  img := TResourceStream.Create(HInstance, IMG_U7, STR_PNG_EXT);
  btnDump.Bitmap.LoadFromStream(img);
  img.Free;
  btnDump.OnMouseEnter := AndroidUI.Operation.UISQLiteDumpEnter;
  btnDump.OnMouseLeave := AndroidUI.Operation.UISQLiteDumpLeave;
  btnDump.OnClick := AndroidUI.Operation.btnSQLiteDumpClick;

  btnLoadScript:= TvgImage.Create(Window);
  btnLoadScript.Parent := Window;
  btnLoadScript.Position.X := 497;
  btnLoadScript.Position.Y := 111;
  btnLoadScript.Width := 32;
  btnLoadScript.Height := 32;
  btnLoadScript.ShowHint := True;
  btnLoadScript.Hint := STR_HINT_DUMP;
  img := TResourceStream.Create(HInstance, IMG_U9, STR_PNG_EXT);
  btnLoadScript.Bitmap.LoadFromStream(img);
  img.Free;
  btnLoadScript.OnMouseEnter := AndroidUI.Operation.UISQLiteLoadEnter;
  btnLoadScript.OnMouseLeave := AndroidUI.Operation.UISQLiteLoadLeave;
  btnLoadScript.OnClick := AndroidUI.Operation.btnSQLiteLoadClick;

  btnSQLiteHelp:= TvgHudButton.Create(Window);
  btnSQLiteHelp.Parent := Window;
  btnSQLiteHelp.Position.X := 24;
  btnSQLiteHelp.Position.Y := 315;
  btnSQLiteHelp.Width := 120;
  btnSQLiteHelp.Height := 22;
  btnSQLiteHelp.Font.Style := vgFontBold;
  btnSQLiteHelp.Text := STR_BTN_SQL_HELP;
  btnSQLiteHelp.OnClick := AndroidUI.Operation.btnSQLite3HelpClick;
  
  btnExecuteSQL:= TvgHudButton.Create(Window);
  btnExecuteSQL.Parent := Window;
  btnExecuteSQL.Position.X := 325;
  btnExecuteSQL.Position.Y := 315;
  btnExecuteSQL.Width := 205;
  btnExecuteSQL.Height := 22;
  btnExecuteSQL.Font.Style := vgFontBold;
  btnExecuteSQL.Text := STR_BTN_EXECUTE_SQL;
  btnExecuteSQL.OnClick := AndroidUI.Operation.btnSQLite3ExecuteClick;
  
  btnSQLOption:= TvgHudButton.Create(Window);
  btnSQLOption.Parent := Window;
  btnSQLOption.Position.X := 430;
  btnSQLOption.Position.Y := 315;
  btnSQLOption.Width := 100;
  btnSQLOption.Height := 22;
  btnSQLOption.Font.Style := vgFontBold;
  btnSQLOption.Text := STR_BTN_SQL_OPT;
  btnSQLOption.OnClick := AndroidUI.Operation.btnSQLite3OptionClick;
  btnSQLOption.Visible := False;
end;

end.
