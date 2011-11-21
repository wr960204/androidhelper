unit UIDialogBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, vg_scene, vg_controls, vg_layouts, vg_tabcontrol, vg_textbox,
  vg_objects, vg_colors, vg_memo, StringConsts, vg_listbox, vg_extctrls,
  UIDirectAlias;

type
  TAndroidUIDialogBase = class
  public
    Scene: AndroidDXUI;
    Root : TvgBackground;
    Window: TvgHudWindow;

    // UI Elements

  protected
    procedure InitUI; virtual;
  public
    FUI: TypeDXUI;
    constructor Create; virtual;
    constructor CreateApplication; virtual;
    destructor Destroy; override;
    function Execute: Cardinal; virtual;
    procedure SetCaption(ACaption: string);
    procedure SetSize(Width, Height: Integer);
    procedure SetScale(Width, Height: Integer);
  end;

implementation

uses
  UI;

{ TAndroidUIDialogBase }

constructor TAndroidUIDialogBase.Create;
begin
  FUI:= TypeDXUI.Create(Application);
  FUI.BorderStyle := bsNone;
  FUI.Position := poMainFormCenter;
  InitUI;
end;

constructor TAndroidUIDialogBase.CreateApplication;
begin
  Application.CreateForm(TypeDXUI, FUI);
  FUI.BorderStyle := bsNone;
  FUI.Position := poScreenCenter;
  FUI.ModalResult := mrNone;
  InitUI;

end;

destructor TAndroidUIDialogBase.Destroy;
begin
  FUI.Free;
  inherited;
end;

function TAndroidUIDialogBase.Execute: Cardinal;
begin
  Result := FUI.ShowModal;
end;

procedure TAndroidUIDialogBase.InitUI;
begin
  Scene:= AndroidDXUI.Create(FUI);
  Scene.Parent := FUI;
  Scene.Align := alClient;
  Scene.Transparency := True;
  Root := TvgBackground.Create(Scene);
  Root.Align := vaClient;

  Scene.AddObject(Root);
  Scene.AllowDrag := True;
  root.HitTest := False;
  Window := TvgHudWindow.Create(Root);
  Window.Parent := Root;
  Window.Align := vaClient;
  Window.HitTest := False;
  Window.TextAlign := vgTextAlignCenter;
  Window.ButtonAlign := vgButtonAlignRight;
  Window.ShowSizeGrip := False;
  Window.Font.Style := vgFontBold;
  Window.ShowCaption := True;
  Window.ShowCloseButton := True;
end;

procedure TAndroidUIDialogBase.SetCaption(ACaption: string);
begin
  Window.Text := ACaption;
end;

procedure TAndroidUIDialogBase.SetScale(Width, Height: Integer);
begin
  Window.Fill.SolidColor := HSLtoRGB(AndroidUI.Quad.Hue, AndroidUI.Quad.Sat, AndroidUI.Quad.Lum);
  Window.Opacity := AndroidUI.tbTransparent.Value;
  Root.Scale.X := AndroidUI.Config.UIScale;
  Root.Scale.Y := AndroidUI.Config.UIScale;
  FUI.Width := Trunc(Width * AndroidUI.Config.UIScale);
  FUI.Height := Trunc(Height * AndroidUI.Config.UIScale);
end;

procedure TAndroidUIDialogBase.SetSize(Width, Height: Integer);
begin
  FUI.Width := Width;
  FUI.Height := Height;
  Root.Width := Width;
  Root.Height := Height;
end;

end.
