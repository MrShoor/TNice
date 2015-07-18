unit untEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AppEvnts, ExtCtrls;

type
  TfrmEditor = class(TForm)
    mmCode: TMemo;
    mmSeparators: TMemo;
    ApplicationEvents1: TApplicationEvents;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mmSeparatorsChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mmSeparatorsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSrc: TStringList;
    FSourceStr: string;
    FSrcWnd: HWND;
    FPrevSeparators: TStringList;
    procedure DoFormat(const Src, Sep: TStrings; const Dest: TStrings);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  public
    procedure SetOptions(const FontName: string; FontSize: Integer; FontColor: TColor; BackColor: TColor);

    property SrcWnd: HWND read FSrcWnd write FSrcWnd;
    property SourceStr: string read FSourceStr write FSourceStr;
    function ForamtedStr: string;
  end;

var
  frmEditor: TfrmEditor;

implementation

uses Math;

{$R *.dfm}

{ TfrmEditor }

procedure TfrmEditor.ApplicationEvents1Deactivate(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmEditor.DoFormat(const Src, Sep, Dest: TStrings);
  function GetSpaces(const n: Integer): string;
  var i: Integer;
  begin
    if n = 0 then
    begin
      Result := '';
      Exit;
    end;
    SetLength(Result, n);
    for i := 1 to n do
      Result[i] := ' ';
  end;
  function SplitStr(const src, separator: string; out before, after: string): Boolean;
  var n: Integer;
  begin
    n := Pos(separator, src);
    if n = 0 then
    begin
      Result := False;
      Exit;
    end;
    before := Copy(src, 1, n - 1);
    n := n + Length(separator);
    after := Copy(src, n, Length(src) - n + 1);
    Result := True;
  end;
var i, j, n, maxn: Integer;
    before, after: string;
    SrcCopy: TStringList;
begin
  Dest.Clear;
  SrcCopy := TStringList.Create;
  try
    SrcCopy.AddStrings(Src);
    for i := 0 to SrcCopy.Count - 1 do Dest.Add('');

    for j := 0 to Sep.Count - 1 do
    begin
      maxn := 0;
      for i := 0 to SrcCopy.Count - 1 do
      begin
        n := Pos(Sep.Strings[j], SrcCopy.Strings[i]);
        maxn := Max(maxn, n);
      end;

      if maxn > 0 then
      begin
        Dec(maxn);
        for i := 0 to SrcCopy.Count - 1 do
        begin
          if SplitStr(SrcCopy.Strings[i],  Sep.Strings[j], before, after) then
          begin
            SrcCopy.Strings[i] := after;
            before := before + GetSpaces(maxn - Length(before)) + Sep.Strings[j];
            Dest.Strings[i] := Dest.Strings[i] + before;
          end
          else
          begin
            Dest.Strings[i] := Dest.Strings[i] + SrcCopy.Strings[i];
            SrcCopy.Strings[i] := '';
          end;
        end;
      end;

      //debug output code
{
      AllocConsole;
      WriteLn('Sep: ', Sep.Strings[j]);
      WriteLn('Src');
      for i := 0 to SrcCopy.Count - 1 do
        Writeln(SrcCopy.Strings[i], '#');
      WriteLn('Dest');
      for i := 0 to Dest.Count - 1 do
        Writeln(Dest.Strings[i], '#');
      WriteLn('--------------');
}
    end;
    for i := 0 to SrcCopy.Count - 1 do
      Dest.Strings[i] := Dest.Strings[i] + SrcCopy.Strings[i];
  finally
    FreeAndNil(SrcCopy);
  end;
end;

function TfrmEditor.ForamtedStr: string;
var HasBreak: Boolean;
begin
  HasBreak := False;
  if Length(FSourceStr)>0 then
  begin
    HasBreak := (FSourceStr[Length(FSourceStr)] = #13);
    if not HasBreak and (Length(FSourceStr)>1) then
      HasBreak := (FSourceStr[Length(FSourceStr)-1] = #13) or (FSourceStr[Length(FSourceStr)] = #10);
  end;
  Result := mmCode.Text;
  if not HasBreak then
    if Length(Result) > 1 then
      if (Result[Length(Result)-1] = #13) and (Result[Length(Result)] = #10) then
        Delete(Result, Length(Result) - 1, 2);
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  FPrevSeparators := TStringList.Create;
  FSrc := TStringList.Create;
end;

procedure TfrmEditor.FormDeactivate(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPrevSeparators);
  FreeAndNil(FSrc);
end;

procedure TfrmEditor.FormShow(Sender: TObject);
begin
  FSrc.Text := FSourceStr;
  mmSeparators.Lines.BeginUpdate;
  try
    mmSeparators.Lines.Clear;
    mmSeparators.Lines.AddStrings(FPrevSeparators);
  finally
    mmSeparators.Lines.EndUpdate;
  end;
  mmSeparators.SetFocus;
  mmSeparators.SelectAll;
  DoFormat(FSrc, mmSeparators.Lines, mmCode.Lines);
  Left := -1;
end;

procedure TfrmEditor.mmSeparatorsChange(Sender: TObject);
begin
  mmCode.Lines.BeginUpdate;
  try
    DoFormat(FSrc, mmSeparators.Lines, mmCode.Lines);
  finally
    mmCode.Lines.EndUpdate;
  end;
end;

procedure TfrmEditor.mmSeparatorsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    FPrevSeparators.Clear;
    FPrevSeparators.AddStrings(mmSeparators.Lines);
    ModalResult := mrOk;
  end;
  if (Key = VK_ESCAPE) then
    ModalResult := mrCancel;
end;

procedure TfrmEditor.SetOptions(const FontName: string; FontSize: Integer;
  FontColor, BackColor: TColor);
begin
  mmCode.Font.Name := FontName;
  mmCode.Font.Size := FontSize;
  mmCode.Font.Color := FontColor;
  mmCode.Color := BackColor;

  mmSeparators.Font.Name := FontName;
  mmSeparators.Font.Size := FontSize;
  mmSeparators.Font.Color := FontColor;
  mmSeparators.Color := BackColor;
end;

procedure TfrmEditor.Timer1Timer(Sender: TObject);
begin
  if Visible then
    SetForegroundWindow(Handle);
end;

procedure TfrmEditor.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var SrcRct: TRect;
begin
  inherited;
  GetWindowRect(FSrcWnd, SrcRct);
  Message.WindowPos.x := SrcRct.Right - Message.WindowPos.cx;
  Message.WindowPos.y := SrcRct.Bottom - Message.WindowPos.cy;
end;

end.
