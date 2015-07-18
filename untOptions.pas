unit untOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, NiceTypes, ImgList, Generics.Collections,
  Spin, Menus, Registry;

const
  LAYOUT_RU = $419;
  LAYOUT_EN = $409;

  LAYOUT_RU_STR = '00000419';
  LAYOUT_EN_STR = '00000409';

  REG_Key = 'Software\TNice';
  REG_Value_FontName = 'FontName';
  REG_Value_FontSize = 'FontSize';
  REG_Value_FontColor = 'FontColor';
  REG_Value_BackColor = 'BackColor';
  REG_Value_EditorW = 'CodeEditorWidth';
  REG_Value_EditorH = 'CodeEditorHeight';
  REG_Value_Hotkey_SLang = 'HK_SwitchLang';
  REG_Value_Modifier_SLang = 'Mod_SwitchLang';
  REG_Value_Hotkey_SCase = 'HK_SwitchCase';
  REG_Value_Modifier_SCase = 'Mod_SwitchCase';
  REG_Value_Hotkey_Trans = 'HK_Trans';
  REG_Value_Modifier_Trans = 'Mod_Trans';
  REG_Value_Hotkey_CodeVAlign = 'HK_CodeVAlign';
  REG_Value_Modifier_CodeVAlign = 'Mod_CodeVAlign';


type
  TCurrentLang = (clEn, clRu);
  TCapsState = (csOff, csOn);

  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    hkSwitchLang: THotKey;
    Label1: TLabel;
    Label2: TLabel;
    hkSwitchReg: THotKey;
    Label3: TLabel;
    hkCodeAligner: THotKey;
    Label4: TLabel;
    hkTranslit: THotKey;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button3: TButton;
    imgSwitchLang: TImage;
    imgSwitchReg: TImage;
    imgTranslit: TImage;
    imgCodeAligner: TImage;
    imgHKList: TImageList;
    TrayIcon1: TTrayIcon;
    edFontSize: TSpinEdit;
    PopupMenu1: TPopupMenu;
    Ext1: TMenuItem;
    cbFont: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    imgFontColor: TImage;
    imgBackColor: TImage;
    ColorDialog1: TColorDialog;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure hkSwitchLangChange(Sender: TObject);
    procedure hkSwitchRegChange(Sender: TObject);
    procedure hkTranslitChange(Sender: TObject);
    procedure hkCodeAlignerChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIcon1Click(Sender: TObject);
    procedure Ext1Click(Sender: TObject);
    procedure cbFontDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edFontSizeChange(Sender: TObject);
    procedure imgFontColorClick(Sender: TObject);
    procedure imgBackColorClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FIsExit: Boolean;

    FLang       : array [TCurrentLang, TCapsState] of TDictionary<WideChar, WideChar>;
    FCaps       : TDictionary<WideChar, WideChar>;
    FTrans      : TDictionary<WideChar, String>;

    FNewEditorW : Integer;
    FNewEditorH : Integer;

    FRegistredHotkey: array [TFormatAction] of Boolean;
    procedure RegHK(const Action: TFormatAction; const Key: TShortCut; const Modifiers: THKModifiers);

    procedure SendCtrlKey(const VKey: Word);

    procedure DoSwitchLayout(h: HWND);
    function CanSwitchLayoutNow(h: HWND; out ActiveLayout: TCurrentLang): Boolean;

    function SwitchSymbols(const Dic: TDictionary<WideChar, WideChar>; const Src: string): string;
    function ReplaceSymbols(const Dic: TDictionary<WideChar, string>; const Src: string): string;
    function DoVerticalFormat(const SrcWnd: HWND; const Src: string; out FormatedStr: string): boolean;

    procedure ProcessBuffer(const action: TFormatAction);
    procedure WMHotkey(var msg: TWMHotKey); message WM_HOTKEY;

    procedure SetColor(img: TImage; color: TColor);
    function GetColor(img: TImage): TColor;
  public
    procedure SetDefaultSettings;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Clipbrd, untEditor, Math;

const HK_Base = 17;
const HK_ID: array [TFormatAction] of Cardinal = (HK_Base + 1, //faSwitchLang
                                                  HK_Base + 2, //faSwitchReg
                                                  HK_Base + 3, //faTranslit
                                                  HK_Base + 4);//faCodeVertical
const HK_IconIndex: array [Boolean] of Integer = (0, 1);

function EnumFontProc(var LogFont: TEnumLogFontEx; var Metrics: TEnumTextMetricW; FontType: Cardinal; Strings: TStrings): Integer; stdcall;
begin
  if (LogFont.elfLogFont.lfPitchAndFamily and FF_MODERN = FF_MODERN) then
    Strings.Add(LogFont.elfFullName);
  Result := 1;
end;

procedure GetFontList(str: TStrings);
var sl: TStringList;
    DC: HDC;
    lf: TLogFont;
begin
  sl := nil;
  if str = nil then Exit;
  str.Clear;
  DC := GetDC(0);
  try
    sl := TStringList.Create;
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;

    FillChar(lf, SizeOf(lf), 0);
    lf.lfPitchAndFamily := DEFAULT_PITCH or FF_MODERN;
    EnumFontFamiliesEx(DC, lf, @EnumFontProc, Integer(sl), 0);

    str.Assign(sl);
  finally
    FreeAndNil(sl);
    ReleaseDC(0, DC);
  end;
end;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  SetDefaultSettings;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  FIsExit := True;
  Close;
end;

function TfrmMain.CanSwitchLayoutNow(h: HWND; out ActiveLayout: TCurrentLang): Boolean;
var layouts: array of HKL;
//    layoutname: array [0..KL_NAMELENGTH-1] of WideChar;
    i:integer;
    hasEn, hasRu: boolean;
    currentLayout: HKL;
begin
//  SetLength(layouts, 1);
  SetLength(layouts, GetKeyboardLayoutList(0, layouts[0]));
  GetKeyboardLayoutList(length(layouts),layouts[0]);

  hasEn := False;
  hasRu := False;
  for i := 0 to Length(layouts) - 1 do
  begin
    hasEn := hasEn or (Word(layouts[i]) = LAYOUT_EN);
    hasRu := hasRu or (Word(layouts[i]) = LAYOUT_RU);
  end;

  currentLayout := GetKeyboardLayout(GetWindowThreadProcessId(h));
  if not (hasEn and hasRu) then Exit(False);
  if (Word(currentLayout) = LAYOUT_EN) then
  begin
    ActiveLayout := clEn;
    Exit(True);
  end;
  if (Word(currentLayout) = LAYOUT_RU) then
  begin
    ActiveLayout := clRu;
    Exit(True);
  end;
  Result := False;
end;

procedure TfrmMain.cbFontDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var s: string;
begin
  if Control = cbFont then
  begin
    cbFont.Canvas.Brush.Style := bsSolid;
    if odSelected in State then
      cbFont.Canvas.Brush.Color := clHighlight
    else
      cbFont.Canvas.Brush.Color := clWindow;
    cbFont.Canvas.FillRect(Rect);

    s := cbFont.Items[Index];
    cbFont.Canvas.Font.Name := s;
    cbFont.Canvas.Font.Size := edFontSize.Value;
    cbFont.Canvas.TextRect(Rect, s, [tfSingleLine, tfVerticalCenter]);
  end;
end;

procedure TfrmMain.DoSwitchLayout(h: HWND);
const KLF_SETFORPROCESS = $00000100;
var currentLayout: HKL;
begin
  currentLayout := GetKeyboardLayout(GetWindowThreadProcessId(h));
  if Word(currentLayout) = LAYOUT_EN then
  begin
    SendMessage(h, WM_INPUTLANGCHANGEREQUEST, 2,
                   LoadKeyboardLayout('00000419', KLF_ACTIVATE or KLF_SETFORPROCESS));
  end
  else
  begin
    SendMessage(h, WM_INPUTLANGCHANGEREQUEST, 2,
                   LoadKeyboardLayout('00000409', KLF_ACTIVATE or KLF_SETFORPROCESS));
  end;
end;

function TfrmMain.DoVerticalFormat(const SrcWnd: HWND; const Src: string; out FormatedStr: string): boolean;
begin
  if frmEditor.Visible then
  begin
    frmEditor.ModalResult := mrCancel;
    Result := False;
    Exit;
  end;

  frmEditor.SetOptions(cbFont.Text, edFontSize.Value, GetColor(imgFontColor), GetColor(imgBackColor));
  frmEditor.SrcWnd := SrcWnd;
  frmEditor.SourceStr := Src;
  frmEditor.Width := FNewEditorW;
  frmEditor.Height := FNewEditorH;
  if frmEditor.ShowModal = ID_OK then
  begin
    FormatedStr := frmEditor.ForamtedStr;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
  FNewEditorW := frmEditor.Width;
  FNewEditorH := frmEditor.Height;
end;

procedure TfrmMain.edFontSizeChange(Sender: TObject);
begin
  cbFont.ItemHeight := Max(12, (edFontSize.Value + 6));
  edFontSize.Font.Size := 8;
  edFontSize.Height := cbFont.Height;
  edFontSize.Font.Size := Trunc((edFontSize.Value - 8) * 0.5) + 8;
end;

procedure TfrmMain.Ext1Click(Sender: TObject);
begin
  FIsExit := True;
  Close;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FIsExit;
  if not FIsExit then Hide;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
  procedure LoadResToStringList(const sl: TStringList; const resname: string);
  var rs: TResourceStream;
  begin
    rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
    try
      sl.LoadFromStream(rs);
    finally
      FreeAndNil(rs);
    end;
  end;
  procedure LoadResToDic(const dic: TDictionary<WideChar, WideChar>; const resname: string); overload;
  var sl: TStringList;
      i: Integer;
  begin
    sl := TStringList.Create;
    try
      LoadResToStringList(sl, resname);
      for i := 0 to sl.Count - 1 do
        dic.Add(sl.Names[i][1], sl.ValueFromIndex[i][1]);
    finally
      FreeAndNil(sl);
    end;
  end;
  procedure LoadResToDic(const dic: TDictionary<WideChar, string>; const resname: string); overload;
  var sl: TStringList;
      i: Integer;
  begin
    sl := TStringList.Create;
    try
      LoadResToStringList(sl, resname);
      for i := 0 to sl.Count - 1 do
        dic.Add(sl.Names[i][1], sl.ValueFromIndex[i]);
    finally
      FreeAndNil(sl);
    end;
  end;
  procedure AddReverse(const Dic: TDictionary<WideChar,WideChar>);
  var pair: TPair<WideChar, WideChar>;
      lst: TList<TPair<WideChar, WideChar>>;
      i: Integer;
  begin
    lst := TList<TPair<WideChar, WideChar>>.Create;
    try
      for pair in Dic do lst.Add(Pair);
      for i := 0 to lst.Count - 1 do
        Dic.AddOrSetValue(lst.Items[i].Value, lst.Items[i].Key);
    finally
      FreeAndNil(lst);
    end;
  end;
begin
  FLang[clEn, csOff] := TDictionary<WideChar, WideChar>.Create;
  LoadResToDic(FLang[clEn, csOff], 'Lang_EnNoCaps');
  FLang[clEn, csOn] := TDictionary<WideChar, WideChar>.Create;
  LoadResToDic(FLang[clEn, csOn], 'Lang_EnCaps');
  FLang[clRu, csOff] := TDictionary<WideChar, WideChar>.Create;
  LoadResToDic(FLang[clRu, csOff], 'Lang_RuNoCaps');
  FLang[clRu, csOn] := TDictionary<WideChar, WideChar>.Create;
  LoadResToDic(FLang[clRu, csOn], 'Lang_RuCaps');

  FCaps := TDictionary<WideChar, WideChar>.Create;
  LoadResToDic(FCaps, 'Case');
  AddReverse(FCaps);

  FTrans := TDictionary<WideChar, String>.Create;
  LoadResToDic(FTrans, 'Translit');

  GetFontList(cbFont.Items);
  SetDefaultSettings;
  LoadSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var i: TFormatAction;
begin
  SaveSettings;
  for i := Low(TFormatAction) to High(TFormatAction) do
    if FRegistredHotkey[i] then
    begin
      UnregisterHotKey(Handle, HK_ID[i]);
      FRegistredHotkey[i] := False;
    end;

  FreeAndNil(FLang[clEn, csOff]);
  FreeAndNil(FLang[clEn, csOn]);
  FreeAndNil(FLang[clRu, csOff]);
  FreeAndNil(FLang[clRu, csOn]);
  FreeAndNil(FCaps);
  FreeAndNil(FTrans);
end;

function TfrmMain.GetColor(img: TImage): TColor;
begin
  Result := img.Picture.Bitmap.Canvas.Pixels[0, 0];
end;

procedure TfrmMain.hkCodeAlignerChange(Sender: TObject);
begin
  RegHK(faCodeVertical, hkCodeAligner.HotKey, hkCodeAligner.Modifiers);
  imgHKList.GetIcon(HK_IconIndex[FRegistredHotkey[faCodeVertical]], imgCodeAligner.Picture.Icon);
end;

procedure TfrmMain.hkSwitchLangChange(Sender: TObject);
begin
  RegHK(faSwitchLang, hkSwitchLang.HotKey, hkSwitchLang.Modifiers);
  imgHKList.GetIcon(HK_IconIndex[FRegistredHotkey[faSwitchLang]], imgSwitchLang.Picture.Icon);
end;

procedure TfrmMain.hkSwitchRegChange(Sender: TObject);
begin
  RegHK(faSwitchReg, hkSwitchReg.HotKey, hkSwitchReg.Modifiers);
  imgHKList.GetIcon(HK_IconIndex[FRegistredHotkey[faSwitchReg]], imgSwitchReg.Picture.Icon);
end;

procedure TfrmMain.hkTranslitChange(Sender: TObject);
begin
  RegHK(faTranslit, hkTranslit.HotKey, hkTranslit.Modifiers);
  imgHKList.GetIcon(HK_IconIndex[FRegistredHotkey[faTranslit]], imgTranslit.Picture.Icon);
end;

procedure TfrmMain.imgBackColorClick(Sender: TObject);
begin
  ColorDialog1.Color := GetColor(imgBackColor);
  if ColorDialog1.Execute then
    SetColor(imgBackColor, ColorDialog1.Color);
end;

procedure TfrmMain.imgFontColorClick(Sender: TObject);
begin
  ColorDialog1.Color := GetColor(imgFontColor);
  if ColorDialog1.Execute then
    SetColor(imgFontColor, ColorDialog1.Color);
end;

procedure TfrmMain.LoadSettings;
var reg: TRegistry;
    n: Integer;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(REG_Key) then
    begin
        reg.OpenKeyReadOnly(REG_Key);
        if reg.ValueExists(REG_Value_FontName) then
          if reg.GetDataType(REG_Value_FontName) = rdString then
          begin
            n := cbFont.Items.IndexOf(reg.ReadString(REG_Value_FontName));
            if n <> -1 then cbFont.ItemIndex := n;
          end;

        if reg.ValueExists(REG_Value_FontSize) then
          if reg.GetDataType(REG_Value_FontSize) = rdInteger then
            edFontSize.Value := reg.ReadInteger(REG_Value_FontSize);

        if reg.ValueExists(REG_Value_FontColor) then
          if reg.GetDataType(REG_Value_FontColor) = rdInteger then
            SetColor(imgFontColor, reg.ReadInteger(REG_Value_FontColor));

        if reg.ValueExists(REG_Value_BackColor) then
          if reg.GetDataType(REG_Value_BackColor) = rdInteger then
            SetColor(imgBackColor, reg.ReadInteger(REG_Value_BackColor));


        if reg.ValueExists(REG_Value_Hotkey_SLang) then
          if reg.GetDataType(REG_Value_Hotkey_SLang) = rdInteger then
            hkSwitchLang.HotKey := reg.ReadInteger(REG_Value_Hotkey_SLang);
        if reg.ValueExists(REG_Value_Modifier_SLang) then
          if reg.GetDataType(REG_Value_Modifier_SLang) = rdInteger then
            hkSwitchLang.Modifiers := THKModifiers(Byte(reg.ReadInteger(REG_Value_Modifier_SLang)));

        if reg.ValueExists(REG_Value_Hotkey_SCase) then
          if reg.GetDataType(REG_Value_Hotkey_SCase) = rdInteger then
            hkSwitchReg.HotKey := reg.ReadInteger(REG_Value_Hotkey_SCase);
        if reg.ValueExists(REG_Value_Modifier_SCase) then
          if reg.GetDataType(REG_Value_Modifier_SCase) = rdInteger then
            hkSwitchReg.Modifiers := THKModifiers(Byte(reg.ReadInteger(REG_Value_Modifier_SCase)));

        if reg.ValueExists(REG_Value_Hotkey_Trans) then
          if reg.GetDataType(REG_Value_Hotkey_Trans) = rdInteger then
            hkTranslit.HotKey := reg.ReadInteger(REG_Value_Hotkey_Trans);
        if reg.ValueExists(REG_Value_Modifier_Trans) then
          if reg.GetDataType(REG_Value_Modifier_Trans) = rdInteger then
            hkTranslit.Modifiers := THKModifiers(Byte(reg.ReadInteger(REG_Value_Modifier_Trans)));

        if reg.ValueExists(REG_Value_Hotkey_CodeVAlign) then
          if reg.GetDataType(REG_Value_Hotkey_CodeVAlign) = rdInteger then
            hkCodeAligner.HotKey := reg.ReadInteger(REG_Value_Hotkey_CodeVAlign);
        if reg.ValueExists(REG_Value_Modifier_CodeVAlign) then
          if reg.GetDataType(REG_Value_Modifier_CodeVAlign) = rdInteger then
            hkCodeAligner.Modifiers := THKModifiers(Byte(reg.ReadInteger(REG_Value_Modifier_CodeVAlign)));

        if reg.ValueExists(REG_Value_EditorW) then
          if reg.GetDataType(REG_Value_EditorW) = rdInteger then
            FNewEditorW := reg.ReadInteger(REG_Value_EditorW);

        if reg.ValueExists(REG_Value_EditorH) then
          if reg.GetDataType(REG_Value_EditorH) = rdInteger then
            FNewEditorH := reg.ReadInteger(REG_Value_EditorH);
    end;
  finally
    FreeAndNil(reg);
  end;
end;

procedure TfrmMain.ProcessBuffer(const action: TFormatAction);
  function GetCStr: string;
//  var c: TClipboard;
  begin
      try
        Result := Clipboard.AsText;
      except
        on e: EClipboardException do
        begin
          Result := '';
        end;
      end;
      {
      c := TClipboard.Create;
      try
        Result := c.AsText;
      finally
        c.Free;
      end;
      }
  end;

  procedure SetCStr(const s: string);
  var hMem: HGLOBAL;
      Data: Pointer;
  begin
    hMem := GlobalAlloc(GMEM_MOVEABLE, (Length(s)+1)*SizeOf(Char));
    if hMem <> 0 then
    begin
      Data := GlobalLock(hMem);
      if Assigned(Data) then
      try
        FillChar(Data^, (Length(s)+1)*SizeOf(Char), 0);
        Move(s[1], Data^, Length(s)*SizeOf(Char));
      finally
        GlobalUnlock(hMem);
      end
      else
        Exit;

      if OpenClipboard(0) then
      try
        EmptyClipboard();
        SetClipboardData(CF_UNICODETEXT, hMem);
      finally
        CloseClipboard();
      end;
    end;
  end;

var h: HWND;
    i: Integer;
    oldBuf, newBuf: string;
    wasFormated: boolean;
    formatedStr: string;
    UpdateID: Cardinal;
    ActiveLayout: TCurrentLang;
    CapsState: TCapsState;
begin
  h := GetForegroundWindow;
  if IsWindow(h) then
  begin
    if action = faSwitchLang then
      if not CanSwitchLayoutNow(h, ActiveLayout) then Exit;

    oldBuf := GetCStr;
    SetCStr('');
    UpdateID := GetClipboardSequenceNumber;
    try
      SendCtrlKey(Ord('C'));
      for i := 0 to 99 do
      begin
        if GetClipboardSequenceNumber > UpdateID then Break;
        Sleep(10);
      end;
      newBuf := GetCStr;
      if newBuf = '' then Exit;

      wasFormated := True;
      case action of
        faSwitchLang  : begin
                          if GetKeyState(VK_CAPITAL)=1 then
                            CapsState := csOn
                          else
                            CapsState := csOff;
                          newBuf := SwitchSymbols(FLang[ActiveLayout, CapsState], newBuf);
                          DoSwitchLayout(h);
                        end;
        faSwitchReg   : newBuf := SwitchSymbols(FCaps, newBuf);
        faTranslit    : newBuf := ReplaceSymbols(FTrans, newBuf);
        faCodeVertical: begin
                          wasFormated := DoVerticalFormat(h, newBuf, formatedStr);
                          if wasFormated then newBuf := formatedStr;
                        end;
      end;

      if wasFormated then
      begin
        SetCStr(newBuf);
        SetForegroundWindow(h);
        SendCtrlKey(Ord('V'));
        Sleep(500);
      end;
    finally
      SetForegroundWindow(h);
      SetCStr(oldBuf);
    end;
  end;
end;

procedure TfrmMain.RegHK(const Action: TFormatAction; const Key: TShortCut; const Modifiers: THKModifiers);
var M: Cardinal;
begin
  if FRegistredHotkey[Action] then
  begin
    UnregisterHotKey(Handle, HK_ID[Action]);
    FRegistredHotkey[Action] := False;
  end;
  M := 0;
  if hkAlt in Modifiers then M := M or MOD_ALT;
  if hkCtrl in Modifiers then M := M or MOD_CONTROL;
  if hkShift in Modifiers then M := M or MOD_SHIFT;
  if hkExt in Modifiers then M := M or MOD_WIN;
  if M = 0 then Exit;
  if Key = 0 then Exit;
  if Key = $8000 then Exit;
  FRegistredHotkey[Action] := RegisterHotKey(Handle, HK_ID[Action], M, LoByte(Key));
end;

function TfrmMain.ReplaceSymbols(const Dic: TDictionary<WideChar, string>; const Src: string): string;
var i: Integer;
    s: string;
begin
  Result := '';
  for i := 1 to Length(src) do
    if Dic.TryGetValue(src[i], s) then
      Result := Result + s
    else
      Result := Result + src[i];
end;

procedure TfrmMain.SaveSettings;
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(REG_Key, True) then
    begin
      reg.WriteString(REG_Value_FontName, cbFont.Text);
      reg.WriteInteger(REG_Value_FontSize, edFontSize.Value);
      reg.WriteInteger(REG_Value_FontColor, GetColor(imgFontColor));
      reg.WriteInteger(REG_Value_BackColor, GetColor(imgBackColor));

      reg.WriteInteger(REG_Value_Hotkey_SLang, hkSwitchLang.HotKey);
      reg.WriteInteger(REG_Value_Modifier_SLang, Byte(hkSwitchLang.Modifiers));
      reg.WriteInteger(REG_Value_Hotkey_SCase, hkSwitchReg.HotKey);
      reg.WriteInteger(REG_Value_Modifier_SCase, Byte(hkSwitchReg.Modifiers));
      reg.WriteInteger(REG_Value_Hotkey_Trans, hkTranslit.HotKey);
      reg.WriteInteger(REG_Value_Modifier_Trans, Byte(hkTranslit.Modifiers));
      reg.WriteInteger(REG_Value_Hotkey_CodeVAlign, hkCodeAligner.HotKey);
      reg.WriteInteger(REG_Value_Modifier_CodeVAlign, Byte(hkCodeAligner.Modifiers));

      reg.WriteInteger(REG_Value_EditorW, FNewEditorW);
      reg.WriteInteger(REG_Value_EditorH, FNewEditorH);
    end;
  finally
    FreeAndNil(reg);
  end;
end;

procedure TfrmMain.SendCtrlKey(const VKey: Word);
  procedure SendKeyInput(const VKey: Word; const UpEvent: Boolean);
  var input: TInput;
  begin
    input.Itype := INPUT_KEYBOARD;
    input.ki.wScan := 0;
    input.ki.time := 0;
    input.ki.dwExtraInfo := 0;
    input.ki.wVk := VKey;
    if UpEvent then input.ki.dwFlags := KEYEVENTF_KEYUP else input.ki.dwFlags := 0;
    SendInput(1, input, SizeOf(input));
  end;
var RC, LC: Boolean;
    RS, LS: Boolean;
begin
  LS := GetAsyncKeyState(VK_LSHIFT) < 0;
  RS := GetAsyncKeyState(VK_RSHIFT) < 0;
  LC := GetAsyncKeyState(VK_LCONTROL) < 0;
  RC := GetAsyncKeyState(VK_RCONTROL) < 0;
  if LS then SendKeyInput(VK_LSHIFT, True);
  if RS then SendKeyInput(VK_RSHIFT, True);
  if LC then SendKeyInput(VK_LCONTROL, True);
  if RC then SendKeyInput(VK_RCONTROL, True);

  SendKeyInput(VK_LCONTROL, False);
  SendKeyInput(VKey, False);
  SendKeyInput(VKey, True);
  SendKeyInput(VK_LCONTROL, True);

  if LS then SendKeyInput(VK_LSHIFT, False);
  if RS then SendKeyInput(VK_RSHIFT, False);
  if LC then SendKeyInput(VK_LCONTROL, False);
  if RC then SendKeyInput(VK_RCONTROL, False);
end;

procedure TfrmMain.SetColor(img: TImage; color: TColor);
begin
  img.Picture.Bitmap.Canvas.Pixels[0, 0] := color;
end;

procedure TfrmMain.SetDefaultSettings;
var n: Integer;
begin
  hkSwitchLang.HotKey := VK_PAUSE;
  hkSwitchLang.Modifiers := [hkShift];
  hkSwitchLang.OnChange(hkSwitchLang);

  hkSwitchReg.HotKey := VK_SCROLL;
  hkSwitchReg.Modifiers := [hkShift];
  hkSwitchReg.OnChange(hkSwitchReg);

  hkTranslit.HotKey := VK_SCROLL;
  hkTranslit.Modifiers := [hkCtrl, hkShift];
  hkTranslit.OnChange(hkTranslit);

  hkCodeAligner.HotKey := Ord('D');
  hkCodeAligner.Modifiers := [hkCtrl, hkShift];
  hkCodeAligner.OnChange(hkCodeAligner);

  n := cbFont.Items.IndexOf('Consolas');
  if n = -1 then n := cbFont.Items.IndexOf('Courier New');
  if n = -1 then n := 0;
  cbFont.ItemIndex := n;

  imgFontColor.Picture.Bitmap.PixelFormat := pf24bit;
  imgFontColor.Picture.Bitmap.Width := 1;
  imgFontColor.Picture.Bitmap.Height := 1;
  imgFontColor.Picture.Bitmap.Canvas.Pixels[0,0] := clBlack;

  imgBackColor.Picture.Bitmap.PixelFormat := pf24bit;
  imgBackColor.Picture.Bitmap.Width := 1;
  imgBackColor.Picture.Bitmap.Height := 1;
  imgBackColor.Picture.Bitmap.Canvas.Pixels[0,0] := clWhite;

  FNewEditorW := 750;
  FNewEditorH := 250;
end;

function TfrmMain.SwitchSymbols(const Dic: TDictionary<WideChar, WideChar>; const Src: string): string;
var i: Integer;
begin
  SetLength(Result, Length(Src));
  for i := 1 to Length(src) do
    if not Dic.TryGetValue(src[i], Result[i]) then Result[i] := src[i];
end;

procedure TfrmMain.TrayIcon1Click(Sender: TObject);
begin
  Show;
  SetForegroundWindow(Handle);
end;

procedure TfrmMain.WMHotkey(var msg: TWMHotKey);
var act: TFormatAction;
begin
  case msg.HotKey of
    HK_Base + 1: act := faSwitchLang;
    HK_Base + 2: act := faSwitchReg;
    HK_Base + 3: act := faTranslit;
    HK_Base + 4: act := faCodeVertical;
  else
    Exit;
  end;
  ProcessBuffer(act);
end;

end.
