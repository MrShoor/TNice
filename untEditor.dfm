object frmEditor: TfrmEditor
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frmEditor'
  ClientHeight = 256
  ClientWidth = 888
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  DesignSize = (
    888
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object mmCode: TMemo
    Left = 0
    Top = 0
    Width = 742
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'mmCode')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object mmSeparators: TMemo
    Left = 741
    Top = 0
    Width = 147
    Height = 256
    Anchors = [akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = [fsUnderline]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnChange = mmSeparatorsChange
    OnKeyDown = mmSeparatorsKeyDown
  end
  object ApplicationEvents1: TApplicationEvents
    OnDeactivate = ApplicationEvents1Deactivate
    Left = 432
    Top = 112
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 184
    Top = 72
  end
end
