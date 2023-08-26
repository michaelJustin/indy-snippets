object ExampleForm: TExampleForm
  Left = 0
  Top = 0
  Caption = 'Request - Reply'
  ClientHeight = 559
  ClientWidth = 1120
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 30
  object Label1: TLabel
    Left = 24
    Top = 40
    Width = 299
    Height = 30
    Caption = 'Client side: TMyTCPClientThread'
  end
  object Label2: TLabel
    Left = 576
    Top = 40
    Width = 243
    Height = 30
    Caption = 'Server side: TMyTCPServer'
  end
  object ClientLog: TMemo
    Left = 24
    Top = 149
    Width = 497
    Height = 375
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ServerLog: TMemo
    Left = 576
    Top = 149
    Width = 497
    Height = 380
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object ClientSendMessage: TButton
    Left = 24
    Top = 88
    Width = 225
    Height = 44
    Caption = 'Send request'
    TabOrder = 2
    OnClick = ClientSendMessageClick
  end
  object ServerSendMessage: TButton
    Left = 576
    Top = 88
    Width = 225
    Height = 44
    Caption = 'Send request'
    TabOrder = 3
    OnClick = ServerSendMessageClick
  end
  object ClientRequest: TEdit
    Left = 255
    Top = 93
    Width = 266
    Height = 38
    TabOrder = 4
    Text = 'Hello, server!'
  end
  object ServerRequest: TEdit
    Left = 807
    Top = 93
    Width = 266
    Height = 38
    TabOrder = 5
    Text = 'Hello, client!'
  end
end
