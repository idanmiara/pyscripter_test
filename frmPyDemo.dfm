object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 289
  ClientWidth = 896
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object bRegPyFunc: TButton
    Left = 89
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Register PyFunc'
    TabOrder = 1
    OnClick = bRegPyFuncClick
  end
  object bPy5: TButton
    Left = 664
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Py5'
    TabOrder = 10
    OnClick = bPy5Click
  end
  object bPyScripter: TButton
    Left = 584
    Top = 8
    Width = 75
    Height = 25
    Caption = 'PyScripter'
    TabOrder = 5
    OnClick = bPyScripterClick
  end
  object bPyInit: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Init'
    TabOrder = 0
    OnClick = bPyInitClick
  end
  object mPy1: TMemo
    Left = 8
    Top = 80
    Width = 121
    Height = 137
    Lines.Strings = (
      'def main():'
      '    RunResult = 1'
      '    return RunResult')
    TabOrder = 11
  end
  object mPy5: TMemo
    Left = 664
    Top = 80
    Width = 212
    Height = 137
    Lines.Strings = (
      'from tkinter import *'
      'root = Tk()'
      'w = Label(root, text='#39'tkinter gui'#39')'
      'w.pack()'
      'root.mainloop()')
    TabOrder = 15
  end
  object mPy2: TMemo
    Left = 135
    Top = 80
    Width = 154
    Height = 137
    Lines.Strings = (
      'import pyt'
      ''
      'def main():'
      '    res = '#39'hello'#39
      '    pyt.ShowMessage(res)'
      '    return res'
      '')
    TabOrder = 12
  end
  object bPy2: TButton
    Left = 135
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Py2'
    TabOrder = 7
    OnClick = bPy2Click
  end
  object bPy1: TButton
    Left = 8
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Py1'
    TabOrder = 6
    OnClick = bPy1Click
  end
  object mOutput: TEdit
    Left = 135
    Top = 248
    Width = 211
    Height = 23
    ParentColor = True
    ReadOnly = True
    TabOrder = 16
  end
  object bPyLoad: TButton
    Left = 216
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Py Load'
    TabOrder = 2
    OnClick = bPyLoadClick
  end
  object mPy4: TMemo
    Left = 455
    Top = 80
    Width = 203
    Height = 137
    Lines.Strings = (
      'import pyt_ext'
      ''
      'def main():'
      '    res = pyt_ext.num2'
      '    return res')
    TabOrder = 14
  end
  object bPy4: TButton
    Left = 455
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Py4'
    TabOrder = 9
    OnClick = bPy4Click
  end
  object bPyRegConst: TButton
    Left = 325
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Register Consts'
    TabOrder = 3
    OnClick = bPyRegConstClick
  end
  object bPyRegExt: TButton
    Left = 436
    Top = 8
    Width = 133
    Height = 25
    Caption = 'Register Ext Module'
    TabOrder = 4
    OnClick = bPyRegExtClick
  end
  object mPy3: TMemo
    Left = 295
    Top = 80
    Width = 154
    Height = 137
    Lines.Strings = (
      'import pyt'
      ''
      'def main():'
      '    res = pyt.num'
      '    pyt.ShowMessage(res)'
      '    return res'
      '')
    TabOrder = 13
  end
  object bPy3: TButton
    Left = 295
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Py3'
    TabOrder = 8
    OnClick = bPy3Click
  end
end
