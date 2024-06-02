unit frmPyDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  PythonEngine, PythonVersions;

type
  TForm1 = class(TForm)
    bRegPyFunc: TButton;
    bPy5: TButton;
    bPyScripter: TButton;
    bPyInit: TButton;
    mPy1: TMemo;
    mPy5: TMemo;
    mPy2: TMemo;
    bPy2: TButton;
    bPy1: TButton;
    mOutput: TEdit;
    bPyLoad: TButton;
    mPy4: TMemo;
    bPy4: TButton;
    bPyRegConst: TButton;
    bPyRegExt: TButton;
    mPy3: TMemo;
    bPy3: TButton;
    procedure bPyScripterClick(Sender: TObject);
    procedure bPy5Click(Sender: TObject);
    procedure bRegPyFuncClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bPyLoadClick(Sender: TObject);
    procedure bPy2Click(Sender: TObject);
    procedure bPy1Click(Sender: TObject);
    procedure bPyInitClick(Sender: TObject);
    procedure bPy4Click(Sender: TObject);
    procedure bPyRegConstClick(Sender: TObject);
    procedure bPyRegExtClick(Sender: TObject);
    procedure bPy3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Run_TempScript(m: TStrings): String;
  end;


type
  TPyScriptFunctions = class
  private
    PyModule: TPythonModule;
    PyConst_names: TStringList;
    PyConst_values: array of variant;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFunction0(
      const FuncGroup: string;
      Func_py: PyCFunction;
      const FuncName, FuncNameAndParameters, Description: string); overload;
    procedure RegisterPyConst(name: string; value: PPyObject);
    function PyLoad: Boolean;
    function TryLoadPythonDLL(PythonHome: String): Boolean;
    procedure RegisterConst(name: string; pas_value: variant);
    procedure RegisterPyPath(path: String);
    procedure DoRegister;
    procedure DoRegister2;
    procedure DoRegisterExt;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses pyscripter_launch;

var
  PyEngine: TPythonEngine = nil;
  PyModule: TPythonModule;
  PythonPath: string;
  PythonScriptsDir: string;
  pyf: TPyScriptFunctions = nil;

const
  EOL: string = #$0D + #$0A;
  PythonVersion = 0;
  PythonHome: string = '';


procedure ErrorMsg(m: string);
begin
   ShowMessage('err: '+m);
end;

function Script_GetStringArg(args: PPyObject; var s: string): PPyObject;
var
  t0: PPyObject;
begin
  Result := nil;
  if pyEngine.PyArg_ParseTuple(args, 'O', @t0)=0 then exit;
  s := pyEngine.PyObjectAsString(t0);
  if pyEngine.PyErr_Occurred() <> nil then exit;
  Result := pyEngine.ReturnNone;
end;

function Script_ShowMessage_py(self, args: PPyObject): PPyObject; cdecl;
var
  s: string;
begin
  Result := Script_GetStringArg(args, s);
  if Result<>nil then
    ShowMessage(s);
end;

function PyScriptIsSuccessful: Boolean;
var
  exc_type, exc_val, exc_tb: PPyObject;
begin
  Result := pyEngine.PyErr_Occurred = nil;
  if not Result then
  begin
    pyEngine.PyErr_Fetch(exc_type, exc_val, exc_tb);
    ShowMessage(pyEngine.PyObjectAsString(exc_type) + ': ' + pyEngine.PyObjectAsString(exc_val)+EOL+
    'line number:'+pyEngine.PyObjectAsString(pyEngine.PyObject_GetAttrString(exc_tb,'tb_lineno')));
    pyEngine.PyErr_Clear;
  end;
end;

function RunScript_py(ScriptLines: TStrings; var ResultStr: String): boolean;
var
  d, args, foo, res: PPyObject;
  __name__: String;
begin
  Result := false;
  try
    d := pyEngine.PyDict_New();
    pyEngine.PyDict_SetItemString(d, '__builtins__', pyEngine.PyEval_GetBuiltins);
    pyEngine.PyDict_SetItemString(d, '__annotations__', pyEngine.PyDict_New());
    pyEngine.PyDict_SetItemString(d, '__doc__',
    pyEngine.PyUnicodeFromString('define a no-argument function called main here'));
    pyEngine.PyDict_SetItemString(d, '__package__', pyEngine.ReturnNone);
    pyEngine.PyDict_SetItemString(d, '__spec__', pyEngine.ReturnNone);

    __name__ := '__main__'; // this is the accepted name for the main script, normal usage: if __name__ == '__main__':
    pyEngine.PyDict_SetItemString(d, '__name__', pyEngine.PyUnicodeFromString(__name__));

    // d := pyEngine.PyEval_GetGlobals;
    //pyEngine.ExecString(AnsiString(ScriptLines.GetText), d, d);
    pyEngine.ExecStrings(ScriptLines, d, d);

    //pyEngine.PyDict_SetItemString(d, '__name__', pyEngine.PyUnicodeFromString('main_func'));
    //pyEngine.ExecStrings(ScriptLines, d, d);
    foo := pyEngine.PyMapping_GetItemString(d, 'main');
    if foo <> nil then
    begin
      args := pyEngine.Py_BuildValue('()');
      res := pyEngine.PyObject_Call(foo, args, nil);
      if PyScriptIsSuccessful then
      begin
        Result := True;
        if res<>pyEngine.Py_None then
          ResultStr := pyEngine.PyObjectAsString(res)
        else
          ResultStr := '';
        pyEngine.PyErr_Clear;
      end;
    end
    else begin
      pyEngine.PyErr_Clear;
    end;
  finally
  end;
end;

constructor TPyScriptFunctions.Create;
begin
  PyModule := TPythonModule.Create(nil);

  PyModule.Name := 'PyModule';
  PyModule.Engine := PyEngine;
  PyModule.ModuleName := 'pyt';

  PyConst_names := TStringList.Create;
  SetLength(PyConst_values,0);
end;

destructor TPyScriptFunctions.Destroy;
begin
  PyConst_names.Free;
  PyConst_values := nil;
  PyEngine.Free;
  PyEngine := nil;
  PyModule.Free;
  inherited;
end;

procedure TPyScriptFunctions.DoRegister;
begin
  RegisterFunction0('General',Script_ShowMessage_py, 'ShowMessage', 'ShowMessage (Message: String)', 'Show the message on the screen');
end;

procedure TPyScriptFunctions.DoRegister2;
var
  i: Integer;
  name : string;
  val : variant;
  temp0 : PPYobject;
begin
  RegisterConst('num',42);
  for i := 0 to pyconst_names.Count-1 do begin
    name := pyconst_names[i];
    val := PyConst_values[i];
    temp0 := PyEngine.VariantAsPyObject(val);
    PyModule.SetVar(UTF8Encode(name), temp0);
  end;
end;

procedure TPyScriptFunctions.DoRegisterExt;
begin
  RegisterPyPath(PythonScriptsDir);
end;

function TPyScriptFunctions.TryLoadPythonDLL(PythonHome: String): Boolean;
begin
  Result := False;
  PyEngine.FatalAbort := False;
  PyEngine.FatalMsgDlg := False;
  PyEngine.UseLastKnownVersion := PyEngine.DllName='';
  try
    PyEngine.SetPythonHome(PythonHome);
    PyEngine.LoadDll;
    Result := PyEngine.IsHandleValid;
    if not Result then
      ErrorMsg('Python Handle is Invalid');
  except
    ErrorMsg('Python Faild to initalize');
  end;
end;

function TPyScriptFunctions.PyLoad: Boolean;
var
  pyVersion: TPythonVersion;
begin
  Result := False;
  if PythonVersion=0 then
    PyEngine.DllName := ''
  else
    PyEngine.DllName := 'python'+IntToStr(PythonVersion)+'.dll';
  if PythonPath<>'' then begin
    PyEngine.DllPath := PythonPath;
    Result := TryLoadPythonDLL(PythonHome);
  end;

  if not Result and PythonVersionFromPath(PythonPath, pyVersion) then begin
    pyVersion.AssignTo(PyEngine);
    Result := TryLoadPythonDLL(PythonHome);
  end;
  if not Result and PythonVersions.GetLatestRegisteredPythonVersion(pyVersion) then begin
    // try the latest system registered python
    pyVersion.AssignTo(PyEngine);
    Result := TryLoadPythonDLL('');
  end;
  if not Result then begin
    //try the default python
    PyEngine.DllName := '';
    PyEngine.DllPath := '';
    Result := TryLoadPythonDLL('');
    if not Result then exit;
  end;
  PythonPath := PyEngine.DllPath;
end;

procedure TPyScriptFunctions.RegisterConst(name: string; pas_value: variant);
begin
  PyConst_names.Add(name);
  SetLength(PyConst_values, Length(PyConst_values)+1);
  PyConst_values[Length(PyConst_values)-1] := pas_value;
end;

var
  PyScriptFuncNames: array of RawByteString = nil;
procedure TPyScriptFunctions.RegisterFunction0(
  const FuncGroup: string;
  Func_py: PyCFunction;
  const FuncName, FuncNameAndParameters, Description: string);
var
  Doc:string;
  i: integer;
begin
  if Assigned(Func_py) then begin
    Doc:=FuncNameAndParameters+EOL+Description;
    i := Length(PyScriptFuncNames);
    SetLength(PyScriptFuncNames, i+2);
    PyScriptFuncNames[i] := UTF8Encode(FuncName);
    PyScriptFuncNames[i+1] := UTF8Encode(Doc);
    Self.PyModule.AddMethod(PAnsiChar(PyScriptFuncNames[i]),Func_py,PAnsiChar(PyScriptFuncNames[i+1]));
  end;
end;

procedure TPyScriptFunctions.RegisterPyConst(name: string; value: PPyObject);
begin
  PyModule.SetVar(UTF8Encode(name), value);
end;

procedure TPyScriptFunctions.RegisterPyPath(path: String);
var
  obj1, obj2: PPyObject;
begin
  obj1 := pyEngine.PySys_GetObject('path');
  obj2 := pyEngine.PyUnicodeFromString(path);
  pyEngine.PyList_Append(obj1, obj2);
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  PythonScriptsDir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + '..\..\scripts';
end;

procedure TForm1.bPyInitClick(Sender: TObject);
begin
  if PyEngine = nil then
    PyEngine := TPythonEngine.Create(nil);
  if pyf = nil then
    pyf := TPyScriptFunctions.Create;
end;

procedure TForm1.bRegPyFuncClick(Sender: TObject);
begin
  bPyInitClick(Sender);
  pyf.DoRegister;
end;

procedure TForm1.bPyLoadClick(Sender: TObject);
begin
  bPyInitClick(Sender);
  pyf.PyLoad;
end;

procedure TForm1.bPyRegConstClick(Sender: TObject);
begin
  pyf.DoRegister2;
end;

procedure TForm1.bPyRegExtClick(Sender: TObject);
begin
  pyf.DoRegisterExt;
end;

procedure TForm1.bPy1Click(Sender: TObject);
begin
  Run_TempScript(mPy1.Lines);
end;

procedure TForm1.bPy2Click(Sender: TObject);
begin
  Run_TempScript(mPy2.Lines);
end;

procedure TForm1.bPy3Click(Sender: TObject);
begin
  Run_TempScript(mPy3.Lines);
end;

procedure TForm1.bPy4Click(Sender: TObject);
begin
  Run_TempScript(mPy4.Lines);
end;

procedure TForm1.bPy5Click(Sender: TObject);
begin
  Run_TempScript(mPy5.Lines);
end;

function TForm1.Run_TempScript(m: TStrings): String;
begin
  Result := '';
  try
    if RunScript_py(m, Result) then
      mOutput.Text := Result;
  except
    on E: Exception do
      ErrorMsg(E.ToString);
  end;
end;

procedure TForm1.bPyScripterClick(Sender: TObject);
begin
  pyscripter_init;
  pyscripter_show;
  pyengine := pyscripter_engine;
end;

end.
