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
    cbThreads: TCheckBox;
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
    procedure cbThreadsClick(Sender: TObject);
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
    constructor Create(_PyEngine: TPythonEngine);
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

uses pyscripter_launch, cInternalPython;

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


procedure MyCreateExternalModules(_PyEngine: TPythonEngine);
begin
  if pyf = nil then begin
    pyf := TPyScriptFunctions.Create(_PyEngine);
    pyf.DoRegister;
  end;
end;

procedure ErrorMsg(m: string);
begin
   ShowMessage('err: '+m);
end;

function Script_GetStringArg(args: PPyObject; var s: string): PPyObject;
var
  t0: PPyObject;
begin
  Result := nil;
  if PyEngine.PyArg_ParseTuple(args, 'O', @t0)=0 then exit;
  s := PyEngine.PyObjectAsString(t0);
  if PyEngine.PyErr_Occurred() <> nil then exit;
  Result := PyEngine.ReturnNone;
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
  Result := PyEngine.PyErr_Occurred = nil;
  if not Result then
  begin
    PyEngine.PyErr_Fetch(exc_type, exc_val, exc_tb);
    ShowMessage(PyEngine.PyObjectAsString(exc_type) + ': ' + PyEngine.PyObjectAsString(exc_val)+EOL+
    'line number:'+PyEngine.PyObjectAsString(PyEngine.PyObject_GetAttrString(exc_tb,'tb_lineno')));
    PyEngine.PyErr_Clear;
  end;
end;

function RunScript_py(ScriptLines: TStrings; var ResultStr: String): boolean;
var
  d, args, foo, res: PPyObject;
  __name__: String;
begin
  Result := false;
  try
    d := PyEngine.PyDict_New();
    PyEngine.PyDict_SetItemString(d, '__builtins__', PyEngine.PyEval_GetBuiltins);
    PyEngine.PyDict_SetItemString(d, '__annotations__', PyEngine.PyDict_New());
    PyEngine.PyDict_SetItemString(d, '__doc__',
    PyEngine.PyUnicodeFromString('define a no-argument function called main here'));
    PyEngine.PyDict_SetItemString(d, '__package__', PyEngine.ReturnNone);
    PyEngine.PyDict_SetItemString(d, '__spec__', PyEngine.ReturnNone);

    __name__ := '__main__'; // this is the accepted name for the main script, normal usage: if __name__ == '__main__':
    PyEngine.PyDict_SetItemString(d, '__name__', PyEngine.PyUnicodeFromString(__name__));

    // d := PyEngine.PyEval_GetGlobals;
    //PyEngine.ExecString(AnsiString(ScriptLines.GetText), d, d);
    PyEngine.ExecStrings(ScriptLines, d, d);

    //PyEngine.PyDict_SetItemString(d, '__name__', PyEngine.PyUnicodeFromString('main_func'));
    //PyEngine.ExecStrings(ScriptLines, d, d);
    foo := PyEngine.PyMapping_GetItemString(d, 'main');
    if foo <> nil then
    begin
      args := PyEngine.Py_BuildValue('()');
      res := PyEngine.PyObject_Call(foo, args, nil);
      if PyScriptIsSuccessful then
      begin
        Result := True;
        if res<>PyEngine.Py_None then
          ResultStr := PyEngine.PyObjectAsString(res)
        else
          ResultStr := '';
        PyEngine.PyErr_Clear;
      end;
    end
    else begin
      PyEngine.PyErr_Clear;
    end;
  finally
  end;
end;

constructor TPyScriptFunctions.Create(_PyEngine: TPythonEngine);
begin
  PyEngine := _PyEngine;
  PyModule := TPythonModule.Create(nil);

  PyModule.Name := 'PyModule';
  PyModule.Engine := _PyEngine;
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
  obj1 := PyEngine.PySys_GetObject('path');
  obj2 := PyEngine.PyUnicodeFromString(path);
  PyEngine.PyList_Append(obj1, obj2);
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  PythonScriptsDir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + '..\..\scripts';
end;

procedure TForm1.bPyInitClick(Sender: TObject);
begin
  if PyEngine = nil then
    try
      PyEngine := GetPythonEngine;
    except
      PyEngine := TPythonEngine.Create(nil);
    end;
  if pyf = nil then
    pyf := TPyScriptFunctions.Create(PyEngine);
end;

procedure TForm1.bRegPyFuncClick(Sender: TObject);
begin
  bPyInitClick(Sender);
  pyf.DoRegister;
end;

procedure TForm1.cbThreadsClick(Sender: TObject);
begin
  if cbThreads.Checked then
    TPythonThread.Py_Begin_Allow_Threads
  else
    TPythonThread.Py_End_Allow_Threads;
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
  //PyEngine.ExecStrings(mPy5.Lines);
  {ThreadPythonExec(
  procedure
  begin
    PyEngine.ExecStrings(mPy5.Lines);
  end);}
  Run_TempScript(mPy5.Lines);
end;

function TForm1.Run_TempScript(m: TStrings): String;
begin
  Result := '';
  if cbThreads.Checked then begin
    ThreadPythonExec(
      procedure
      begin
        PyEngine.ExecStrings(m);
      end);
  end else begin
    try
      if RunScript_py(m, Result) then
        mOutput.Text := Result;
    except
      on E: Exception do
        ErrorMsg(E.ToString);
    end;
  end;
end;

procedure TForm1.bPyScripterClick(Sender: TObject);
begin
  cInternalPython.CreateExternalModules := MyCreateExternalModules;
  pyscripter_init;
  pyscripter_show;
end;

end.
