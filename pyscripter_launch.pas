unit pyscripter_launch;

interface

uses PythonEngine;

procedure pyscripter_init;
procedure pyscripter_show;
function pyscripter_engine: TPythonEngine;

implementation


uses
  dmResources,
  dmCommands,
  frmPyIDEMain,
  cPyControl,
  Vcl.Forms;

procedure pyscripter_init;
begin
  ReportMemoryLeaksOnShutdown :=  DebugHook <> 0;

  //TStyleManager.SystemHooks := TStyleManager.SystemHooks - [shMenus, shDialogs];

  //Application.Initialize;
  //SetDefaultUIFont(Application.DefaultFont);

  Application.MainFormOnTaskbar := True;

  {if TStyleManager.TrySetStyle('Windows10 SlateGray') then
    TStyleSelectorForm.CurrentSkinName := 'Windows10 SlateGray';}

  Application.Title := 'PyScripter';
  Application.CreateForm(TResourcesDataModule, ResourcesDataModule);
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.CreateForm(TPyIDEMainForm, PyIDEMainForm);
end;

procedure pyscripter_show;
begin
  PyIDEMainForm.Show;
end;

function pyscripter_engine: TPythonEngine;
begin
  Result := PyControl.InternalPython.PythonEngine;
end;

end.
