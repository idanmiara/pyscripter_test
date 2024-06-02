program PyDemo;

uses
  Vcl.Forms,
  frmPyDemo in 'frmPyDemo.pas' {Form1},
  pyscripter_launch in 'pyscripter_launch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
