program Example;

uses
  Forms,
  Unit1 in 'Unit1.pas' {ExampleForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExampleForm, ExampleForm);
  Application.Run;
end.

