program TreatmentForm;

uses
  Vcl.Forms,
  UUseMode in 'UUseMode.pas' {fUseMode},
  UTips in 'UTips.pas',
  RRClass in 'RRClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfUseMode, fUseMode);
  Application.Run;
end.
