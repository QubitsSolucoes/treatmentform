program TreatmentForm;

uses
  Vcl.Forms,
  UBaseForm in 'UBaseForm.pas' {fBaseForm},
  UUseMode in 'UUseMode.pas' {fUseMode};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfUseMode, fUseMode);
  Application.CreateForm(TfBaseForm, fBaseForm);
  Application.Run;
end.
