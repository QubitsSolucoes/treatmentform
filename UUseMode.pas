unit UUseMode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UBaseForm, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Mask;

type
  TfUseMode = class(TfBaseForm)
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    Button1: TButton;
    ComboBox1: TComboBox;
    MaskEdit1: TMaskEdit;
    MaskEdit2: TMaskEdit;
    MaskEdit3: TMaskEdit;
    MaskEdit4: TMaskEdit;
    MaskEdit5: TMaskEdit;
    MaskEdit6: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbStatus: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUseMode: TfUseMode;

implementation

{$R *.dfm}

procedure TfUseMode.Button1Click(Sender: TObject);
var
  ListIgnora : TStringList;
begin
  ListIgnora := TStringList.Create;
  //retirando componente do tratamento geral.
  ListIgnora.Add(LabeledEdit4.Name);

  if StartTreatment(ListIgnora) then
    lbStatus.Caption := 'OK'
  else
    lbStatus.Caption := 'Não OK';

  ListIgnora.Free;
end;

procedure TfUseMode.FormShow(Sender: TObject);
begin
  inherited;
  //Tratando taborder do fomulário inteiro.
  TreatTabOrder;
end;

end.
