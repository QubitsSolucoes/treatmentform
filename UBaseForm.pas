unit UBaseForm;

interface

uses
  Winapi.Messages, Windows, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids, Data.DB, Math,

  System.Types, Vcl.Graphics, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Mask, DateUtils,

  //ZEOS
  ZDataSet, ZAbstractDataset, ZAbstractRODataSet;

type
  TCharSet = set of AnsiChar;

type
  TEditBalloonTip = packed record
    cbStruct: DWORD ;
    pszTitle: LPCWSTR ;
    pszText: LPCWSTR;
    ttiIcon: Integer;
  public
    const
      ECM_FIRST = $1500;
      EM_SHOWBALLOONTIP = (ECM_FIRST + 3);
      EM_HIDEBALLOONTIP = (ECM_FIRST + 4);
      TTI_NONE = 0;
      TTI_INFO = 1;
      TTI_WARNING = 2;
      TTI_ERROR = 3;
    procedure ShowBalloonTip(Window : HWnd; Texto, Titulo : PWideChar; Tipo : Integer);
    procedure HideBalloonTip(Window : HWnd);
  private
  end;

type
  TFormHelper = class helper for TForm
  private
    procedure TreatTabOrder(const Parent: TWinControl); overload;
  public
    function StartTreatment(const IgnoreList : TStringList = nil) : boolean;
    procedure TreatTabOrder; overload;
  end;

type
  TDBGrid = Class(Vcl.DBGrids.TDBGrid)
  strict private
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  private
    FColorDataCell : TColor;
    procedure SetColorDataCell(const Value: TColor);
  protected
    procedure TitleClick(Column : TColumn);override;
    procedure DrawDataCell(const Rect: TRect; Field: TField; State: TGridDrawState); override;
  published
    procedure AjustColumns;
    property ColorDataCell : TColor read FColorDataCell write SetColorDataCell;
  end;

type
  TComboBoxHelper = class helper for TComboBox
  public
    function GetIndex(str : string) : Integer;
  end;

type
  TCustomEditHelper = class helper for TCustomEdit
  private
    function OnlyNumber : String;overload;
    function OnlyNumber(const AValue: String): String;overload;
    function OnlyNumberOrChar(const AValue: String): String;
    function CharsInSet(Value: String; CharSet: TCharSet): String;
  public
    function IsDate : Boolean;
    function IsCNPJ : boolean;
    function IsCPF : boolean;
    function IsCEP : Boolean; overload;
    function IsCEP(cEstado: string): Boolean; overload;
    function IsEmail : boolean;

    function SetMaskCpfCnpj : string;
    function SetMaskDate2D : string;
    function SetMaskDate4D : string;
    function SetMaskPhone : string;
  end;

type
  TfBaseForm = class(TForm)
    FadeIn: TTimer;
    FadeOut: TTimer;
    procedure FadeInTimer(Sender: TObject);
    procedure FadeOutTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  protected
    function VersaoExe : string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fBaseForm: TfBaseForm;
  fBallonTip : TEditBalloonTip;

  Function ProximoDiaUtil(dData : TDate) : String;
implementation

{$R *.dfm}

Function ProximoDiaUtil(dData : TDate) : String;
  function IsFeriado(data: TDate): Boolean;
    function CalculaPascoa(AAno: Word): TDateTime;
    var
      R1, R2, R3, R4, R5 : Longint;
      FPascoa : TDateTime;
      VJ, VM, VD : Word;
    begin
      R1 := AAno mod 19;
      R2 := AAno mod 4;
      R3 := AAno mod 7;
      R4 := (19 * R1 + 24) mod 30;
      R5 := (6 * R4 + 4 * R3 + 2 * R2 + 5) mod 7;
      FPascoa := EncodeDate(AAno, 3, 22);
      FPascoa := FPascoa + R4 + R5;
      DecodeDate(FPascoa, VJ, VM, VD);
      case VD of
        26 : FPascoa := EncodeDate(Aano, 4, 19);
        25 : if R1 > 10 then
               FPascoa := EncodeDate(AAno, 4, 18);
      end;
      Result:= FPascoa;
    end;
  var
    i: integer;
    dia, mes, ano: Word;
    n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12: Integer;
    pascoa,
    Sab_Carnaval,
    Dom_Carnaval,
    Seg_Carnaval,
    Ter_Carnaval,
    Qua_Carnaval,
    Sem_Santa_Dom,
    Sem_Santa_Seg,
    Sem_Santa_Ter,
    Sem_Santa_Qua,
    Sem_Santa_Qui,
    Paixao_de_Cristo_Sex,
    Sab_Aleluia,
    corpus : TDate;
  begin
    Result := false;

    dia := DayOf(data);
    mes := MonthOf(data);

    // feriados fixos
    if ((dia = 1)   and (mes = 1))  or
       ((dia = 21)  and (mes = 4))  or
       ((dia = 1)   and (mes = 5))  or
       ((dia = 7)   and (mes = 9))  or
       ((dia = 12)  and (mes = 10)) or
       ((dia = 2)   and (mes = 11)) or
       ((dia = 15)  and (mes = 11)) or
       ((dia = 25)  and (mes = 12)) then
    begin
      Result := true;
      Exit;
    end;

    ano := YearOf(data);

    // feriados móveis
    pascoa := CalculaPascoa(ano);

    Sab_Carnaval := IncDay(pascoa, -50);
    Dom_Carnaval := IncDay(pascoa, -49);
    Seg_Carnaval := IncDay(pascoa, -48);
    Ter_Carnaval := IncDay(pascoa, -47);
    Qua_Carnaval := IncDay(pascoa, -46);

    Sem_Santa_Dom        := IncDay(pascoa, -7);
    Sem_Santa_Seg        := IncDay(pascoa, -6);
    Sem_Santa_Ter        := IncDay(pascoa, -5);
    Sem_Santa_Qua        := IncDay(pascoa, -4);
    Sem_Santa_Qui        := IncDay(pascoa, -3);
    Paixao_de_Cristo_Sex := IncDay(pascoa, -2);
    Sab_Aleluia          := IncDay(pascoa, -1);

    corpus := IncDay(pascoa, 60);

    if (data = pascoa) or
       (data = Sab_Carnaval) or
       (data = Dom_Carnaval) or
       (data = Seg_Carnaval) or
       (data = Ter_Carnaval) or
       (data = Qua_Carnaval) or

       (data = Sem_Santa_Dom) or
       (data = Sem_Santa_Seg) or
       (data = Sem_Santa_Ter) or
       (data = Sem_Santa_Qua) or
       (data = Sem_Santa_Qui) or
       (data = Paixao_de_Cristo_Sex) or
       (data = Sab_Aleluia) or

       (data = corpus) then
    begin
      Result := true;
      Exit;
    end;
  end;
begin
  //fins de semana
  if DayOfWeek(dData) = 7 then
    dData := IncDay(dData, 2)
  else if DayOfWeek(dData) = 1 then
    dData := IncDay(dData, 1);

  //feriados
  while IsFeriado(dData) do
    dData := StrToDate(ProximoDiaUtil(IncDay(dData, 1)));

  Result := DateToStr(dData);
end;

{ TDBGrid }

procedure TDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  if DataSource.DataSet.Active then
  begin
    if DataSource.DataSet.RecordCount > 0 then
    begin
      if not odd(DataSource.DataSet.RecNo) then // zebra
      begin
        if not (gdSelected in State) then
        begin
          Canvas.Brush.Color := FColorDataCell;
          Canvas.FillRect(Rect);
          DefaultDrawDataCell(Rect, Field, State);
        end;
      end;
    end;
  end;
end;

procedure TDBGrid.SetColorDataCell(const Value: TColor);
begin
  FColorDataCell := Value;
end;

procedure TDBGrid.TitleClick(Column: TColumn);
{$J+}
 const PreviousColumnIndex : integer = -1;
{$J-}
var
  ATitleCaption : string;
begin
  if DataSource.DataSet is TZQuery then
  begin
    ATitleCaption := '';

    if PreviousColumnIndex <> -1 then
    begin
      Columns[PreviousColumnIndex].title.Font.Style :=
        Columns[PreviousColumnIndex].title.Font.Style - [fsBold];

      ATitleCaption := trim(StringReplace(Columns[PreviousColumnIndex].title.Caption, '▼', '', [rfReplaceAll]));
      ATitleCaption := trim(StringReplace(ATitleCaption, '▲', '', [rfReplaceAll]));
      Columns[PreviousColumnIndex].title.Caption := ATitleCaption;
    end;

    Column.title.Font.Style :=
      Column.title.Font.Style + [fsBold];
    PreviousColumnIndex := Column.Index;

    ATitleCaption := trim(StringReplace(Column.Title.Caption, '▼', '', [rfReplaceAll]));
    ATitleCaption := trim(StringReplace(ATitleCaption, '▲', '', [rfReplaceAll]));
    Column.Title.Caption := ATitleCaption;

    Column.Width := Column.Width - 20;

    if (TZQuery(DataSource.DataSet).SortType = stAscending) then
    begin
      TZQuery(DataSource.DataSet).SortedFields := Column.Field.FieldName + ' DESC';
      TZQuery(DataSource.DataSet).SortType := stDescending;
      Column.Title.Caption := ATitleCaption+' ▼';
      Column.Width := Column.Width + 20;
    end
    else
    begin
      TZQuery(DataSource.DataSet).SortedFields := Column.Field.FieldName + ' ASC';
      TZQuery(DataSource.DataSet).SortType := stAscending;
      Column.Title.Caption := ATitleCaption+' ▲';
      Column.Width := Column.Width + 20;
    end;
  end;
end;

procedure TDBGrid.AjustColumns;
var
  ColumnCount, RowCount : integer;
  DataSetTemp : TDataSet;
  DataSourceTemp : TDataSource;
  contCol, contRow : integer;
  AValue : integer;
  MStrValue, AStrValue : string;
begin
  ColumnCount := Columns.Count;

  if (ColumnCount = 0) then Exit;
  if not DataSource.DataSet.Active  then Exit;

  DataSetTemp := DataSource.DataSet;
  DataSourceTemp := DataSource;
  //DataSource := nil;
  RowCount := DataSetTemp.RecordCount;

  for contCol := 0 to ColumnCount-1 do
  begin
    AValue := 0;
    AStrValue := '';

    DataSetTemp.First;
    MStrValue := Columns[contCol].Title.Caption;//DataSetTemp.FieldByName(DBGrid.Columns[contCol].FieldName).AsString;
    while not DataSetTemp.Eof do
    begin
      AValue := Length(DataSetTemp.FieldByName(Columns[contCol].FieldName).AsString);
      AStrValue := DataSetTemp.FieldByName(Columns[contCol].FieldName).AsString;
      DataSetTemp.Next;

      if length(MStrValue) < AValue then
        MStrValue := AStrValue;
    end;

    Columns[contCol].Width := Canvas.TextWidth(MStrValue)+15;
  end;

  DataSource := DataSourceTemp;
end;

constructor TDBGrid.Create(AOwner: TComponent);
begin
  ColorDataCell := $00FFEFDF;
  inherited;
  ReadOnly := True;
  Options := Options - [dgEditing, dgIndicator, dgRowLines];
end;

destructor TDBGrid.Destroy;
begin
  inherited;
end;

{TfBaseForm}


procedure TfBaseForm.FadeInTimer(Sender: TObject);
begin
  AlphaBlendValue := AlphaBlendValue + 15;
  FadeIn.Enabled := not (AlphaBlendValue >= 255);
end;

procedure TfBaseForm.FadeOutTimer(Sender: TObject);
begin
  AlphaBlendValue := AlphaBlendValue - 15;
  if AlphaBlendValue <= 0 then
    Close;
end;

procedure TfBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  while AlphaBlendValue <> 0 do
  begin
    FadeOut.Enabled := true;
    Application.ProcessMessages;
  end;
end;

procedure TfBaseForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_ESCAPE : Close;//FadeOut.Enabled := true;
    VK_RETURN : perform(WM_NEXTDLGCTL,0,0);
  end;
end;

procedure TfBaseForm.FormShow(Sender: TObject);
begin
  FadeIn.Enabled := True;
end;


function TfBaseForm.VersaoExe: string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  V1, V2, V3, V4: Word;
  Prog : string;
  I: Integer;
begin
  Prog := Application.ExeName;
  VerInfoSize := GetFileVersionInfoSize(PChar(prog), Dummy);

  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(prog), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    V1 := dwFileVersionMS shr 16;
    V2 := dwFileVersionMS and $FFFF;
    V3 := dwFileVersionLS shr 16;
    V4 := dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
  result := inttostr(v1) + '.' + inttostr(v2) + '.' + inttostr(v3) + '.' + inttostr(v4);
end;

{ TCustomEditHelper }

function TCustomEditHelper.IsCNPJ : boolean;
var
  D1 : array[1..12] of byte;
  I,
  DF1,
  DF2,
  DF3,
  DF4,
  DF5,
  DF6,
  Resto1,
  Resto2,
  PrimeiroDigito,
  SegundoDigito : integer;
  _Text : string;
begin
  Result := true;
  _Text := OnlyNumber;
  if Length(_Text) = 14 then
  begin
    for I := 1 to 12 do
      if _Text[I] in ['0'..'9'] then
        D1[I] := StrToInt(_Text[I])
      else
        Result := false;

    if Result then
    begin
      DF1 := 0;
      DF2 := 0;
      DF3 := 0;
      DF4 := 0;
      DF5 := 0;
      DF6 := 0;
      Resto1 := 0;
      Resto2 := 0;
      PrimeiroDigito := 0;
      SegundoDigito := 0;

      DF1 := 5*D1[1] + 4*D1[2] + 3*D1[3] + 2*D1[4] + 9*D1[5] + 8*D1[6] +
             7*D1[7] + 6*D1[8] + 5*D1[9] + 4*D1[10] + 3*D1[11] + 2*D1[12];
      DF2 := DF1 div 11;
      DF3 := DF2 * 11;
      Resto1 := DF1 - DF3;

      if (Resto1 = 0) or (Resto1 = 1) then
        PrimeiroDigito := 0
      else
        PrimeiroDigito := 11 - Resto1;

      DF4 := 6*D1[1] + 5*D1[2] + 4*D1[3] + 3*D1[4] + 2*D1[5] + 9*D1[6] +
             8*D1[7] + 7*D1[8] + 6*D1[9] + 5*D1[10] + 4*D1[11] + 3*D1[12] +
             2*PrimeiroDigito;
      DF5 := DF4 div 11;
      DF6 := DF5 * 11;

      Resto2 := DF4 - DF6;
      if (Resto2 = 0) or (Resto2 = 1) then
        SegundoDigito := 0
      else
        SegundoDigito := 11 - Resto2;
      if (PrimeiroDigito <> StrToInt(_Text[13])) or
         (SegundoDigito <> StrToInt(_Text[14])) then
        Result := false;
    end;
  end
  else
    if Length(_Text) <> 0 then
      Result := false;
end;

{Valida dígito verificador de CPF}
function TCustomEditHelper.IsCPF : boolean;
var
  D1 : array[1..9] of byte;
  I,
  DF1,
  DF2,
  DF3,
  DF4,
  DF5,
  DF6,
  Resto1,
  Resto2,
  PrimeiroDigito,
  SegundoDigito : integer;
  _Text : string;
begin
  Result := true;
  _Text := OnlyNumber;
  if Length(_Text) = 11 then
  begin
    for I := 1 to 9 do
    if _Text[I] in ['0'..'9'] then
      D1[I] := StrToInt(_Text[I])
    else
      Result := false;

    if Result then
    begin
      DF1 := 0;
      DF2 := 0;
      DF3 := 0;
      DF4 := 0;
      DF5 := 0;
      DF6 := 0;
      Resto1 := 0;
      Resto2 := 0;
      PrimeiroDigito := 0;
      SegundoDigito := 0;

      DF1 := 10*D1[1] + 9*D1[2] + 8*D1[3] + 7*D1[4] + 6*D1[5] + 5*D1[6] +
              4*D1[7] + 3*D1[8] + 2*D1[9];
      DF2 := DF1 div 11;
      DF3 := DF2 * 11;

      Resto1 := DF1 - DF3;

      if (Resto1 = 0) or (Resto1 = 1) then
        PrimeiroDigito := 0
      else
        PrimeiroDigito := 11 - Resto1;

      DF4 := 11*D1[1] + 10*D1[2] + 9*D1[3] + 8*D1[4] + 7*D1[5] + 6*D1[6] +
              5*D1[7] + 4*D1[8] + 3*D1[9] + 2*PrimeiroDigito;

      DF5 := DF4 div 11;
      DF6 := DF5 * 11;
      Resto2 := DF4 - DF6;

      if (Resto2 = 0) or (Resto2 = 1) then
        SegundoDigito := 0
      else
        SegundoDigito := 11 - Resto2;

      if (PrimeiroDigito <> StrToInt(_Text[10])) or
         (SegundoDigito <> StrToInt(_Text[11])) then
        Result := false;
    end;
  end
  else
    if Length(_Text) <> 0 then
      Result := false;
end;

function TCustomEditHelper.IsDate: Boolean;
begin
  try
    StrToDate(Text);
    Result := True;
  except
    Result := False;
    Clear;
    SetFocus;
  end;
end;

function TCustomEditHelper.IsCEP : Boolean;
var
  D1 : array[1..9] of byte;
  cCEP1, I : Integer;
begin
//  Text := StringReplace(Text, '-', '', [rfReplaceAll]);
//  Text := copy(Text,1,5) + copy(Text,6,3);
  cCEP1 := StrToInt(copy(OnlyNumber,1,3));

  if Length(trim(Text)) > 0 then
  begin
    if (StrToInt(Text) <= 1000000.0) then
    begin
      MessageDlg('CEP tem que ser maior que [01000-000]', mtError,[mbOk],0);
      Result := False;
    end
    else
    begin
      if Length(trim(copy(Text,6,3))) < 3 then
        Result := False
      else
      if (cCEP1 >= 10 ) and (cCEP1 <= 199) then Result := True else
      if (cCEP1 >= 200) and (cCEP1 <= 289) then Result := True else
      if (cCEP1 >= 290) and (cCEP1 <= 299) then Result := True else
      if (cCEP1 >= 300) and (cCEP1 <= 399) then Result := True else
      if (cCEP1 >= 400) and (cCEP1 <= 489) then Result := True else
      if (cCEP1 >= 490) and (cCEP1 <= 499) then Result := True else
      if (cCEP1 >= 500) and (cCEP1 <= 569) then Result := True else
      if (cCEP1 >= 570) and (cCEP1 <= 579) then Result := True else
      if (cCEP1 >= 580) and (cCEP1 <= 589) then Result := True else
      if (cCEP1 >= 590) and (cCEP1 <= 599) then Result := True else
      if (cCEP1 >= 600) and (cCEP1 <= 639) then Result := True else
      if (cCEP1 >= 640) and (cCEP1 <= 649) then Result := True else
      if (cCEP1 >= 650) and (cCEP1 <= 659) then Result := True else
      if (cCEP1 >= 660) and (cCEP1 <= 688) then Result := True else
      if ((cCEP1 >= 690) and (cCEP1 <= 692) or (cCEP1 >= 694) and (cCEP1 <= 698)) then
        Result := True
      else
      if (cCEP1 = 689) then Result := True else
      if (cCEP1 = 693) then Result := True else
      if (cCEP1 = 699) then Result := True else
      if (cCEP1 >= 000) and (cCEP1 <= 999) then
        Result := True
      else
      if (cCEP1 >= 770) and (cCEP1 <= 779) then Result := True else
      if (cCEP1 >= 780) and (cCEP1 <= 788) then Result := True else
      if (cCEP1 >= 790) and (cCEP1 <= 799) then Result := True else
      if (cCEP1 = 789) then
        Result := True
      else
      if (cCEP1 >= 800) and (cCEP1 <= 879) then Result := True else
      if (cCEP1 >= 880) and (cCEP1 <= 899) then Result := True else
      if (cCEP1 >= 900) and (cCEP1 <= 999) then Result := True else
        Result := False;
    end;
  end
  else
    Result := True;
end;

function TCustomEditHelper.CharsInSet(Value: String; CharSet: TCharSet): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    if Value[i] in CharSet then
      Result := Result + Value[i];
  end;
end;

function TCustomEditHelper.IsCEP(cEstado: string): Boolean;
var
  D1 : array[1..9] of byte;
  cCEP1, I : Integer;
begin
  Text := StringReplace(Text, '-', '', [rfReplaceAll]);
  Text := copy(Text,1,5) + copy(Text,6,3);
  cCEP1 := StrToInt(copy(Text,1,3));

  if Length(trim(Text)) > 0 then
  begin
    if (StrToInt(Text) <= 1000000.0) then
    begin
      MessageDlg('CEP tem que ser maior que [01000-000]', mtError,[mbOk],0);
      Result := False;
    end
    else
    begin
      if Length(trim(copy(Text,6,3))) < 3 then
        Result := False
      else
      if (cEstado = 'SP') and (cCEP1 >= 10 ) and (cCEP1 <= 199) then Result := True else
      if (cEstado = 'RJ') and (cCEP1 >= 200) and (cCEP1 <= 289) then Result := True else
      if (cEstado = 'ES') and (cCEP1 >= 290) and (cCEP1 <= 299) then Result := True else
      if (cEstado = 'MG') and (cCEP1 >= 300) and (cCEP1 <= 399) then Result := True else
      if (cEstado = 'BA') and (cCEP1 >= 400) and (cCEP1 <= 489) then Result := True else
      if (cEstado = 'SE') and (cCEP1 >= 490) and (cCEP1 <= 499) then Result := True else
      if (cEstado = 'PE') and (cCEP1 >= 500) and (cCEP1 <= 569) then Result := True else
      if (cEstado = 'AL') and (cCEP1 >= 570) and (cCEP1 <= 579) then Result := True else
      if (cEstado = 'PB') and (cCEP1 >= 580) and (cCEP1 <= 589) then Result := True else
      if (cEstado = 'RN') and (cCEP1 >= 590) and (cCEP1 <= 599) then Result := True else
      if (cEstado = 'CE') and (cCEP1 >= 600) and (cCEP1 <= 639) then Result := True else
      if (cEstado = 'PI') and (cCEP1 >= 640) and (cCEP1 <= 649) then Result := True else
      if (cEstado = 'MA') and (cCEP1 >= 650) and (cCEP1 <= 659) then Result := True else
      if (cEstado = 'PA') and (cCEP1 >= 660) and (cCEP1 <= 688) then Result := True else
      if (cEstado = 'AM') and ((cCEP1 >= 690) and (cCEP1 <= 692) or (cCEP1 >= 694) and (cCEP1 <= 698)) then
        Result := True
      else
      if (cEstado = 'AP') and (cCEP1 = 689) then Result := True else
      if (cEstado = 'RR') and (cCEP1 = 693) then Result := True else
      if (cEstado = 'AC') and (cCEP1 = 699) then Result := True else
      if ((cEstado = 'DF') or (cEstado = 'GO')) and (cCEP1 >= 000)and(cCEP1 <= 999) then
        Result := True
      else
      if (cEstado = 'TO') and (cCEP1 >= 770) and (cCEP1 <= 779) then Result := True else
      if (cEstado = 'MT') and (cCEP1 >= 780) and (cCEP1 <= 788) then Result := True else
      if (cEstado = 'MS') and (cCEP1 >= 790) and (cCEP1 <= 799) then Result := True else
      if (cEstado = 'RO') and (cCEP1 = 789) then
        Result := True
      else
      if (cEstado = 'PR') and (cCEP1 >= 800) and (cCEP1 <= 879) then Result := True else
      if (cEstado = 'SC') and (cCEP1 >= 880) and (cCEP1 <= 899) then Result := True else
      if (cEstado = 'RS') and (cCEP1 >= 900) and (cCEP1 <= 999) then Result := True else
        Result := False;
    end;
  end
  else
    Result := True;
end;

function TCustomEditHelper.IsEmail : boolean;
var
  _Text : string;
begin
  _Text := Trim(UpperCase(Text));
  if (Pos('@', _Text) > 1) or (Pos('.', _Text) > 0) or (Pos('-', _Text) > 0) or (Pos('_', _Text) > 0)then
  begin
    Delete(_Text, 1, pos('@', _Text));
//    Delete(email, 1, pos('.', email));
    Result := (Length(_Text) > 0) and (Pos('.', _Text) > 1);
  end
  else
   Result := False;
end;

// Função para permitir apenas número e letras
function TCustomEditHelper.OnlyNumber: String;
begin
  Result := CharsInSet(Text, ['0' .. '9']);
end;

function TCustomEditHelper.OnlyNumberOrChar(const AValue: String): String;
begin
  Result := CharsInSet(AValue, ['0' .. '9', 'A' .. 'Z', 'a' .. 'z']);
end;

// Função para permitir apenas números
function TCustomEditHelper.OnlyNumber(const AValue: String): String;
begin
  Result := CharsInSet(AValue, ['0' .. '9']);
end;

function TCustomEditHelper.SetMaskCpfCnpj : string;
var
  _Text : String;
begin
  _Text := Text;
  if length(_Text) = 11 then
    result := copy(_Text, 1, 3)+'.'+copy(_Text, 4, 3)+'.'+copy(_Text, 7, 3)+'-'+copy(_Text, 10, 2)
  else
    result := copy(_Text, 1, 2)+'.'+copy(_Text, 3, 3)+'.'+copy(_Text, 6, 3)+'/'+copy(_Text, 9, 4)+'-'+copy(_Text, 13, 2);
end;

function TCustomEditHelper.SetMaskDate2D : string;
var
  _Text : String;
begin
  _Text := Text;
  Result := Copy(_Text, 5, 2) + '/'  + Copy(_Text, 3, 2) + '/' + Copy(_Text, 1, 2);
end;

function TCustomEditHelper.SetMaskDate4D: string;
var
  _Text : String;
begin
  _Text := Text;
  Result := Copy(_Text, 7, 2) + '/'  + Copy(_Text, 5, 2) + '/' + Copy(_Text, 1, 4);
end;

function TCustomEditHelper.SetMaskPhone: string;
var
  _Text : String;
begin
  _Text := Text;
  if length(_Text) = 10 then
    result := '('+copy(_Text, 1, 2)+') '+copy(_Text, 3, 4)+'-'+copy(_Text, 7, 4)
  else
    result := '('+copy(_Text, 1, 2)+') '+copy(_Text, 3, 5)+'-'+copy(_Text, 8, 4)
end;

{ TComboBoxHelper }

function TComboBoxHelper.GetIndex(str: string): Integer;
var
  c : integer;
begin
  result := -1;
  for c := 0 to Items.Count-1 do
  begin
    ItemIndex := c;
    if UpperCase(Trim(Text)) = UpperCase(trim(str)) then
      Result := c;
  end;
end;

{ TEditBalloonTip }

procedure TEditBalloonTip.ShowBalloonTip(Window : HWnd; Texto, Titulo : PWideChar; Tipo : Integer);
var
  EditBalloonTip : TEditBalloonTip;
begin
  EditBalloonTip.cbStruct := SizeOf(TEditBalloonTip);
  EditBalloonTip.pszText := Texto;
  EditBalloonTip.pszTitle := Titulo;
  EditBalloonTip.ttiIcon := Tipo;
  SendMessageW(Window, EM_SHOWBALLOONTIP, 0, Integer(@EditBalloonTip));
end;

procedure TEditBalloonTip.HideBalloonTip(Window : HWnd);
begin
  SendMessageW(Window, EM_HIDEBALLOONTIP, 0, 0);
end;

{ TForm }

function TFormHelper.StartTreatment(const IgnoreList : TStringList = nil): boolean;
  function VerificaNome(Nome : string; IgnoreList : TStringList) : Boolean;
  var
    i : integer;
  begin
    result := false;

    if IgnoreList <> nil then
    begin
      for I := 0 to IgnoreList.Count-1 do
      begin
        if Nome = IgnoreList[i] then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
var
  i, j : integer;
  List : TList;
  TabOrder : integer;
begin
  {Aviso inicial}
  try
    result := false;
    List := TList.Create;

    //captura a lista de taborders
    GetTabOrderList(List);
    for i := 0 to List.Count - 1 do
    begin
      //TRATAMENTO PARA VERIFICAÇÃO DOS COMPONENTES QUE SERÃO IGNORADOS
      if not VerificaNome(Components[TWinControl(List[i]).ComponentIndex].Name, IgnoreList) then
      begin
        //verifica se o componente da vez está habilitado
        if (Components[TWinControl(List[i]).ComponentIndex] as TWinControl).Enabled then
        begin
          //verifica se o componente da vez está visível
          if (Components[TWinControl(List[i]).ComponentIndex] as TWinControl).Visible then
          begin
            {verifica se o componente naquele momento é um TEedit}
            if Components[TWinControl(List[i]).ComponentIndex] is TEdit then
            begin
              {se for um TEdit e ele estiver vazio e sem espaço em branco
              função trim remove os espaços em branco do início da string}
              if trim(UPPERCASE((Components[TWinControl(List[i]).ComponentIndex] as TEdit).Name)).Contains('EMAIL') then
              begin
                if not ((Components[TWinControl(List[i]).ComponentIndex] as TEdit).IsEmail) then
                begin
                  //VERIFICA A PARTIR DA FUNÇÃO ACIMA - E MOSTRA O BALÃO NO CAMPO INDICADO.
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TEdit).Handle, 'Por favor, preencha um E-mail válido.', 'E-mail Inválido!', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              else
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TEdit).Text) = '' then
                begin
                  //VERIFICA A PARTIR DA FUNÇÃO ACIMA - E MOSTRA O BALÃO NO CAMPO INDICADO.
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TEdit).Handle, 'Por favor, preencha este campo.', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  Result := true;
              end;
            end;

            if Components[TWinControl(List[i]).ComponentIndex] is TLabeledEdit then
            begin
              if trim((Components[TWinControl(List[i]).ComponentIndex] as TLabeledEdit).Text) = '' then
              begin
                fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TLabeledEdit).Handle, 'Por favor, preencha este campo.', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                Result := False;
                Exit;
              end
              else
                result := true;
            end;

            if Components[TWinControl(List[i]).ComponentIndex] is TMaskEdit then
            begin
              //TRATAMENTO DE DATAS
              if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).EditMask).Contains('99/99/99') then
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '  /  /  ' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha esta Data!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else if not (Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).IsDate then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha esta uma Data válida!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              else if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).EditMask).Contains('99/99/9999') then
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '  /  /    ' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha esta Data!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else if not (Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).IsDate then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha esta Data!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              //TRATAMENTO DE CNPJ
              else if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).EditMask).Contains('99.999.999/9999-99') then
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha o CNPJ!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else if not (Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).IsCNPJ then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha um CNPJ válido!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              //TRATAMENTO DE CPF
              else if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).EditMask).Contains('999.999.999-99') then
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha o CPF!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else if not (Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).IsCPF then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha um CPF válido!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              //TRATAMENTO DE CEP
              else if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).EditMask).Contains('99.999-999') then
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha o CEP!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else if not (Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).IsCep then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha um CEP válido!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end
              //TRATAMENTO TRADICIONAL
              else
              begin
                if trim((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Text) = '' then
                begin
                  fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TMaskEdit).Handle, 'Por favor, preencha este campo!', 'Campo obrigatório', fBallonTip.TTI_WARNING);
                  Result := False;
                  Exit;
                end
                else
                  result := true;
              end;
            end;

            if Components[TWinControl(List[i]).ComponentIndex] is TComboBox then
            begin
              if (Components[TWinControl(List[i]).ComponentIndex] as TComboBox).ItemIndex < 0 then
              begin
                if (Components[TWinControl(List[i]).ComponentIndex] as TComboBox).TextHint <> '' then
                  MessageDlg('Por favor, selecione '+(Components[TWinControl(List[i]).ComponentIndex] as TComboBox).TextHint+' item!', mtWarning, [mbOK], 0)
                else
                  MessageDlg('Por favor, selecione algum item!', mtWarning, [mbOK], 0);

                (Components[TWinControl(List[i]).ComponentIndex] as TComboBox).SetFocus;
                //fBallonTip.ShowBalloonTip((Components[TWinControl(List[i]).ComponentIndex] as TComboBox).Handle, 'Por favor, selecione algum item.', 'Campo com item inválido', fBallonTip.TTI_WARNING);
                Result := False;
                Exit;
              end
              else
                result := true;
            end;
          end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TFormHelper.TreatTabOrder;
var
  i, Cont: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i] is TWinControl then
      begin
        if List.Count = 0 then
          Cont := 0
        else
        begin
          with Controls[i] do
            for Cont := 0 to List.Count - 1 do
              if (Top < TControl(List[Cont]).Top) or
                 ((Top = TControl(List[Cont]).Top) and (Left < TControl(List[Cont]).Left)) then
                Break;
        end;
        List.Insert(Cont, Controls[i]);
        TreatTabOrder(TWinControl(Controls[i]));
      end;
    end;

    for i := 0 to List.Count - 1 do TWinControl(List[i]).TabOrder := i;
  finally
    List.Free;
  end;
end;

procedure TFormHelper.TreatTabOrder(const Parent: TWinControl);
var
  i, Cont: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    for i := 0 to Parent.ControlCount - 1 do
    begin
      if Parent.Controls[i] is TWinControl then
      begin
        if List.Count = 0 then
          Cont := 0
        else
        begin
          with Parent.Controls[i] do
            for Cont := 0 to List.Count - 1 do
              if (Top < TControl(List[Cont]).Top) or
                 ((Top = TControl(List[Cont]).Top) and (Left < TControl(List[Cont]).Left)) then
                Break;
        end;
        List.Insert(Cont, Parent.Controls[i]);
        TreatTabOrder(TWinControl(Self.Controls[i]));
      end;
    end;

    for i := 0 to List.Count - 1 do TWinControl(List[i]).TabOrder := i;
  finally
    List.Free;
  end;
end;

end.
