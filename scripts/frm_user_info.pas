unit frm_user_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormUserInfo }

  TFormUserInfo = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    edtAccount: TEdit;
    edtpassword: TEdit;
    lblAccount: TLabel;
    lblPassword: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormUserInfo: TFormUserInfo;

implementation

{$R *.lfm}

end.

