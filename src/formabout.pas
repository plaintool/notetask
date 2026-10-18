//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit FormAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  StdCtrls,
  ExtCtrls,
  Graphics,
  LCLIntf;

type

  { TformAboutNotetask }

  TformAboutNotetask = class(TForm)
    buttonOk: TButton;
    imageLogo: TImage;
    labelBy: TLabel;
    LabelLicUrl1: TLabel;
    labelName: TLabel;
    labelLic: TLabel;
    LabelLicUrl: TLabel;
    MemoInfo: TMemo;
    labelBy1: TLabel;
    labelVersion: TLabel;
    panelHeader: TPanel;
    panelBody: TPanel;
    panelFooter: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure LabelLicUrlClick(Sender: TObject);
  private

  public

  end;

var
  formAboutNotetask: TformAboutNotetask;

implementation

uses Localize, CheckUpdates, DarkUtils;

  {$R *.lfm}

  { TformAboutNotetask }

procedure TformAboutNotetask.FormCreate(Sender: TObject);
var
  Arch: string;
begin
  {$IFDEF CPU64}
    Arch := 'x64';
  {$ELSE}
  Arch := 'x86';
  {$ENDIF}

  labelName.Caption := 'Notetask © 2024';
  labelVersion.Caption := 'Version ' + GetAppVersion + ' · ' + Arch;

  // Links: color within the theme
  LabelLicUrl.Font.Color := TDarkUtils.ThemeColor(clBlue, clSkyBlue);
  LabelLicUrl1.Font.Color := LabelLicUrl.Font.Color;

  // Panels: Light/Dark Theme Adaptation
  panelHeader.Color := TDarkUtils.ThemeColor(16315632, $002D2D2D);
  panelBody.Color := TDarkUtils.ThemeColor(clWhite, $001E1E1E);
  labelVersion.Font.Color:=TDarkUtils.ThemeColor(clGray, clSilver);
  panelFooter.Color := panelBody.Color;
  MemoInfo.Color := panelBody.Color;
end;

procedure TformAboutNotetask.LabelLicUrlClick(Sender: TObject);
begin
  OpenUrl((Sender as TLabel).Hint);
end;

end.
