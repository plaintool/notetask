//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit Consts;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics;

  { Common Consts }

const
  REPO = 'plaintool/notetask';
  APP_NAME = 'notetask';

  { Main Form }

const
  DefRowHeight = 22;
  {$IFDEF UNIX}
  DefFontSize = 10;
  {$ELSE}
  DefFontSize = 9;
  {$ENDIF}

  TagsColorBrigtness = 80;
  TagsColorSaturation = 80;
  TagsDimnessSelected = 55;
  TagsDimnessPrint = 60;
  TagsDimnessColor = 45;
  TagsDimness = 35;

  IndentStr = '  ';
  CommentSlashStr = '//';
  CommentHashStr = '#';
  CommentStarStr = '*';
  CommentMinusStr = '--';
  CommentSemicolonStr = ';';
  CommentTwoColonStr = '::';
  CommentREMStr = 'REM ';
  CommentApostropheStr = '''';
  mailto = 'mailto:';
  http = 'http://';

  // Light theme colors
  clRowHighlight_Light = TColor($FFF0DC);       // RGB(220,240,255)
  clRowFocused_Light = TColor($FFDCC8);         // RGB(200,220,255)
  clRowExpired_Light = TColor($DCDCFF);         // RGB(255,220,220)
  clRowNotDone_Light = TColor($000096);         // RGB(150,0,0)
  clPlanned_Light = TColor($B40000);            // RGB(0,0,180)
  clReadOnly_Light = TColor($F0F0F0);           // RGB(240,240,240)
  clSplitFilter_Light = TColor($FAFAFA);        // RGB(250,250,250)
  clSpit_Light = TColor($E9E9E9);               // RGB(233,233,233)
  clSplitHighlight_Light = TColor($D2D2D2);     // RGB(210,210,210)
  clTagSuffix_Light = TColor($FEFEFE);          // RGB(254,254,254)
  clDuplicateHighlight_Light = TColor($AAFFFF); // RGB(255,255,170)
  clGridLineColor_Light = TColor($CACACA);      // RGB(202,202,202)

  // Dark theme colors
  clRowHighlight_Dark = TColor($463027);        // RGB(39, 48, 70)
  clRowFocused_Dark = TColor($6C4C38);          // RGB(56, 76, 108)
  clRowExpired_Dark = TColor($2D2D50);          // RGB(80, 45, 45)
  clRowNotDone_Dark = TColor($9696FF);          // RGB(255,150,150)
  clPlanned_Dark = TColor($FF8C00);             // RGB(0, 140, 255)
  clReadOnly_Dark = TColor($404040);            // RGB(64, 64, 64)
  clSplitFilter_Dark = TColor($505050);         // RGB(80, 80, 80)
  clSplit_Dark = TColor($404040);               // RGB(64, 64, 64)
  clSplitHighlight_Dark = TColor($505050);      // RGB(80, 80, 80)
  clTagSuffix_Dark = TColor($303030);           // RGB(48, 48, 48)
  clDuplicateHighlight_Dark = TColor($008C8C);  // RGB(140, 140, 0)
  clGridLineColor_Dark = TColor($8C8C8C);       // RGB(140, 140, 140)

implementation

end.
