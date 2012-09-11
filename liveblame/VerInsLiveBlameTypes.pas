{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is VerInsLiveBlameTypes.pas.                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 Uwe Schuster.          }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsLiveBlameTypes;

interface

uses
  Classes, Graphics;

type
  TJVCSLineHistoryRevision = class(TPersistent)
  private
    FDate: TDateTime;
    FDateStr: string;
    FRevisionStr: string;
    FOrgUserStr: string;
    FUserStr: string;
    FComment: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Date: TDateTime read FDate write FDate;
    property DateStr: string read FDateStr write FDateStr;
    property OrgUserStr: string read FOrgUserStr write FOrgUserStr;
    property RevisionStr: string read FRevisionStr write FRevisionStr;
    property UserStr: string read FUserStr write FUserStr;
    property Comment: string read FComment write FComment;
  end;

  TRevisionColor = class(TPersistent)
  private
    FDateColor: TColor;
    FRevisionColor: TColor;
    FLineHistoryRevision: TJVCSLineHistoryRevision;
    FUserColor: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
    property DateColor: TColor read FDateColor write FDateColor;
    property RevisionColor: TColor read FRevisionColor write FRevisionColor;
    property LineHistoryRevision: TJVCSLineHistoryRevision read FLineHistoryRevision;
    property UserColor: TColor read FUserColor write FUserColor;
  end;

implementation

{ TJVCSLineHistoryRevision }

procedure TJVCSLineHistoryRevision.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryRevision then
  begin
    TJVCSLineHistoryRevision(Dest).FDate := FDate;
    TJVCSLineHistoryRevision(Dest).FDateStr := FDateStr;
    TJVCSLineHistoryRevision(Dest).FRevisionStr := FRevisionStr;
    TJVCSLineHistoryRevision(Dest).FOrgUserStr := FOrgUserStr;
    TJVCSLineHistoryRevision(Dest).FUserStr := FUserStr;
    TJVCSLineHistoryRevision(Dest).FComment := FComment;
  end
  else
    inherited AssignTo(Dest);
end;

{ TRevisionColor }

constructor TRevisionColor.Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
begin
  inherited Create;
  FDateColor := clNone;
  FRevisionColor := clNone;
  FLineHistoryRevision := ALineHistoryRevision;
  FUserColor := clNone;
end;

procedure TRevisionColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TRevisionColor then
  begin
    TRevisionColor(Dest).FDateColor := FDateColor;
    TRevisionColor(Dest).FRevisionColor := FRevisionColor;
    TRevisionColor(Dest).FLineHistoryRevision := FLineHistoryRevision;
    TRevisionColor(Dest).FUserColor := FUserColor;
  end
  else
    inherited AssignTo(Dest);
end;

end.
