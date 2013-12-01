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
{ The Original Code is delphisvn: Subversion plugin for CodeGear Delphi.       }
{                                                                              }
{ The Initial Developer of the Original Code is Ondrej Kelle.                  }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights      }
{ reserved.                                                                    }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Ondrej Kelle (tondrej)                                                       }
{ Uwe Schuster (uschuster)                                                     }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains resource strings used by svn package.                     }
{                                                                              }
{******************************************************************************}

unit svnconst;

interface

resourcestring
  SNodeKindNone = 'None';
  SNodeKindFile = 'File';
  SNodeKindDir = 'Directory';
  SNodeKindUnknown = 'Unknown';
  SNodeKindSymlink = 'Symlink';
  SAuthor = 'Author: ';
  STime = 'Time: ';
  SComment = 'Comment: ';

  SWcStatusNone = '';
  SWcStatusUnversioned = 'Unversioned';
  SWcStatusNormal = 'Normal';
  SWcStatusAdded = 'Added';
  SWcStatusMissing = 'Missing';
  SWcStatusDeleted = 'Deleted';
  SWcStatusReplaced = 'Replaced';
  SWcStatusModified = 'Modified';
  SWcStatusMerged = 'Merged';
  SWcStatusConflicted = 'Conflicted';
  SWcStatusIgnored = 'Ignored';
  SWcStatusObstructed = 'Obstructed';
  SWcStatusExternal = 'External';
  SWcStatusIncomplete = 'Incomplete';

  SWcNotifyAdd = 'Added';
  SWcNotifyCopy = 'Copied';
  SWcNotifyDelete = 'Deleted';
  SWcNotifyRestore = 'Restored';
  SWcNotifyRevert = 'Reverted';
  SWcNotifyFailedRevert = 'Revert Failed';
  SWcNotifyResolved = 'Resolved';
  SWcNotifySkip = 'Skipped';
  SWcNotifyUpdateDelete = 'Deleted';
  SWcNotifyUpdateAdd = 'Added';
  SWcNotifyUpdateUpdate = 'Updated';
  SWcNotifyUpdateCompleted = 'Completed';
  SWcNotifyUpdateExternal = 'External Updated';
  SWcNotifyStatusCompleted = 'Completed';
  SWcNotifyStatusExternal = 'External';
  SWcNotifyCommitModified = 'Modified';
  SWcNotifyCommitAdded = 'Added';
  SWcNotifyCommitDeleted = 'Deleted';
  SWcNotifyCommitReplaced = 'Replaced';
  SWcNotifyCommitPostfixTxdelta = 'File Sent';
  SWcNotifyBlameRevision = 'Blame Revision';
  SWcNotifyLocked = 'Locked';
  SWcNotifyUnlocked = 'Unlocked';
  SWcNotifyFailedLock = 'Lock Failed';
  SWcNotifyFailedUnlock = 'Unlock Failed';
  SWcNotifyExists = 'Exists';
  SWcNotifyChangelistSet = 'Changelist Set';
  SWcNotifyChangelistClear = 'Changelist Clear';
  SWcNotifyChangelistMoved = 'Changelist Moved';
  SWcNotifyMergeBegin = 'Merge Begin';
  SWcNotifyForeignMergeBegin = 'Foreign Merge Begin';
  SWcNotifyUpdateReplace = 'Update Replace';
  SWcNotifyPropertyAdded = 'Property Added';
  SWcNotifyPropertyModified = 'Property Modified';
  SWcNotifyPropertyDeleted = 'Property Deleted';
  SWcNotifyPropertyDeletedNonexistent = 'Nonexistent Property Deleted';
  SWcNotifyRevpropSet = 'Revprop Set';
  SWcNotifyRevpropDeleted = 'Revprop Deleted';
  SWcNotifyMergeCompleted = 'Merge Completed';
  SWcNotifyTreeConflict = 'Tree Conflict';
  SWcNotifyFailedExternal = 'Failed External';
  SWcNotifyUpdateStarted = 'Update Started';
  SWcNotifyUpdateSkipObstruction = 'Update Skip Obstruction';
  SWcNotifyUpdateSkipWorkingOnly = 'Update Skip Working Only';
  SWcNotifyUpdateSkipAccessDenied = 'Update Skip Access Denied';
  SWcNotifyUpdateExternalRemoved = 'Update External Removed';
  SWcNotifyUpdateShadowedAdd = 'Update Shadowed Add';
  SWcNotifyUpdateShadowedUpdate = 'Update Shadowed Update';
  SWcNotifyUpdateShadowedDelete = 'Update Shadowed Delete';
  SWcNotifyMergeRecordInfo = 'Merge Record Info';
  SWcNotifyUpgradedPath = 'Upgraded Path';
  SWcNotifyMergeRecordInfoBegin = 'Merge Record Info Begin';
  SWcNotifyMergeElideInfo = 'Merge Elide Info';
  SWcNotifyPatch = 'Patch';
  SWcNotifyPatchAppliedHunk = 'Patch Applied Hunk';
  SWcNotifyPatchRejectedHunk = 'Patch Rejected Hunk';
  SWcNotifyPatchHunkAlreadyApplied = 'Patch Hunk Already Applied';
  SWcNotifyCommitCopied = 'Commit Copied';
  SWcNotifyCommitCopiedReplaced = 'Commit Copied Replaced';
  SWcNotifyUrlRedirect = 'Url Redirect';
  SWcNotifyPathNonexistent = 'Path Nonexistent';
  SWcNotifyExclude = 'Exclude';
  SWcNotifyFailedConflict = 'Failed Conflict';
  SWcNotifyFailedMissing = 'Failed Missing';
  SWcNotifyFailedOutOfDate = 'Failed Out Of Date';
  SWcNotifyFailedNoParent = 'Failed No Parent';
  SWcNotifyFailedLocked = 'Failed Locked';
  SWcNotifyFailedForbiddenByServer = 'Failed Forbidden By Server';
  SWcNotifySkipConflicted = 'Skip Conflicted';
  SWcNotifyUpdateBrokenLock = 'Update Broken Lock';
  SWcNotifyFailedObstruction = 'Failed Obstruction';
  SWcNotifyConflictResolverStarting = 'Conflict Resolver Starting';
  SWcNotifyConflictResolverDone = 'Conflict Resolver Done';
  SWcNotifyLeftLocalModifications = 'Left Local Modifications';
  SWcNotifyForeignCopyBegin = 'Foreign Copy Begin';
  SWcNotifyMoveBroken = 'Move Broken';

  sWcNotifyStateInapplicable = 'Inapplicable';
  sWcNotifyStateUnknown = 'Unknown';
  sWcNotifyStateUnchanged = 'Unchanged';
  sWcNotifyStateMissing = 'Missing';
  sWcNotifyStateObstructed = 'Obstructed';
  sWcNotifyStateChanged = 'Changed';
  sWcNotifyStateMerged = 'Merged';
  sWcNotifyStateConflicted = 'Conflicted';
  sWcNotifyStateSourceMissing = 'Source Missing';


  sCancelledByUser = 'Cancelled by user';

implementation

end.
